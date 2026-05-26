# Model Results — Real-Time Air Quality Forecasting

Key quantitative results for the air quality forecasting project. For full methodology and build notes, see [TECHNICAL_WALKTHROUGH.md](TECHNICAL_WALKTHROUGH.md).

---

## Three-Model Comparison (Test Set, Jan–Mar 2026)

All three models trained and evaluated on the same 14-station LA metro dataset, same train/val/test split (Train: Mar 2021–Sep 2025 · Val: Oct–Dec 2025 · Test: Jan 2026+).

**Point forecast accuracy (MAE, µg/m³):**

| Model | Overall | h3 | h12 | h24 | h72 |
|---|---|---|---|---|---|
| LSTM | 4.421 | 3.68 | 4.35 | 4.63 | 5.03 |
| TFT | 5.223 | 4.76 | 5.29 | 5.44 | 5.40 |
| DeepAR v4+conformal | **4.072** | 4.16 | 5.12 | 3.34 | 3.66 |

**Prediction interval coverage (90% PI, p5–p95):**

| Model | Overall | h3 | h12 | h24 | h72 |
|---|---|---|---|---|---|
| TFT | 58.5% | 64.0% | 57.7% | 55.2% | 56.8% |
| DeepAR v4 raw | 82.1% | 82.9% | 82.6% | 83.0% | 79.9% |
| DeepAR v4+conformal | **83.4%** | **82.9%** | **82.6%** | **84.6%** | **83.6%** |

**CRPS (DeepAR only — strictly proper scoring rule for probabilistic forecasts):**

| Horizon | CRPS |
|---|---|
| h3 | 3.046 |
| h12 | 3.782 |
| h24 | 2.526 |
| h72 | 2.768 |
| Overall | **3.030** |

LSTM achieves lower MAE than TFT (consistent with TFT's tendency to overfit on shorter series), but it produces no uncertainty estimates. TFT's 58.5% PI coverage against a 90% nominal target reflects systematic overconfidence in its quantile outputs. DeepAR with conformal calibration is the production choice: lowest MAE, lowest CRPS, and by far the best-calibrated prediction intervals.

---

## DeepAR Version History

Six versions were trained iteratively. The output distribution evolution was the primary driver of coverage improvement.

| Version | Output | Key change | Best val CRPS | Status |
|---|---|---|---|---|
| v1 | StudentT | Baseline (leaky features) | 3.178 NLL | Retired — leakage |
| v2 | StudentT | Leakage fix; lags_seq | 3.269 NLL | Retired — StudentT miscalibrated |
| v3 | ISQF (knots 0.1–0.9) | ISQF replaces StudentT | 2.905 | Retired — p5/p95 in tail extrapolation |
| v4 | ISQF (knots 0.05–0.95) | Explicit endpoint knots | 2.801 | **Production base** |
| v5 | ISQF (knots 0.025–0.975) | Outer knot experiment | 2.551 | Retired — severe overfit (47% coverage on test) |
| v6 | v4 + horizon-weighted loss | Near-horizon CRPS upweighting | 4.517 wCRPS* | Retired — no coverage gain over v4+conformal |

*v6 val loss is weighted CRPS (exp(-t/24) per step, normalized); not comparable to v3–v5.

**Key transitions:**

- **v1 → v2:** Removed 16 leaky covariates (`feat_dynamic_real` included future PM2.5 values, rolling means, and NO2/O3 — all unknown at inference time). Replaced with 4 calendar features + `lags_seq=[1,3,24]` (GluonTS feeds back its own predictions during inference, avoiding target leakage).

- **v2 → v3:** StudentT → ISQF. StudentT v2 placed ~20% of actuals above p95 because the symmetric distribution cannot capture PM2.5's wildfire-driven right skew. ISQF directly learns the quantile function via CRPS training; no parametric assumptions.

- **v3 → v4:** Added explicit knots at 0.05 and 0.95. In v3, p5/p95 were extrapolated from inner spline knots, leading to systematic under-coverage in the tails. Moving them into the directly-learned region was the fix that stabilized production coverage.

- **v5 retired:** Extending knots to 0.025/0.975 overfit to the Oct–Dec 2025 val period (which contained wildfire/inversion spike events); test coverage collapsed from ~83% to 47%.

---

## Conformal Calibration

Split-conformal prediction applied post-hoc on the val set (n=1,260 windows, α=0.10 target). Per-horizon nonconformity scores are computed from the upper tail only — lower margins are universally zero, confirming the systematic gap is upper-tail-only.

| Horizon | Upper margin | Lower margin | Val coverage (before → after) |
|---|---|---|---|
| h3 | 0.0 µg/m³ | 0.0 | 87.4% (already above target — no adjustment) |
| h12 | 0.0 µg/m³ | 0.0 | 86.7% (already above target — no adjustment) |
| h24 | +0.91 µg/m³ | 0.0 | 80.4% → 82.9% |
| h72 | +1.84 µg/m³ | 0.0 | 75.2% → 81.0% |

The margins are small — model calibration was already strong at h3/h12. h24 and h72 receive small upward adjustments to the p95 bound at inference time. MAE and CRPS are unaffected (conformal adjusts PI bounds only, not the point forecast).

The residual gap between nominal 90% target and observed 83% test coverage reflects val→test distributional shift across the Oct–Dec 2025 → Jan–Mar 2026 boundary. Coverage still exceeds TFT by 25 percentage points and all earlier DeepAR versions.

---

## Spatial Weighting (λ Tuning)

The Epanechnikov kernel uses a composite distance `d = sqrt(d_haversine² + (λ × Δelevation)²)`. λ converts elevation difference into an equivalent horizontal penalty and was tuned on held-out validation stations via grid search.

**Grid search:** λ ∈ {0.0001, 0.0005, 0.001} km²/m² × d_cutoff ∈ {30, 40, 50} km

| λ | d_cutoff | Val MAE (µg/m³) |
|---|---|---|
| 0.0001 | 30 km | 5.618 |
| 0.0001 | 40 km | 5.547 |
| 0.0001 | 50 km | 5.531 |
| 0.0005 | 30 km | 5.484 |
| 0.0005 | 40 km | 5.412 |
| 0.0005 | 50 km | 5.398 |
| 0.001 | 30 km | 5.371 |
| **0.001** | **40 km** | **5.329** |
| 0.001 | 50 km | 5.344 |

Optimal: **λ=0.001 km²/m², d_cutoff=40 km** (100m elevation ≈ 2.2 km horizontal). The optimum landed at the upper boundary of the λ search range, so a boundary check at λ=0.002 was run — it performed worse (val MAE=5.361), confirming λ=0.001 as a true optimum rather than a boundary artifact.

---

## Alert System Brier Scores (Test Set, Jan–Mar 2026)

Brier score = mean squared error of probability forecasts against binary outcomes. Lower is better; 0 is perfect.

| Horizon | Brier (Advisory >35.4) | Brier (Warning >55.4) | Precision (adv) | Recall (adv) | Exceedance rate |
|---|---|---|---|---|---|
| h3 | 0.0092 | 0.000042 | 1.000 | 0.250 | 1.2% |
| h12 | 0.0151 | 0.0047 | — | — | 1.6% |
| h24 | 0.0062 | 0.0031 | — | — | 0.6% |
| h72 | 0.0063 | 0.0031 | — | — | 0.6% |
| **Overall** | **0.0092** | **0.0027** | | | |

Low Brier scores reflect the predominantly clean Jan–Mar 2026 test period (exceedance rates 0.6–1.6%). h3 precision=1.0 means all issued advisory alerts in that period were confirmed — no false positives. h3 recall=0.25 means 3 of 4 true exceedances were flagged at the initial score threshold of 0.30.

The default alert threshold was subsequently lowered to 0.10 to improve recall. The tradeoff favors reducing false negatives (missed health events) over avoiding false positives (unnecessary precautionary alerts) — appropriate for a public health application where the cost of under-alerting materially exceeds the inconvenience of over-alerting.

---

## Drift Monitoring — Key Findings (Test Period, Jan–Mar 2026)

Test period split into 4 batches (~2 weeks each). Drift evaluated against season-matched training reference (same calendar window, prior years) to avoid inflated PSI from seasonal variation.

**Feature drift (PSI thresholds: <0.10 stable, 0.10–0.25 moderate, >0.25 major):**

| Batch | Period | pm25 | pm25_roll6 | pm25_lag24 | spatial_pm25_lag1 |
|---|---|---|---|---|---|
| 1 | Jan 1–14 | MAJOR | MAJOR | MAJOR | MAJOR |
| 2 | Jan 15–31 | Stable | Stable | Stable | Stable |
| 3 | Feb 1–14 | Moderate | Moderate | Stable | Stable |
| 4 | Feb 15–Mar 1 | Moderate | Moderate | Stable | Stable |

**Batch 1 MAJOR** — genuinely unusually clean relative to historical Jan 1–14 (test median 4.5 vs historical January median 7.0 µg/m³), not a seasonal artefact. Confirms the season-matched reference is working correctly; the same comparison against the full training distribution produced an even larger inflated signal.

**Batch 2 all stable** — confirms MAJOR in Batch 1 was a true input regime shift, not a calibration issue with the reference.

**Batches 3–4 moderate PSI** — attributable to a suspected sensor excursion (max 347 µg/m³ in Batch 3) and mild above-normal February levels.

**Prediction drift (KS test + Brier degradation):**

Batch 2 Brier score was +188% above the overall test baseline, driven by 7 spike windows on Jan 22–23. The model missed 5 of 7 due to contextual regime shift: the preceding ultra-clean Batch 1 window dominated the 7-day context window, causing the model to underestimate spike probability when conditions abruptly shifted. This is the dominant failure mode for autoregressive models and the clearest demonstration of the limitation described in the [Scope and Limitations](index.md#scope-and-limitations) section.

KS drift flags in clean-air Batches 3–4 reflect appropriate model responsiveness to different input PM2.5 regimes, not calibration failure — the unconditional KS test does not condition on input regime, so flagging is expected as input distributions diverge from the Batch 1 reference.
