# Real-Time Air Quality Forecasting and Health Alert System

An end-to-end ML pipeline that ingests EPA monitoring data for the LA metro area, streams it through Kafka and PySpark, forecasts PM2.5 concentrations at four time horizons using three competing models, and generates probabilistic public health alerts when air quality is projected to breach EPA threshold levels based on the best-performing model selected to move forward to production.

**[Live demo](https://airquality-forecast.streamlit.app/)** — model comparison, interactive forecast traces with prediction interval bands, spatial catchment maps, and threshold sensitivity analysis. No setup required.

**[Technical walkthrough](TECHNICAL_WALKTHROUGH.md)** — full step-by-step implementation log with design decisions, version history, acceptance criteria, and build notes for each component.

**[Model results](MODEL_RESULTS.md)** — three-model comparison, DeepAR version history, conformal calibration, alert Brier scores, and drift monitoring findings.

---

## Why PM2.5?

PM2.5 refers to fine particulate matter smaller than 2.5 micrometers — small enough to reach the alveoli in the lungs. It is the pollutant most strongly linked to cardiopulmonary disease, and in the LA basin it spikes from wildfires, traffic inversions, and regional transport events. The EPA sets two action thresholds:

| Level | PM2.5 concentration | Meaning |
|---|---|---|
| Advisory | > 35.4 µg/m³ | Sensitive groups should limit outdoor exposure |
| Warning | > 55.4 µg/m³ | General population should limit outdoor exposure |

Forecasting at horizons of 3 hours to 3 days allows enough lead time for agencies and individuals to take protective action before levels breach these thresholds. Because spikes are infrequent and unevenly distributed in time, a **probabilistic** forecast (producing a distribution over possible futures rather than a single point estimate) is more useful than a point forecast for alert generation.

---

## Scope and Limitations

**This system forecasts PM2.5 based on observed pollutant history. It does not predict wildfire ignition or incorporate weather data, and cannot anticipate the onset of a new smoke event.**

The DeepAR model's inputs are four calendar features (hour of day, day of week, month, is-weekend) and the historical PM2.5 time series itself. There is no integration of:

- **Weather data** — wind speed and direction are the primary determinants of smoke transport and dispersion. Without them, the model cannot distinguish a forecast window where conditions favor smoke buildup from one where marine layer or offshore flow will clear the air.
- **Fire detection** — satellite hotspot data (e.g., NASA FIRMS) or active fire perimeters would allow the model to anticipate smoke before it reaches ground-level sensors. This system has no such input.
- **Fuel/drought conditions** — regional fuel moisture and drought indices are leading indicators of wildfire risk over days-to-weeks timescales.

In practice, this means the model performs well at capturing:
- **Diurnal PM2.5 cycles** driven by traffic patterns and temperature inversions
- **Persistence and regional spread** of smoke events already visible in the sensor network
- **Gradual seasonal trends** in background PM2.5 levels

It will **underperform** for:
- **Onset of new wildfire smoke events** before they reach monitoring stations
- **Sharp spikes after extended clean periods**, where the model's context window is dominated by low readings
- **72-hour outlook during active fire weather**, where wind-driven smoke transport is the dominant signal

A production system covering wildfire-driven PM2.5 risk would integrate NOAA HRRR smoke forecasts, FIRMS active fire data, and NWS red-flag warning status alongside observed readings. That integration is out of scope here; the focus is on the ML pipeline architecture, probabilistic forecasting methodology, and monitoring infrastructure.

---

## Architecture

```
EPA AQS REST API (5 pollutants × 14 hourly FEM stations, 5 SCAQMD counties)
        ↓
Ingestion (Python) + USGS Elevation API (one-time station metadata pull)
        ↓  stored in DuckDB
┌─────────────────────────────────────────────────────────────┐
│  Kafka Producer                                             │
│  Replays historical hourly readings in real time            │
│  Topic: raw_air_quality (partitioned by station_id)         │
└─────────────────────────────────────────────────────────────┘
        ↓
┌─────────────────────────────────────────────────────────────┐
│  PySpark Structured Streaming Consumer                      │
│  Sensor validation → imputation → feature engineering       │
│  Spatial lag features (Epanechnikov kernel, 40km cutoff)    │
│  Topic out: processed_air_quality                           │
└─────────────────────────────────────────────────────────────┘
        ↓
┌─────────────────────────────────────────────────────────────┐
│  Forecasting Layer (three models evaluated)                 │
│  Baseline 1: LSTM        — point forecast, 4 horizons       │
│  Baseline 2: TFT         — quantile forecast, 4 horizons    │
│  Primary:    DeepAR      — probabilistic, 500 trajectories  │
│  Horizons:   3 h · 12 h · 24 h · 72 h                      │
└─────────────────────────────────────────────────────────────┘
        ↓
┌─────────────────────────────────────────────────────────────┐
│  Probabilistic Alert System                                 │
│  P(PM2.5 > 35.4) and P(PM2.5 > 55.4) per station/horizon   │
│  Recency-weighted station risk score (τ=24h exp decay)      │
└─────────────────────────────────────────────────────────────┘
        ↓                            ↓
┌───────────────────────┐  ┌─────────────────────────────────┐
│  InfluxDB + Grafana   │  │  Streamlit                      │
│  Operational health   │  │  Model comparison & explainability│
│  - System metrics     │  │  - 3-model results table        │
│  - Drift flag history │  │  - Forecast traces w/ PI bands  │
│  - Alert time series  │  │  - Spatial catchment maps       │
└───────────────────────┘  │  - Threshold sensitivity        │
                           └─────────────────────────────────┘
```

---

## Data

**Source:** [EPA Air Quality System (AQS)](https://www.epa.gov/aqs) — the US federal reference monitoring network. AQS data is free, publicly available via REST API. Two instrument classes report to AQS:

- **FEM (Federal Equivalent Method)** — continuous, hourly instruments (BAM, TEOM-FDMS). These are the stations used in all modeling and alert generation.
- **FRM (Federal Reference Method)** — 24-hour filter-based gravimetric instruments. These meet regulatory standards but produce one reading per day, not hourly, so they cannot support hourly forecasting.

| | |
|---|---|
| **Geographic coverage** | LA metro: LA, Orange, Riverside, San Bernardino, Ventura counties (SCAQMD) |
| **Stations discovered** | 19 active PM2.5 monitors across 5 counties |
| **Stations used in modeling** | 14 — FEM (continuous hourly); 5 FRM-only sites excluded (no hourly instrument in AQS) |
| **Parameters ingested** | PM2.5, NO2, O3, PM10, CO |
| **Date range** | March 2021 – March 2026 (~5 years hourly) |
| **Train / Val / Test split** | Train: Mar 2021–Sep 2025 · Val: Oct–Dec 2025 · Test: Jan 2026+ |

Elevation data from the USGS National Elevation Dataset is pulled once per station and used to compute the composite spatial distance metric (haversine + elevation difference) used in kernel weighting.

---

## Streaming Pipeline

The raw data lives in DuckDB. The Kafka **producer** replays it at 1× real time — one message per station per hour — simulating a live feed from physical sensors. Each message follows a typed Avro-like schema (`RawReading`) that captures station ID, parameter, timestamp, value, and a quality flag.

The **PySpark Structured Streaming consumer** reads from `raw_air_quality`, applies micro-batch processing, and writes feature-engineered records to `processed_air_quality`. Key stages:

1. **Sensor validation** — range checks, duplicate detection, spike flagging (z-score vs rolling window). Invalid readings are marked and passed to imputation rather than dropped.
2. **Temporal feature engineering** — rolling 6-hour mean, 24-hour lag, hour-of-day, day-of-week, month, is-weekend. Rolling and lag features use only past data (no look-ahead leakage).
3. **Spatial lag features** — each station's PM2.5 is augmented with a kernel-weighted average of its neighbors' readings from the previous hour. This captures regional transport events where a plume reaches downwind stations before upwind sensors register it.

> **On the PySpark consumer in Docker:** PySpark requires a JVM, which adds ~1 GB to the container image. The consumer is defined in `docker-compose.yml` under a `streaming` profile and excluded from the default `docker compose up`. See [Running the Stack](#running-the-stack).

---

## Spatial Weighting

Neighbor influence decays with distance using an **Epanechnikov kernel** with a 40 km cutoff:

```
w(d) = 1 − (d / d_cutoff)²    if d < d_cutoff, else 0
```

where `d` is a composite distance combining haversine distance and elevation difference:

```
d_composite = sqrt(d_haversine² + (λ × Δelevation)²)
```

The scaling parameter `λ` converts elevation difference to an equivalent horizontal distance. λ was tuned on held-out validation stations via a grid search over λ ∈ {0.0001, 0.0005, 0.001} km²/m² × d_cutoff ∈ {30, 40, 50} km. The optimal was λ=0.001 km²/m² (100m elevation ≈ 2.2km horizontal) at d_cutoff=40km, which landed on the upper boundary of the search range. A boundary check at λ=0.002 performed worse, confirming λ=0.001 as the true optimum.

Using a kernel over fixed-N nearest neighbors prevents the inconsistent spatial context that arises in variable-density networks: dense urban clusters in central LA vs. isolated rural monitors would otherwise get very different amounts of neighbor information.

---

## Forecasting Models

Three models were trained and evaluated on the same train/val/test split to support a principled comparison.

### LSTM — Long Short-Term Memory (baseline)
Two-layer LSTM (RNN architecture) with a linear head for each horizon. Produces point forecasts only. Trained jointly on all stations via a station-embedding layer. Fastest to train and interpret; provides a lower-bound reference for the more complex models.

### TFT — Temporal Fusion Transformer (baseline)
Attention-based architecture (via PyTorch Forecasting) that simultaneously handles multiple input types: past observed values, known future covariates, and static station metadata. Produces quantile forecasts. The TFT's interpretability via variable importance weights and temporal attention patterns can be visualized if chosen to move to production.

### DeepAR (primary model)
Autoregressive RNN that models the full conditional distribution of future values rather than fixed quantiles. Configured with an **ISQF (Incremental Spline Quantile Function) output distribution** — a non-parametric quantile spline with explicit knots at p5 through p95 — to capture PM2.5's right-skewed distribution without the symmetry assumptions of Gaussian or Student-t parametric forms. The spline is trained end-to-end via CRPS loss, directly optimizing distributional accuracy across the full forecast horizon.

At inference time, 500 trajectory samples are drawn per forecast window per station. These samples are used directly for:
- Plotting prediction interval bands
- Computing P(PM2.5 > threshold) via empirical CDF
- CRPS evaluation (a strictly proper scoring rule for probabilistic forecasts)

**Conformal calibration** is applied post-hoc to the 24-hour and 72-hour horizons to bring prediction interval coverage up to the target 80% level. This is a distribution-free coverage guarantee that does not require the forecasting model's uncertainty estimates to be perfectly calibrated.

---

## Model Results

### Three-model comparison (overall, test set Jan–Mar 2026)

| Metric | LSTM | TFT | DeepAR+conformal |
|---|---|---|---|
| MAE (µg/m³) | 4.42 | 5.22 | **4.07** |
| RMSE (µg/m³) | 7.06 | 8.18 | **6.55** |
| 90% PI coverage (p5–p95) | — | 58% | **83%** |
| CRPS | — | — | **3.03** |

LSTM's lower MAE than TFT is consistent with TFT's known tendency to overfit on shorter series; LSTM's lack of uncertainty quantification is its primary limitation for the alert use case. TFT's PI coverage of 58% against a 90% nominal target indicates systematic overconfidence in its quantile estimates. DeepAR with conformal calibration achieves 83% coverage of the nominal 90% PI — the remaining gap reflects the val→test distributional shift — and produces the best point accuracy of all three models.

### DeepAR per-horizon breakdown

| Horizon | MAE | RMSE | 90% PI Coverage (p5–p95) | CRPS |
|---|---|---|---|---|
| 3 h | 4.16 | 6.13 | 82.9% | 3.05 |
| 12 h | 5.12 | 8.05 | 82.6% | 3.78 |
| 24 h | 3.34 | 5.70 | 84.6% | 2.53 |
| 72 h | 3.66 | 6.08 | 83.6% | 2.77 |
| **Overall** | **4.07** | **6.55** | **83.4%** | **3.03** |

Coverage is after conformal calibration (h3/h12 margins were already sufficient; +0.91 µg/m³ applied to h24 upper bound, +1.84 µg/m³ to h72). MAE and CRPS are unaffected by conformal adjustment.

The 12-hour horizon is the hardest (highest MAE/CRPS), which is typical: short-range forecasts benefit from autocorrelation persistence, and daily-cycle forecasts benefit from periodicity, but 12 hours lands in neither regime. The 24-hour horizon benefits from strong diurnal periodicity in LA basin PM2.5.

**Known limitation:** DeepAR's fixed context window makes it susceptible to regime-shift underestimation. After an extended clean period (e.g., calm holiday weather), the model's running context window is dominated by low readings, and it underestimates the probability of a sharp spike if conditions shift. This is the dominant failure mode observed in the Jan 2026 test period.

---

## Probabilistic Alert System

For each forecast window, 500 Monte Carlo samples from DeepAR are used to compute:

```
P(advisory) = fraction of samples where PM2.5 > 35.4 µg/m³
P(warning)  = fraction of samples where PM2.5 > 55.4 µg/m³
```

A **recency-weighted station risk score** aggregates these probabilities across the four horizons using fixed exponential-decay weights (τ=24h): h3≈46%, h12≈32%, h24≈19%, h72≈3%. The 3-hour forecast always dominates by construction — not because the model assigns it the narrowest prediction interval, but because it is the most actionable horizon for a public health alert.

Inverse-variance weighting (weighting by 1/σ from the MC samples) was evaluated and rejected: DeepAR's uncertainty estimates are non-monotonic across horizons (h24/h72 are actually narrower than h3 in 80% of windows), which would cause 3-day forecasts to dominate the alert score — the opposite of the desired behavior.

A station is flagged at the advisory or warning level when its aggregated risk score exceeds a threshold (0.1 by default, tunable in the Streamlit app).

**Alert system Brier scores (test set):**

| Horizon | Brier (Advisory) | Brier (Warning) |
|---|---|---|
| 3 h | 0.0092 | 0.000042 |
| 12 h | 0.0151 | 0.0047 |
| 24 h | 0.0062 | 0.0031 |
| 72 h | 0.0063 | 0.0031 |

Brier scores near zero indicate well-calibrated alert probabilities; however the test window is short (~2 months) with few exceedance events, so these scores should be interpreted with caution.

---

## Drift Monitoring

Model inputs can shift over time in ways that degrade forecast quality without triggering obvious errors. The drift module monitors four PM2.5 features across temporal batches:

- `pm25` — raw observed value
- `pm25_roll6` — 6-hour rolling mean
- `pm25_lag24` — 24-hour lag
- `spatial_pm25_lag1` — kernel-weighted neighbor lag

NO2 and O3 are deliberately excluded: they are not model inputs (they cannot be known 72 hours ahead and were withheld from DeepAR at training time), so monitoring them would add noise without providing signal about model input drift.

**Population Stability Index (PSI)** is used as the drift metric:

```
PSI = Σ (P_test − P_ref) × ln(P_test / P_ref)
```

where each distribution is binned into 10 equal-frequency intervals. Thresholds: < 0.10 stable · 0.10–0.25 moderate · > 0.25 major.

**Season-matched reference cohort:** Rather than comparing a winter test batch against the full multi-year training distribution (which would conflate seasonal change with genuine drift), each test batch is compared only against the same calendar window from training years. For example, the January 2026 batch is compared against all January data from 2021–2025.

Drift results are written to InfluxDB and visualized as a state-timeline panel in Grafana.

---

## Running the Stack

### Prerequisites

- Docker Desktop
- Python 3.11+ with a virtualenv
- EPA AQS API credentials (free at https://aqs.epa.gov/data/api/signup)

### Environment setup

```bash
cp .env.example .env
# Edit .env: add AQS_EMAIL, AQS_KEY, and set GRAFANA_PASSWORD
```

### One-time data ingestion (run on host)

```bash
python -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt

python -m ingestion.aqs_client        # fetch historical readings → DuckDB
python -m ingestion.usgs_elevation    # fetch station elevations → stations.csv
```

### Start the demo stack

```bash
docker compose up -d
```

This starts: Zookeeper, Kafka, Kafdrop (Kafka UI), InfluxDB, Grafana, the Streamlit app, and the Kafka producer.

| Service | URL |
|---|---|
| Streamlit app | http://localhost:8501 |
| Grafana dashboard | http://localhost:3000 |
| Kafdrop (Kafka UI) | http://localhost:9000 |
| InfluxDB | http://localhost:8086 |

### Optional: run the PySpark consumer

```bash
docker compose --profile streaming up consumer
```

The consumer is opt-in due to the JVM dependency (~1 GB image). The Streamlit demo uses pre-computed evaluation outputs and does not require the consumer to be running.

### Backfill InfluxDB (model monitoring data)

```bash
python -m monitoring.influxdb_writer    # seeds alert records, drift metrics, system health
```

### Run drift monitoring manually

```bash
python -m monitoring.run_drift                        # prints report
python -m monitoring.run_drift --write-influxdb       # also writes to InfluxDB
```

---

## Project Structure

```
air_quality/
├── alerts/
│   ├── alert_router.py              # CLEAR / ADVISORY / WARNING classification
│   ├── breach_probability.py        # P(PM2.5 > threshold) from Monte Carlo samples
│   ├── risk_score.py                # Recency-weighted station risk score (τ=24h exp decay)
│   └── threshold_config.py          # EPA threshold definitions
├── app/
│   └── streamlit_app.py             # ML interface — comparison, forecast viz, maps
├── data/
│   ├── metadata/                    # committed — station list + USGS elevations
│   ├── processed/                   # gitignored — DuckDB, parquet feature files
│   └── raw/                         # gitignored — AQS API responses
├── evaluation/
│   ├── deepar_metrics.json          # MAE, RMSE, PI coverage, CRPS per horizon
│   ├── lstm_metrics.json
│   ├── tft_metrics.json
│   ├── alert_metrics.json           # Brier scores, threshold exceedance rates
│   ├── alert_output.json            # Per-window advisory/warning probabilities
│   ├── deepar_samples.npz           # 500 Monte Carlo trajectories (test set)
│   ├── conformal_margins.json       # Post-hoc PI calibration offsets by horizon
│   ├── model_comparison.py
│   ├── spatial_catchment_viz.py     # Epanechnikov kernel weight maps per station
│   └── threshold_sensitivity.py     # Risk score under different threshold configs
├── ingestion/
│   ├── aqs_client.py                # EPA AQS REST API wrapper + pagination
│   ├── database.py                  # DuckDB schema and write helpers
│   ├── station_registry.py          # Station metadata, spatial index, kernel weights
│   └── usgs_elevation.py            # One-time USGS elevation pull per station
├── models/
│   ├── deepar/
│   │   ├── model.py                 # DeepAR via GluonTS (ISQF output, quantile spline)
│   │   ├── train.py
│   │   ├── evaluate.py              # CRPS, PI coverage
│   │   └── sample_forecasts.py      # 500-trajectory Monte Carlo generation
│   ├── lstm/
│   │   ├── model.py                 # Two-layer LSTM, point forecast per horizon
│   │   ├── train.py
│   │   └── evaluate.py
│   └── tft/
│       ├── model.py                 # TFT via PyTorch Forecasting
│       ├── train.py
│       ├── evaluate.py
│       └── attention_viz.py         # Variable selection weights + attention patterns
├── monitoring/
│   ├── drift/
│   │   ├── feature_drift.py         # PSI on model-input features vs season-matched ref
│   │   └── prediction_drift.py      # KS test + Brier score tracking across batches
│   ├── grafana/
│   │   ├── datasources/influxdb.yaml
│   │   └── dashboards/
│   │       ├── provider.yaml
│   │       └── air_quality.json     # 7-panel dashboard: system health + model health
│   ├── influxdb_writer.py           # Writes alert records, drift metrics, system health
│   └── run_drift.py                 # CLI: compute and optionally write drift report
├── streaming/
│   ├── producer.py                  # Multi-station Kafka producer (replays DuckDB)
│   ├── consumer.py                  # PySpark Structured Streaming consumer
│   ├── feature_engineering.py       # Temporal and spatial feature computation
│   ├── sensor_validation.py         # Quality flagging and imputation
│   └── spatial_weights.py           # Epanechnikov kernel, composite distance metric
├── tests/
│   ├── test_alert_system.py
│   ├── test_risk_score.py
│   ├── test_sensor_validation.py
│   └── test_spatial_weights.py
├── .dockerignore
├── .env.example
├── docker-compose.yml               # Full multi-service orchestration
├── Dockerfile                       # Streamlit app image
├── Dockerfile.producer              # Kafka producer image
├── Dockerfile.consumer              # PySpark consumer image (opt-in, JVM)
├── requirements.txt                 # Full dev dependencies
├── requirements-app.txt             # Lean app image dependencies
├── requirements-producer.txt        # Producer image dependencies
├── requirements-consumer.txt        # Consumer image dependencies
└── README_proj-plan.md              # Step-by-step build log with implementation notes
```

---

## Stack

| Component | Technology | Notes |
|---|---|---|
| Air quality data | EPA AQS REST API | 14 FEM (continuous hourly) + 5 FRM-only (excluded) |
| Elevation data | USGS National Elevation Dataset | One-time pull per station |
| Local storage | DuckDB | Embedded columnar DB; no server needed |
| Message broker | Apache Kafka (Docker) | Topic-per-parameter, partitioned by station |
| Stream processing | PySpark Structured Streaming | Micro-batch, stateful feature engineering |
| Time series DB | InfluxDB 2.x (Docker) | Flux query language |
| LSTM baseline | TensorFlow / Keras | |
| TFT baseline | PyTorch Forecasting | |
| DeepAR primary | GluonTS (PyTorch backend) | ISQF quantile spline, 500-sample MC inference |
| Operational dashboard | Grafana (Docker) | Auto-provisioned via YAML |
| ML interface | Streamlit | |
| Containerization | Docker Compose | Multi-service with Kafka healthcheck |

---

## Key Design Decisions

**EPA AQS over commercial sensor APIs.** AQS FEM instruments provide continuous hourly data with known, documented uncertainty characteristics. Commercial low-cost sensor networks offer higher spatial density but introduce sensor-to-sensor calibration drift that would need to be modeled separately — adding complexity without improving the core forecasting problem. Note that 5 of the 19 SCAQMD sites are FRM-only (daily gravimetric readings) and are excluded from the model; only the 14 FEM sites contribute hourly data to the pipeline.

**Epanechnikov kernel over fixed nearest-N.** Variable station density in LA metro means fixed-N produces inconsistent spatial context: a station in central LA has many neighbors within 10 km, while a rural Riverside monitor might have its nearest neighbor 30 km away. Kernel weighting with a distance cutoff is density-invariant and generalizes cleanly to different spatial configurations.

**λ tuned on validation set.** A grid search over λ ∈ {0.0001, 0.0005, 0.001} km²/m² confirmed λ=0.001 km²/m² as optimal for the LA basin — the result landed at the upper boundary of the search range, and a boundary check at λ=0.002 confirmed it was a true optimum rather than a boundary artifact. For California production extension, the `lambda_search.py` script supports re-running the search without retraining the full model.

**ISQF output for DeepAR.** PM2.5 distributions are right-skewed with heavy tails from wildfire smoke events. An Incremental Spline Quantile Function with explicit knots at p5–p95 learns the quantile function directly via CRPS loss, without the symmetry constraint of Gaussian or Student-t parametric forms. Earlier versions used Student-t but it placed ~20% of actuals above p95, reflecting the distribution's asymmetry. Placing explicit knots at p5 and p95 (rather than extrapolating from inner quantiles) was the key fix for achieving calibrated coverage.

**Conformal post-hoc calibration.** Rather than retraining DeepAR until coverage is perfect, conformal prediction adds distribution-free offsets to the 24-hour and 72-hour intervals after the fact. This is statistically principled and doesn't require access to the model's internals.

**Recency-weighted risk score.** Fixed exponential-decay weights (τ=24h) guarantee the 3-hour forecast always dominates the station risk score regardless of model uncertainty estimates. Inverse-variance weighting was evaluated and rejected: DeepAR's MC samples have non-monotonic spread across horizons (h24/h72 are narrower than h3 in 80% of test windows), which would cause 3-day forecasts to drive the alert score — contrary to the goal of prioritising the most actionable near-term prediction.

**Separate Grafana and Streamlit.** Grafana for operational real-time monitoring (is the pipeline healthy? are input/output distributions changing over time?); Streamlit for ML performance and explainability (how do the models compare? what do the predictions and their certainty look like, and how do changing thresholds impact performance?). This mirrors production MLOps architecture where operational and ML interfaces serve different audiences with different latency and interactivity requirements.

**Season-matched drift reference.** Comparing a January test batch against the full multi-year training distribution artificially inflates PSI due to seasonal changes in pollutant levels. By comparing only against the same calendar window from training years, the drift signal reflects genuine distribution shift rather than predictable seasonal variation.
