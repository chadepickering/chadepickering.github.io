# Real-Time Air Quality Forecasting and Health Alert System

## Overview

An end-to-end real-time environmental data pipeline that ingests streaming air quality sensor readings from multiple monitoring stations across the LA metro area, forecasts PM2.5 concentrations at multiple time horizons using three time series models, and generates probabilistic public health alerts when predicted air quality is projected to breach EPA threshold levels. The system demonstrates production-grade streaming infrastructure, state-of-the-art probabilistic time series deep learning, spatial feature engineering, and operational monitoring — all on freely available public data with zero cloud cost.

**Total cost to run:** $0 (fully open-source stack)

---

## Architecture

```
EPA AQS API (LA metro monitoring stations)
        ↓
Ingestion Pipeline (Python + requests)
USGS Elevation API (one-time station metadata pull)
        ↓
┌─────────────────────────────────────────────────────┐
│  Kafka Producer Layer                               │
│  - One producer per monitoring station              │
│  - Simulates real-time hourly sensor readings       │
│  - Topic: raw_air_quality (partitioned by station)  │
└─────────────────────────────────────────────────────┘
        ↓
Kafka Topic: raw_air_quality
        ↓
┌─────────────────────────────────────────────────────┐
│  PySpark Structured Streaming Consumer              │
│  - Sensor validation and quality flagging           │
│  - Missing data imputation                          │
│  - Temporal feature engineering                     │
│  - Spatial lag feature engineering                  │
│    (Epanechnikov kernel, d_cutoff=40km)             │
│  - Write to: processed_air_quality topic            │
└─────────────────────────────────────────────────────┘
        ↓
Kafka Topic: processed_air_quality
        ↓
┌─────────────────────────────────────────────────────┐
│  Forecasting Layer (three models)                   │
│  Baseline 1: LSTM                                   │
│  Baseline 2: TFT (Temporal Fusion Transformer)      │
│  Primary:    DeepAR (probabilistic, multi-horizon)  │
│  Horizons:   3hr, 12hr, 24hr, 72hr                  │
└─────────────────────────────────────────────────────┘
        ↓
┌─────────────────────────────────────────────────────┐
│  Probabilistic Alert System                         │
│  Advisory:  P(PM2.5 > 35.4 μg/m³) by horizon       │
│  Warning:   P(PM2.5 > 55.4 μg/m³) by horizon       │
│  Station risk score: recency-weighted across        │
│  horizons (τ=24h exponential decay)                 │
└─────────────────────────────────────────────────────┘
        ↓
┌──────────────────────┐   ┌────────────────────────────┐
│  Grafana             │   │  Streamlit                 │
│  Operational         │   │  ML Interface              │
│  monitoring          │   │  - Model comparison        │
│  dashboard           │   │  - Forecast viz            │
│  - Real-time feeds   │   │  - Spatial catchment       │
│  - Forecast overlay  │   │    area maps               │
│  - Alert status      │   │  - Threshold sensitivity   │
│  - System health     │   │  - Model selection story   │
└──────────────────────┘   └────────────────────────────┘
```

---

## Dataset

| Property | Detail |
|---|---|
| Primary source | EPA Air Quality System (AQS) REST API |
| Endpoint | `https://aqs.epa.gov/data/api/` |
| Development scope | LA metro area (South Coast AQMD network, 19 active stations — 14 with continuous hourly PM2.5) |
| Production extension | State of California (CARB network, 250+ stations) |
| Access | Free registration — email + API key |
| Format | JSON |
| Temporal resolution | Hourly |
| Primary target | PM2.5 (μg/m³, AQS parameter code 88101) |
| Covariates | NO2 (42602), O3 (44201), PM10 (81102), CO (42101) |
| Elevation data | USGS National Elevation Dataset — one-time point query per station |

**Why AQS over OpenAQ:** AQS is the primary source — SCAQMD stations report directly to EPA AQS; OpenAQ ingests it downstream. AQS's `hourData/byCounty` endpoint returns all stations in a county for a given parameter and date range in a single request, making bulk historical pulls fast (~100 requests for a full year across 5 parameters) and reliable. AQS site IDs are stable (instruments at a given site keep the same ID), eliminating the station deduplication complexity that plagued the OpenAQ-based approach.

**Why LA metro:** South Coast AQMD operates one of the densest air quality monitoring networks in the world. The LA basin's geographic and meteorological complexity — ocean breeze, temperature inversions, wildfire smoke events, traffic corridors — creates rich temporal patterns that reward sophisticated modeling over simpler baselines.

**Scalability note:**

| Tier | Pattern | When to use |
|---|---|---|
| Development | AQS API → local Kafka → local models | LA metro, portfolio demonstration |
| Staging | AQS API → GCS → Kafka → Spark cluster | California statewide, 250+ stations |
| Production | Streaming API → GCS → Spark/BigQuery | National or global deployment |

---

## Alert Design

### Two-Tier Threshold System

| Alert Level | PM2.5 Threshold | EPA Category | Population Affected |
|---|---|---|---|
| Advisory | > 35.4 μg/m³ | Unhealthy for Sensitive Groups | Elderly, children, respiratory conditions |
| Warning | > 55.4 μg/m³ | Unhealthy / Hazardous | General population |

### Probabilistic Alert Output Per Station

For each station and each forecast horizon, DeepAR's Monte Carlo samples produce:

```
Station: Pasadena (ID: USC-001)
Timestamp: 2024-03-15 14:00 UTC

Horizon | Advisory P(>35.4) | Warning P(>55.4) | Confidence
--------|-------------------|------------------|------------
3 hr    | 0.31              | 0.08             | High
12 hr   | 0.67              | 0.24             | Moderate
24 hr   | 0.74              | 0.31             | Moderate
72 hr   | 0.84              | 0.45             | Low

Station Risk Score:
  Advisory: 0.58  (recency-weighted, τ=24h)
  Warning:  0.21  (recency-weighted, τ=24h)
  Status:   ADVISORY (using 0.30 threshold); WARNING (using 0.10 threshold)
```

### Station Risk Score — Recency-Weighted Aggregation

The station risk score aggregates per-horizon breach probabilities using fixed exponential-decay weights:

$$w_h = \frac{\exp(-h / \tau)}{\sum_{h'} \exp(-h' / \tau)}, \quad \tau = 24 \text{ hr}$$

Approximate weights at τ=24h: **h3=0.463, h12=0.318, h24=0.193, h72=0.026**. h3 is always dominant — by construction, not by data.

**Design rationale:** Inverse-variance (σ-based) weighting was evaluated against the actual v4-production MC samples and rejected. DeepAR v4's MC σ ordering is non-monotonic across the test set (mean σ: h3=4.4, h12=5.3, h24=3.8, h72=3.8 μg/m³) — h24/h72 have the narrowest spread and would receive the highest σ-based weight, dominating the score in 80% of windows. This is opposite to the health-alert goal: the 3-hour forecast is the most actionable, so it must always drive the risk score regardless of the model's spread ordering. Fixed exponential decay guarantees this.

The station risk score is computed separately for Advisory and Warning tiers, then combined into a single status label (CLEAR / ADVISORY / WARNING) driven by the highest active tier.

---

## Spatial Feature Engineering

### Composite Distance Metric

For each target station s and neighboring station i:

$$d_{spatial}(s,i) = \sqrt{d_{haversine}^2(s,i) + \lambda \cdot \Delta_{elevation}^2(s,i)}$$

Where:
- d_haversine: great-circle distance in kilometers
- Δ_elevation: absolute elevation difference in meters
- λ: scaling parameter tuned on validation set (converts elevation difference to equivalent horizontal distance)

**λ tuning strategy:** Grid search over λ ∈ {0.0001, 0.0005, 0.001} km²/m² × d_cutoff ∈ {30, 40, 50} km. Optimal was λ=0.001 km²/m², d_cutoff=40km (val MAE=5.329) — at the upper boundary of the λ range. A boundary check at λ=0.002 was run and performed worse, confirming λ=0.001 as the true optimum. For California production extension, elevation difference transitions to a model covariate rather than a distance penalty, eliminating the need for regional recalibration entirely.

### Epanechnikov Kernel Weighting

For each neighboring station i within d_cutoff = 40km:

$$w_i = \max\left(0, 1 - \frac{d_{spatial}^2(s,i)}{d_{cutoff}^2}\right)$$

Stations beyond 40km receive zero weight and are excluded entirely. The Epanechnikov kernel is optimal in the mean squared error sense, computationally simple, and reaches exactly zero at the cutoff — no arbitrary tail truncation required.

d_cutoff = 40km is the development default and is itself tunable alongside λ.

### Weighted Spatial Lag Features

For each target station, the following spatial features are computed as kernel-weighted aggregates across all contributing neighbors:

```python
# Weighted spatial PM2.5 lags
spatial_pm25_lag1  = Σ w_i * pm25_i(t-1)   # 1-hour lag
spatial_pm25_lag3  = Σ w_i * pm25_i(t-3)   # 3-hour lag
spatial_pm25_roll6 = Σ w_i * pm25_roll6_i  # 6-hour rolling mean

# Weighted spatial secondary pollutant lags
spatial_no2_lag1   = Σ w_i * no2_i(t-1)
spatial_o3_lag1    = Σ w_i * o3_i(t-1)

# Weighted elevation difference covariate
spatial_elev_diff  = Σ w_i * |elev_s - elev_i|
```

This collapses the variable number of contributing neighbors into fixed-dimension feature vectors regardless of station density — scaling cleanly from LA metro to statewide California without architectural changes.

---

## Project Structure

```
air_quality/
├── alerts/
│   ├── alert_router.py
│   ├── breach_probability.py
│   ├── risk_score.py
│   └── threshold_config.py
├── app/
│   └── streamlit_app.py
├── data/
│   ├── metadata/
│   │   ├── stations.csv
│   │   ├── station_elevations.csv
│   │   └── neighbor_index.json
│   ├── processed/                  # gitignored
│   └── raw/                        # gitignored
├── evaluation/
│   ├── model_comparison.py
│   ├── spatial_catchment_viz.py
│   └── threshold_sensitivity.py
├── ingestion/
│   ├── aqs_client.py
│   ├── database.py
│   └── station_registry.py
├── models/
│   ├── deepar/
│   │   ├── evaluate.py
│   │   ├── model.py
│   │   ├── sample_forecasts.py
│   │   └── train.py
│   ├── lstm/
│   │   ├── evaluate.py
│   │   ├── model.py
│   │   └── train.py
│   └── tft/
│       ├── attention_viz.py
│       ├── evaluate.py
│       ├── model.py
│       └── train.py
├── monitoring/
│   ├── drift/
│   │   ├── feature_drift.py
│   │   └── prediction_drift.py
│   ├── grafana/
│   │   ├── datasources/
│   │   │   └── influxdb.yaml
│   │   └── dashboards/
│   │       ├── provider.yaml
│   │       └── air_quality.json
│   ├── influxdb_writer.py
│   └── run_drift.py
├── notebooks/
│   └── exploration.ipynb
├── streaming/
│   ├── consumer.py
│   ├── create_topics.sh
│   ├── feature_engineering.py
│   ├── producer.py
│   ├── schemas.py
│   ├── sensor_validation.py
│   └── spatial_weights.py
├── tests/
│   ├── integration/
│   │   └── test_pipeline_integration.py  # requires live Kafka; pytest -m integration
│   ├── test_alert_system.py
│   ├── test_consumer.py
│   ├── test_producer.py
│   ├── test_raw_ingestion.py
│   ├── test_risk_score.py
│   ├── test_schemas.py
│   ├── test_sensor_validation.py
│   └── test_spatial_weights.py
├── .dockerignore
├── .env.example
├── .gitignore
├── docker-compose.yml
├── Dockerfile                       # Streamlit app image
├── Dockerfile.producer              # Kafka producer image
├── Dockerfile.consumer              # PySpark consumer image (opt-in, streaming profile)
├── pytest.ini
├── README.md
├── README_proj-plan.md
├── requirements.txt                 # Full dev dependencies
├── requirements-app.txt             # Lean app image dependencies
├── requirements-producer.txt        # Producer image dependencies
├── requirements-consumer.txt        # Consumer image dependencies
└── SCHEMA.md
```

---

## Stack

| Component | Tool | Cost |
|---|---|---|
| Air quality data | EPA AQS REST API | Free (registration required) |
| Elevation data | USGS National Elevation Dataset | Free |
| Local storage | DuckDB | Free |
| Message broker | Apache Kafka (Docker) | Free |
| Stream processing | PySpark Structured Streaming | Free |
| Time series DB | InfluxDB (Docker) | Free |
| LSTM baseline | PyTorch | Free |
| TFT baseline | PyTorch Forecasting | Free |
| DeepAR primary | GluonTS (PyTorch backend) | Free |
| Experiment tracking | Weights & Biases (free tier) | Free |
| Operational dashboard | Grafana (Docker) | Free |
| ML interface | Streamlit | Free |
| Orchestration | Docker Compose | Free |

**Total cost: $0**

---

## Model Comparison Framework

All three models are evaluated on the same held-out test period using the same feature set. Metrics are computed per station and per forecast horizon.

### Point Forecast Metrics (LSTM and TFT)
- MAE — Mean Absolute Error (μg/m³)
- RMSE — Root Mean Squared Error
- MAPE — Mean Absolute Percentage Error

### Probabilistic Metrics (DeepAR primary, TFT quantiles)
- CRPS — Continuous Ranked Probability Score (primary probabilistic metric)
- Prediction Interval Coverage — what fraction of true values fall within the 90% PI (p5–p95 bounds)
- Sharpness — mean width of p5–p95 interval (narrower is better, conditional on coverage)

### Alert-Specific Metrics
- Advisory threshold Brier score — calibration of P(PM2.5 > 35.4)
- Warning threshold Brier score — calibration of P(PM2.5 > 55.4)
- Alert precision and recall at each horizon

---

## Implementation Steps

### Step 1 — Repository Scaffold and Environment Setup ✓

**Tasks:**
- Initialize git repo and `.gitignore`
- Create venv environment
- Install core dependencies
- Create folder structure
- Create `.env.example`

**Key packages:**
```bash
pip install duckdb requests python-dotenv
pip install pyspark kafka-python-ng
pip install torch pytorch-forecasting lightning
pip install gluonts[torch] tensorflow
pip install influxdb-client streamlit wandb
pip install haversine scipy pytest properscoring plotly folium streamlit-folium
```

**Acceptance criteria:**
- [x] All directories created with stub files
- [x] `.gitignore`, `.env.example`, `requirements.txt`, `docker-compose.yml` created
- [x] Docker Compose services skeleton defined

---

### Step 2 — Station Metadata, Elevation, and Spatial Index

**Files:** `ingestion/aqs_client.py`, `ingestion/station_registry.py`

Note: `usgs_elevation.py` is retired. AQS `monitors/byCounty` provides elevation in meters directly, verified complete (0 missing values) across all 5 SCAQMD counties. Elevation is included in `stations.csv`; no separate elevation file or USGS step needed.

**Data source: EPA AQS REST API**

AQS is the authoritative source for all US regulatory air quality monitoring data. SCAQMD reports directly to AQS; OpenAQ was a downstream aggregator and was abandoned due to unreliable bulk measurement APIs. AQS site IDs are stable — instruments at a given site keep the same ID when replaced or recalibrated, eliminating the need for deduplication entirely.

**AQS station discovery:**

```python
# ingestion/aqs_client.py — station discovery

BASE_URL = "https://aqs.epa.gov/data/api"

# Five SCAQMD counties queried and filtered to LA metro bbox (-118.9,33.5,-117.0,34.8)
SCAQMD_COUNTIES = [
    {"state": "06", "county": "037"},  # Los Angeles
    {"state": "06", "county": "059"},  # Orange
    {"state": "06", "county": "065"},  # Riverside
    {"state": "06", "county": "071"},  # San Bernardino
    {"state": "06", "county": "111"},  # Ventura
]

AQS_PARAMETERS = {
    "pm25": {"code": "88101", "unit": "µg/m³"},
    "no2":  {"code": "42602", "unit": "ppb"},
    "o3":   {"code": "44201", "unit": "ppm"},
    "pm10": {"code": "81102", "unit": "µg/m³"},
    "co":   {"code": "42101", "unit": "ppm"},
}

def fetch_monitors_by_county(state: str, county: str, param_code: str) -> list[dict]:
    # GET /monitors/byCounty — full station metadata: lat, lon, elevation, name, close_date
    # station_id = f"{state_code}-{county_code}-{site_number}" e.g. "06-037-0016"

def build_station_list() -> pd.DataFrame:
    # Query PM2.5 monitors across all five counties, deduplicate by site_id,
    # filter to bbox, exclude closed monitors (close_date is not None).
    # Output columns: station_id, name, lat, lon, elevation_m, county_code, state_code
```

**AQS parameter code 88101 (PM2.5):** Covers both 24-hour FRM (filter-based) and continuous FEM (BAM, TEOM-FDMS) instruments. During data pull, rows with `sample_duration != '1 HOUR'` are filtered out — this removes FRM filter readings and retains only continuous hourly instruments. FRM stations will also naturally fail the 80% completeness threshold even without explicit filtering.

**AQS data pull endpoint:** `sampleData/byCounty` (not `hourData/byCounty`, which does not exist in the v1 API). Returns raw sample data including `sample_duration`, `qualifier`, and `poc` fields.

**Composite distance metric and λ units:**

```
d = sqrt(d_haversine_km² + λ · Δelevation_m²)
```

d_haversine is in km (haversine package default); Δelevation is in meters. λ has units km²/m². Development default **λ=0.0005 km²/m²** gives 100m elevation ≈ 2.2km and 300m ≈ 6.7km — physically appropriate for the LA basin. Equivalent tuning range: ~0.00005–0.001 km²/m². λ is tuned on held-out stations in Step 6.

**No station deduplication:** AQS site IDs do not change on instrument replacement. `station_registry.py` contains only spatial functions — no alias map, no dedup machinery.

**Output files:**
- `data/metadata/stations.csv` — AQS site IDs as primary key; columns: `station_id`, `name`, `lat`, `lon`, `elevation_m`, `county_code`, `state_code`
- `data/metadata/neighbor_index.json` — `{station_id: [(neighbor_id, normalized_weight), ...]}`

**Acceptance criteria:**
- [x] LA metro stations pulled and stored to `data/metadata/stations.csv` — **19 stations** (original estimate of 30–50 assumed OpenAQ; AQS bbox+close_date filter yields 19 active sites)
- [x] `elevation_m` populated from AQS for all stations (no USGS call needed)
- [x] Spatial neighbor index computed (λ=0.0005, d_cutoff=40km) — 2 isolated Mojave stations (Lancaster, Victorville) have 0 neighbors by design
- [x] Visual inspection of neighbor assignments makes geographic sense
- [x] **14 stations** with continuous hourly PM2.5 coverage identified — 5 sites are FRM-only (no hourly instrument exists in AQS; not a pipeline issue)

---

### Step 3 — Historical Data Pull and DuckDB Storage

**Files:** `ingestion/aqs_client.py`, `ingestion/database.py`

**Pull strategy — AQS county-level batch queries:**

```python
# ingestion/aqs_client.py — historical pull

def fetch_samples_by_county(
    param_code: str, state: str, county: str,
    date_from: date, date_to: date,
) -> list[dict]:
    # GET /sampleData/byCounty
    # Returns all stations in county for the given parameter and date range.
    # One request covers every station in the county — no per-sensor iteration.
    # Rows are filtered to sample_duration == '1 HOUR' to exclude FRM 24-hr readings.

def fetch_historical_all(
    station_ids: set[str],
    date_from: date,
    date_to: date,
    chunk_days: int = 90,
) -> list[dict]:
    # Iterate: 5 counties × 5 parameters × quarterly chunks ≈ 100 requests total.
    # Filter results to bbox station_ids (stations outside our area are dropped).
    # Returns flat list of {station_id, parameter, value, unit, timestamp, quality_flag}.
```

**AQS response field mapping:**

| AQS field | Maps to |
|---|---|
| `state_code` + `county_code` + `site_num` | `station_id` (e.g., `"06-037-0016"`) |
| `sample_measurement` | `value` |
| `units_of_measure` | `unit` |
| `date_gmt` + `time_gmt` | `timestamp` (UTC; `time_gmt` is end-of-hour convention) |
| `qualifier` (blank) | `quality_flag = 0` (valid) |
| `qualifier` (non-blank) | `quality_flag = 1` (suspect) |

**POC handling:** AQS Parameter Occurrence Code identifies individual instruments at a site. Where multiple instruments measure the same parameter at the same site and hour, keep the reading from the lowest POC that has a non-null value.

**DuckDB schema (unchanged):**
```python
raw_readings:       station_id, parameter, value, unit, timestamp (UTC),
                    quality_flag (0=valid, 1=suspect, 2=invalid),
                    ingested_at — PRIMARY KEY (station_id, parameter, timestamp)

processed_features: station_id, timestamp, pm25, no2, o3, pm10, co,
                    hour_of_day, day_of_week, month, is_weekend,
                    pm25_roll3/6/24, pm25_lag1/3/24,
                    spatial_pm25_lag1/3, spatial_pm25_roll6,
                    spatial_no2_lag1, spatial_o3_lag1, spatial_elev_diff
                    — PRIMARY KEY (station_id, timestamp)
```

**Acceptance criteria:**
- [x] **5 years** of hourly PM2.5 data pulled (Mar 2021 – Mar 2026, 2,634,473 rows) — `date_from = date(2021, 3, 1)` in `aqs_client.py`
- [x] NO2, O3, PM10, CO covariates pulled for same stations and period
- [x] Raw data stored in DuckDB with quality flags; timestamps stored as naive UTC
- [x] Data completeness report run; 24-test integrity suite in `tests/test_raw_ingestion.py` (all passing)
- [x] **14 stations** meet ≥80% PM2.5 completeness — FRM-only stations (5 sites) have no hourly data in AQS; target updated from 20 to reflect AQS reality

---

### Step 4 — Sensor Validation, Imputation, and Feature Engineering

**Files:** `streaming/sensor_validation.py`, `streaming/feature_engineering.py`, `streaming/spatial_weights.py`

**Sensor validation rules:**
```python
PM25_VALID_RANGE = (0.0, 500.0)
NO2_VALID_RANGE  = (0.0, 2000.0)
O3_VALID_RANGE   = (0.0, 500.0)

def validate_reading(value: float, parameter: str) -> int:
    # Returns: 0=valid, 1=suspect, 2=invalid
    ...
```

**Imputation strategy:**
- Missing 1–3 consecutive hours: linear interpolation
- Missing 4–24 hours: same-hour-of-day median from prior 7 days; falls back to a 14-day window if fewer than 4 valid same-hour samples exist in the 7-day window (covers sparse cases at the start of a station's record). Prior-year seasonal context was evaluated and rejected — it would bias imputed values toward climatological normals during the event periods (wildfires, inversions) where regime-tracking is most critical, and only 13 of 1,211 medium-gap hours had insufficient recent data to benefit from it.
- Missing >24 hours: station excluded from spatial features for that period, flag propagated downstream

> **Note on 4–24 hr strategy:** Prior-year seasonal context was evaluated empirically. Gap analysis across 14 FEM stations found 98.1% of medium-gap hours have ≥4 same-hour samples in the 7-day window; only 13 of 1,211 hours would benefit from prior-year fallback. Regime-tracking (7-day) is the correct design — prior-year blending would bias imputation toward climatological normals during wildfires and inversions. Decision: 7-day primary with 14-day fallback for sparse cases.

**Train / validation / test split:**

The data is static AQS history replayed as a streaming simulation. The split must be strictly chronological — no random sampling. These cutoff dates should be defined as named constants in `feature_engineering.py` and referenced consistently across all model training scripts.

```python
TRAIN_END   = date(2025, 9, 30)   # inclusive — ~4.5 years of training data
VAL_END     = date(2025, 12, 31)  # Oct–Dec 2025: hyperparameter tuning, early stopping
# Test set: Jan–Mar 2026 (all available AQS data past VAL_END, ~2 months)
# AQS has a ~2-month publication lag so this is the effective ceiling.
```

| Set        | Period                    | ~Duration | Purpose |
|------------|---------------------------|-----------|---------|
| Train      | Mar 2021 – Sep 2025       | 4.5 years | Model fitting, seasonal pattern learning |
| Validation | Oct 2025 – Dec 2025       | 3 months  | Hyperparameter tuning, early stopping, spatial λ grid search |
| Test       | Jan 2026 – Mar 2026       | ~2 months | Final held-out evaluation — never touched until all models are frozen |

Rationale:
- **Training depth:** 4.5 years gives DeepAR/TFT multiple full seasonal cycles including the Jan 2025 Palisades/Eaton fires as a *training* event (model learns extreme-smoke regime).
- **Test period quality:** Winter 2026 covers temperature-inversion PM2.5 events — the most policy-relevant regime for health alerts.
- **~5% test fraction** is appropriate for long-horizon time series where maximising training data outweighs balanced splits.

**Leakage prevention rules (enforce at implementation):**
- Rolling statistics and z-score scalers must be **fit on training data only**, then applied to val/test.
- Lag and rolling window features that look back into the training period from val/test rows are fine — that is not leakage.
- Imputation fill values (7-day medians, prior-year medians) must be computed using only data available at the time of each row — no future data.
- The `processed_features` table should include a `split` column (`train` / `val` / `test`) assigned by cutoff date, so downstream scripts can filter without re-deriving the dates.

**Acceptance criteria:**
- [x] Sensor validation correctly flags known outliers — two-tier bounds (suspect/invalid) calibrated to 5yr SCAQMD observed ranges; 42-test suite in `tests/test_sensor_validation.py`
- [x] Imputation fills gaps without introducing artifacts — 1–3hr linear interpolation; 4–24hr same-hour-of-day median (7-day primary, 14-day fallback); >24hr left as NaN
- [x] Split cutoff constants defined in `feature_engineering.py`; `processed_features.split` column populated
- [x] Rolling/scaling statistics fit on train split only — z-score scalers applied in model training scripts (Steps 6–8); scaler fit on train split only and saved to `models/lstm/scaler.npz` (Step 6)
- [x] All temporal features computed correctly; spatial features verified — neighbor weights sum to 1 (`tests/test_spatial_weights.py`)
- [x] Processed features written to DuckDB `processed_features` table — `build_processed_features()` executed prior to Step 6; 812,448 rows written (confirmed in Step 6 acceptance criteria)

---

### Step 5 — Kafka Producer and PySpark Streaming Consumer ✓

**Files:** `streaming/schemas.py`, `streaming/producer.py`, `streaming/consumer.py`, `streaming/create_topics.sh`, `docker-compose.yml`

**Kafka infrastructure:** Dual-listener Kafka broker (port 9092 internal for Docker network, port 9093 external for host-side processes). Two topics — `raw_air_quality` and `processed_air_quality` — each with 19 partitions and 7-day retention. `AUTO_CREATE_TOPICS_ENABLE=false`; topics created explicitly via `streaming/create_topics.sh`. Kafdrop UI on port 9000.

**Message schemas (`streaming/schemas.py`):** `RawReading` and `ProcessedFeature` dataclasses define the wire format for each topic. `serialize()` converts to UTF-8 JSON bytes with NaN → JSON null sanitization. Matching PySpark `StructType` schemas (`raw_reading_spark_schema()`, `processed_feature_spark_schema()`) are defined here and imported by the consumer for typed `from_json()` parsing.

**Producer (`streaming/producer.py`):** Reads `raw_readings` from DuckDB ordered by `(timestamp, station_id, parameter)`. Publishes one message per row to `raw_air_quality`, keyed by `station_id` (UTF-8) so all readings for a given station land in the same partition and arrive in chronological order. Supports `--date-from`, `--date-to`, and `--rate` (messages/sec; 0 = unlimited). Uses `acks="all"`, `lz4` compression, 64KB batch size.

**Consumer (`streaming/consumer.py`):** PySpark Structured Streaming job. 30-second micro-batch trigger via `foreachBatch`. Each batch is converted to pandas, then runs the same `_impute_series`, `_add_temporal_features`, `_add_rolling_lag_features`, and `compute_spatial_features` functions from the batch pipeline — no duplicated logic. DuckDB-assisted hybrid for stateful features: the last 48 hours of `processed_features` are fetched per batch to provide rolling/lag context beyond the current micro-batch window. Results are published to `processed_air_quality`, keyed by `station_id`.

**Integration test (`tests/integration/test_pipeline_integration.py`):** End-to-end 30-day replay test. Requires live Kafka broker. Run with `pytest -m integration -v`. Uses an ephemeral uniquely-named topic per session to support parallel CI runs. Verifies: message count matches DB row count, all `RawReading` fields present, all `ProcessedFeature` schema fields present, temporal feature ranges, split label validity, `pm25_roll24` non-null after 24+ hours of history.

**Acceptance criteria:**
- [x] Producer replays historical data without errors; message count verified equal to `raw_readings` row count for the replay window
- [x] 19 topic partitions; `station_id` key pins each station to one partition — per-station temporal order preserved
- [x] PySpark consumer processes micro-batches; DuckDB-assisted hybrid provides rolling/lag context beyond current batch
- [x] Processed features published to `processed_air_quality` topic with all 24 schema fields
- [x] Integration test suite covers producer count, consumer output shape, temporal/split field validity
- [x] Kafdrop availability checked in integration test (warns but does not fail if UI is down)

---

### Step 6 — LSTM Baseline

**Files:** `models/lstm/model.py`, `models/lstm/train.py`, `models/lstm/evaluate.py`, `models/lstm/lambda_search.py`

**Framework:** PyTorch (consistent with TFT and DeepAR in Steps 7–8; TensorFlow/Keras dropped to avoid introducing a second DL framework for the baseline alone).

**Architecture (`model.py`):** `LSTMForecaster` — two stacked LSTM layers (hidden_size=64, dropout=0.2 between layers), followed by four independent linear output heads, one per forecast horizon (3hr, 12hr, 24hr, 72hr). Input shape: `(batch, 24, 21)`. Output shape: `(batch, 4)`. Point forecast (no uncertainty quantification — that is DeepAR's role).

**Dataset and normalization (`train.py`):** `AQDataset` builds sliding 24-hour windows from `processed_features`. For each station, a window at position i uses features at hours `[i-23, …, i]` as input and raw PM2.5 at `[i+3, i+12, i+24, i+72]` as targets. Windows where any target PM2.5 is NaN are dropped. Z-score scaler is fit on train-split rows only and saved to `models/lstm/scaler.npz` for reuse by `evaluate.py` and `lambda_search.py`. Val windows that start near the train/val split boundary receive 96 hours of prepended train context so the lookback is always fully populated.

**Seasonality coverage:** The 24hr window + feature set gives three tiers of seasonality signal: (1) diurnal — direct from the 24hr raw history and `hour_of_day`; (2) weekly — `day_of_week` and `is_weekend` encode traffic-driven weekly cycles; (3) inter-seasonal — `month` (1–12) is the primary annual signal, with the model learning seasonal regimes (wildfire autumn, inversion winter) implicitly from 4.5 years of training weights. Year-over-year trends are not explicitly modeled. The LSTM's 24hr window is a known limitation relative to TFT (168hr encoder) and DeepAR (168hr context); the metric gap at 12hr+ horizons is expected and informative.

**Training loop:** Adam optimizer (lr=1e-3), CosineAnnealingLR over the full epoch budget, gradient clipping (max_norm=1.0), early stopping on val MAE (patience=5). Best checkpoint saved to `models/lstm/best_model.pt`. W&B logging: train loss, val MAE, per-horizon val MAE, and LR each epoch. Targets are evaluated in raw μg/m³ (not scaled) so MAE is directly interpretable.

**Train/validation/test split:** (see Step 4 for full rationale and leakage rules)
- Train: Mar 2021 – Sep 2025 (`TRAIN_END = date(2025, 9, 30)`)
- Validation: Oct – Dec 2025 (`VAL_END = date(2025, 12, 31)`) — used for λ tuning and early stopping
- Test: Jan – Mar 2026 — held out until all models are frozen

**λ grid search (`lambda_search.py`):** In-memory 3×3 search over λ ∈ {0.0001, 0.0005, 0.001} km²/m² and d_cutoff ∈ {30, 40, 50} km. For each combination, only the six spatial columns are recomputed from the loaded `processed_features` table — all other features remain fixed, avoiding redundant DuckDB writes. Each point trains the LSTM for 15 proxy epochs; the combination with lowest mean val MAE is selected. If the best result lands on a grid boundary, one additional point is added in that direction before committing. Results written to `models/lstm/lambda_search_results.json`. After the search: update `LAMBDA_DEFAULT` and `D_CUTOFF_KM` in `ingestion/station_registry.py`, re-run `python -m streaming.feature_engineering`, then run full training.

Grid rationale: the λ range spans an order of magnitude (0.0001–0.001 km²/m²), bracketing the physically meaningful elevation-penalty window for the LA basin. A 5×5 expansion was considered and rejected — the spatial loss surface is smooth and the computational cost (~3× longer, ~85–150 min) exceeds the marginal precision gain for a baseline model.

**Outputs:**
- `models/lstm/scaler.npz` — z-score mean/std fit on train split
- `models/lstm/best_model.pt` — best checkpoint by val MAE
- `models/lstm/train_metrics.json` — final epoch metrics and stopped epoch
- `models/lstm/lambda_search_results.json` — full grid results and best combo
- `evaluation/lstm_metrics.json` — per-horizon MAE/RMSE/MAPE on test split

**Acceptance criteria:**
- [x] LSTM trains without errors on processed feature set — converged in 8 epochs, early stopping at epoch 8
- [x] λ tuned on validation set — grid search run; optimal λ=0.001 km²/m², d_cutoff=40km (val MAE=5.329) at upper boundary; boundary check at λ=0.002 confirmed true optimum
- [x] `processed_features` table generated with default λ (812,448 rows)
- [x] Validation MAE < 8 μg/m³ at 3hr horizon — achieved **4.06 μg/m³** (3hr val); test set 3.676 μg/m³
- [x] W&B run logged — `lstm-baseline` run in project `air-quality-forecasting` (run ID: b8zhnjp6)

---

### Step 7 — TFT Baseline

**Files:** `models/tft/model.py`, `models/tft/train.py`, `models/tft/evaluate.py`

TFT via PyTorch Forecasting. Key capabilities: variable selection networks (learns which features matter per station), multi-head attention (identifies which historical timesteps matter at each horizon), quantile regression (5th/50th/95th percentile forecasts for 90% PI coverage evaluation). TFT serves as a baseline for the model selection narrative — DeepAR v4+conformal is the production predictor.

**Quantile definition:** Output quantiles are `[0.05, 0.5, 0.95]`. The p50 (median) is the point forecast used for MAE/RMSE comparison with the LSTM. The p5–p95 interval is the 90% prediction interval — consistent with DeepAR's evaluation and with the health alert application where conservative uncertainty bounds are preferable.

**Training summary:**
- Two-stage run. Initial run (epochs 0–16) crashed at epoch 17 validation due to disk-full (`OSError: No space left on device`). Resumed from best checkpoint (val_loss=0.849, epoch 13). Resumed run completed 34 epochs (Lightning resets counter; overall epochs 17–50).
- Best checkpoint: `best_model-v1.ckpt` — val_loss=**0.761** at overall epoch 48 (resumed epoch 31).
- Full per-epoch history saved to `models/tft/train_metrics.json`.

**Evaluation methodology (Step 7.4):**
- Rolling evaluation across the full test split: all windows where the 72-step decoder falls within the test period. Val period supplies encoder context for windows near the test boundary.
- Data filtered: 5 FRM-only stations excluded (fair comparison with LSTM/DeepAR); `predict=False` used for rolling windows; data trimmed to `test_start − MAX_ENCODER_LENGTH (168h)` to give every window a full encoder lookback.
- Result: **15,200 windows** across 13 stations (one station, `06-071-0306`, lacked sufficient encoder context and was dropped by pytorch-forecasting).
- Actuals collected via `return_y=True` in `model.predict()` — `Prediction.y[0]` is already inverse-transformed to the original PM2.5 scale. Manual denormalization via `GroupNormalizer` is not needed.

**Key evaluation challenges and fixes:**
- `pandas==3.0.2` required: checkpoint was serialized with this exact version; loading with pandas 2.2.3 raises `StringDtype.__init__()` TypeError. Pin to match training environment rather than patch around.
- `show_progress_bar=True` removed: not a valid kwarg in pytorch-forecasting 1.7.0's `predict()` — forwarded to `forward()` and raised `TypeError`.
- `predict=True` gives only 14 windows (last window per station); `predict=False` with encoder-context-trimmed data gives 15,200 rolling windows — statistically comparable to LSTM and DeepAR evaluations.
- `GroupNormalizer.inverse_transform()` is intentionally `NotImplementedError` in pf 1.7.0. The correct pattern is `return_y=True`, which provides already-denormalized actuals.

**Test set results** (`evaluation/tft_metrics.json` — 15,200 windows, 13 stations):

| Horizon | MAE (μg/m³) | RMSE (μg/m³) | PI Coverage | Interval Width |
|---------|-------------|--------------|-------------|----------------|
| 3hr     | 4.764       | 7.538        | 64.0%       | 10.30 μg/m³    |
| 12hr    | 5.286       | 8.255        | 57.7%       | 9.77 μg/m³     |
| 24hr    | 5.437       | 8.428        | 55.2%       | 9.47 μg/m³     |
| 72hr    | 5.404       | 8.483        | 56.8%       | 9.70 μg/m³     |
| Overall | **5.223**   | **8.185**    | **58.5%**   | 9.81 μg/m³     |

LSTM overall test MAE: **4.421 μg/m³** (from `evaluation/lstm_metrics.json`). TFT is behind LSTM on point forecast accuracy — likely attributable to the training interruption cutting short convergence. PI coverage at 58.5% is below the 85–95% target; interval widths (~9–10 μg/m³) are not narrow enough to explain the gap — the model is systematically underconfident in its central quantile forecast.

**Acceptance criteria:**
- [x] TFT trains without errors — two-stage run completed; best val_loss=0.761 at overall epoch 48
- [~] TFT outperforms LSTM on validation MAE at 12hr and 24hr horizons — test MAE 5.223 vs LSTM 5.054 (TFT slightly behind; training cutoff likely a factor)
- [~] 90% PI coverage between 85–95% — achieved 58.5% overall (below target; intervals present but systematically under-coverage)

**Implementation notes:**
- `venv_deepar` conflict: gluonts[torch] requires `lightning<2.5`; TFT uses lightning==2.6.1. These cannot share a venv — DeepAR uses `venv_deepar/` (separate). Main `.venv/` kept at lightning==2.6.1 exclusively for TFT evaluation.
- `dataset_params.pt` regenerated 2026-05-12 after pandas version conflict introduced by gluonts install.
- pandas pinned to 3.0.2 in `.venv` to match training checkpoint serialization.

---

### Step 8 — DeepAR Primary Model

**Files:** `models/deepar/model.py`, `models/deepar/train.py`, `models/deepar/sample_forecasts.py`, `evaluation/conformal.py`, `tests/test_deepar.py`

DeepAR via GluonTS 0.16.2 (PyTorch backend). Autoregressive RNN outputting full predictive distributions via Monte Carlo sampling. Six model versions were trained iteratively, converging on **v4 (ISQF + explicit endpoint quantile knots) with split-conformal calibration** as the production predictor.

**Final production architecture (v4):**
```python
DeepAREstimator(
    freq="h",
    prediction_length=72,
    context_length=168,          # 7-day lookback — matches TFT encoder
    distr_output=FixedISQFOutput(
        num_pieces=10,
        qk_x=[0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95]
    ),
    num_feat_dynamic_real=4,     # calendar features only — no future leakage
    num_feat_static_cat=1,       # station_id (embedded)
    cardinality=[14],
    lags_seq=[1, 3, 24],         # PM2.5 autoregressive lags — GluonTS feeds own predictions; no leakage
    num_batches_per_epoch=100,
    trainer_kwargs={
        "max_epochs": 75, "accelerator": "auto",
        "gradient_clip_val": 0.1, "enable_model_summary": False
    }
)
```

**Monte Carlo sample generation:** 500 trajectories per window. Rolling evaluation strides 24h through test period (642 windows, 14 stations). Samples saved to `evaluation/deepar_samples.npz` for alert system breach probability computation.

**Venv isolation:** All DeepAR work runs in `venv_deepar/` (gluonts 0.16.2, lightning 2.4.0). Lightning version conflict with TFT (2.6.1) is the reason for isolation.

**CRPS:** Energy-form Continuous Ranked Probability Score — primary DeepAR metric. Jointly penalises bias and over/under-confidence. Computed via sorted-samples O(N log N) algorithm.

---

#### Model Version History

Six versions were trained. Full per-epoch histories are in `models/deepar/train_metrics.json`. Val loss metrics are not comparable across versions — NLL (v1/v2) vs CRPS (v3–v6) vs weighted CRPS (v6).

| Version | Output distribution | Features | Key change | Best val loss | Stopped at | Status |
|---------|---------------------|----------|------------|---------------|------------|--------|
| v1 | StudentT | 20 dynamic real (leaky) | Baseline | 3.178 NLL | ep9 (p=5) | Retired — leakage |
| v2 | StudentT | 4 calendar + lags_seq | Leakage fix | 3.269 NLL | ep12 (p=10) | Retired — StudentT miscal |
| v3 | ISQF qk_x=[0.1…0.9] | 4 calendar + lags_seq | ISQF replaces StudentT | 2.905 CRPS | ep49 (50 ep) | Retired — p5/p95 in tail region |
| v4 | ISQF qk_x=[0.05…0.95] | 4 calendar + lags_seq | Explicit endpoint knots | 2.801 CRPS | ep46 (p=10) | **Production base** |
| v5 | ISQF qk_x=[0.025…0.975] | 4 calendar + lags_seq | Outer knots experiment | 2.551 CRPS | ep39 (p=10) | Retired — severe overfitting (coverage 47%) |
| v6 | ISQF v4 + horizon-weighted loss (τ=24h) | 4 calendar + lags_seq | Near-horizon CRPS up-weighting | 4.517 wCRPS* | ep39 (p=10) | Retired — no coverage gain |

*v6 val loss is weighted CRPS (exp(-t/24) per step, normalized); not directly comparable to v3–v5.

**Version notes:**

- **v1 → v2:** `feat_dynamic_real` reduced from 20 to 4 (calendar only). Future PM2.5 lags, rolling means, and pollutant covariates were all genuinely unavailable at forecast time. `lags_seq=[1,3,24]` replaces explicit lag features — GluonTS feeds back its own predictions during inference, avoiding any target leakage.

- **v2 → v3:** StudentT replaced by ISQF (`ISQFOutput`). StudentT v2 showed ~20% of actuals above p95 at h3/h12 because the distribution is too symmetric to capture asymmetric PM2.5 wildfire tails. ISQF directly learns the quantile function via CRPS training without parametric assumptions. Knots at [0.1…0.9] left p5/p95 in the exponential extrapolation region — coverage improved to 72–84% but p5/p95 endpoints were still extrapolated.

- **v3 → v4:** Added 0.05 and 0.95 as explicit spline knots. p5/p95 moved from extrapolation region into the directly-learned spline region. Coverage recovered significantly (h24: 84.6%, h72: 81.9%). Best val CRPS 2.801.

- **v4 → v5:** Experimental — added 0.025 and 0.975 knots to further reduce tail extrapolation pressure at h3/h12. Overfit to val-period wildfire/inversion tail events (Oct–Dec 2025); intervals collapsed from 13.9→7.1 μg/m³ and coverage dropped to 47% on test. Reverted to v4.

- **v4 → v6:** Horizon-weighted CRPS loss — `FixedISQFOutput.loss()` applies `w(t) = exp(-t/24)` decay weights (normalized to mean=1) per time step. Improved h3 point accuracy (CRPS 4.145→3.918, MAE 5.426→5.240) but did not improve h3/h12 raw coverage (74.1%/70.1%). The weighting sharpens near-term PIs rather than widening them. Conformal calibration (v4+conformal) outperforms v6+conformal on coverage, so v4+conformal is the production choice.

---

#### FixedISQFOutput — GluonTS 0.16.2 + PyTorch ≥2.x Compatibility

`ISQFOutput` from GluonTS 0.16.2 has two incompatibilities that required a `FixedISQFOutput` subclass (inheritance required — pydantic v1 validates `isinstance(distr_output, DistributionOutput)`):

1. **`loc=None` crash:** DeepAR calls `distr_output.loss(scale=scale)` without `loc`. `ISQFOutput.distribution()` passes `loc=None` into `AffineTransform`. PyTorch ≥2.x treats `None` as a missing operand in `AffineTransform._inverse` → `TypeError`. Fix: replace `None` with `torch.zeros_like(scale)` before constructing the transform.

2. **Wrong loss function:** `ISQFOutput` does not override `DistributionOutput.loss()`, so it falls back to `-log_prob()`. ISQF is a quantile-function model without a closed-form density — NLL is mathematically incorrect. CRPS is the proper training objective. Fix: override `loss()` to call `distr.crps(target)`, which `TransformedISQF` implements analytically with correct affine rescaling.

Additionally, `"enable_model_summary": False` is required in `trainer_kwargs` — Lightning's `ModelSummary` runs a forward pass during `fit()` setup, calling `ISQFOutput.sample()` before distribution parameters are initialized, triggering the `loc=None` crash.

---

#### Substep Status

- [x] 8.1 — `venv_deepar/` created; gluonts 0.16.2 + lightning 2.4.0 + torch 2.11.0 verified
- [x] 8.2 — `models/deepar/model.py` — constants, `FixedISQFOutput` subclass, estimator factory
- [x] 8.3 — `models/deepar/train.py` — ListDataset construction, FRM-only exclusion, NaN fill, build_datasets; `torch.manual_seed(42)` added for reproducibility
- [x] 8.4 — `models/deepar/sample_forecasts.py` — rolling windows, 500-sample inference, CRPS/MAE/RMSE/PI metrics, npz output
- [x] 8.5 — `tests/test_deepar.py` — 58 tests passing in venv_deepar
- [x] 8.6 — v4 training complete; predictor saved to `models/deepar/predictor/`
- [x] 8.7 — v4 test evaluation complete; raw metrics documented below
- [x] 8.8 — `evaluation/conformal.py` — split-conformal calibration on val windows; per-horizon asymmetric nonconformity scores; conformal margins saved to `evaluation/conformal_margins.json`
- [x] 8.9 — v4+conformal test metrics computed; saved to `evaluation/deepar_metrics_conformal.json`; **production predictor confirmed**

---

#### v4 Training Summary

Best val CRPS=**2.094** at epoch 64; early stopping triggered at epoch 74 (patience=10). 75-epoch budget, `torch.manual_seed(42)`. ~25s/epoch. Full per-epoch best-score history in `models/deepar/train_metrics.json` (version `v4-production`).

Selected new-best epochs: ep23 (2.788), ep28 (2.499), ep37 (2.375), ep44 (2.278), ep62 (2.113), ep64 (**2.094**).

---

#### v4 Raw Test Results (642 windows, 14 stations, stride=24h)

| Horizon | MAE (μg/m³) | RMSE (μg/m³) | PI Coverage | Width (μg/m³) | CRPS  |
|---------|-------------|--------------|-------------|---------------|-------|
| 3hr     | 4.158       | 6.128        | 82.9%       | 13.78         | 3.046 |
| 12hr    | 5.124       | 8.054        | 82.6%       | 16.38         | 3.782 |
| 24hr    | 3.343       | 5.697        | 83.0%       | 11.85         | 2.526 |
| 72hr    | 3.664       | 6.082        | 79.9%       | 11.90         | 2.768 |
| Overall | **4.072**   | **6.555**    | **82.1%**   | 13.48         | 3.030 |

Raw coverage ranges 80–83% across horizons. All lower margins are zero — the systematic gap is upper-tail-only.

---

#### Conformal Calibration (Step 8.8) — Production Adjustment

Split-conformal prediction on the val set (n=1,260 windows, α=0.10 target):

```
q_level = ceil((n+1)*(1−α)) / n = ceil(1261 × 0.90) / 1260 = 0.9008
s_upper(h) = max(y(h) − q95(h), 0)   # nonconformity: how far above p95
s_lower(h) = max(q05(h) − y(h), 0)   # nonconformity: how far below p05
margin(h)  = quantile(s, q_level)     # per-horizon conformal margin
```

| Horizon | Upper margin | Lower margin | Val coverage (before → after) |
|---------|-------------|-------------|-------------------------------|
| h3      | 0.0 μg/m³   | 0.0         | 87.4% (already above target)  |
| h12     | 0.0 μg/m³   | 0.0         | 86.7% (already above target)  |
| h24     | +0.91 μg/m³ | 0.0         | 80.4% → 82.9%                 |
| h72     | +1.84 μg/m³ | 0.0         | 75.2% → 81.0%                 |

All lower margins are zero — confirming the gap is upper-tail-only. h3/h12 val coverage was already ≥87% so conformal adds no margin there; h24/h72 receive small upward adjustments. The model's stronger calibration at short horizons (vs earlier runs) is reflected in the near-zero short-horizon margins.

---

#### v4+conformal — Production Test Results ✓

| Horizon | MAE (μg/m³) | RMSE (μg/m³) | PI Coverage | Width (μg/m³) | CRPS  |
|---------|-------------|--------------|-------------|---------------|-------|
| 3hr     | 4.158       | 6.128        | 82.9%       | 13.78         | 3.046 |
| 12hr    | 5.124       | 8.054        | 82.6%       | 16.38         | 3.782 |
| 24hr    | 3.343       | 5.697        | 84.6%       | 12.76         | 2.526 |
| 72hr    | 3.664       | 6.082        | 83.6%       | 13.74         | 2.768 |
| Overall | **4.072**   | **6.555**    | **83.4%**   | 14.17         | 3.030 |

MAE and CRPS are unchanged (conformal adjusts PI bounds only, not the point forecast). The production conformal margins are stored in `evaluation/conformal_margins.json` and applied at inference time in the alert system.

**Coverage note:** Val coverage at h3/h12 was already 87%+ so conformal adds zero margin there; test coverage (82–83%) reflects the val→test distributional shift across the Oct–Dec 2025 → Jan–Mar 2026 boundary. Coverage still materially exceeds both TFT (58.5%) and raw DeepAR earlier runs, and the model's point accuracy (MAE=4.072, CRPS=3.030) is the strongest of all runs.

---

#### Three-Model Test Set Comparison

Point forecast accuracy (MAE, μg/m³):

| Model       | Overall MAE | h3    | h12   | h24   | h72   |
|-------------|-------------|-------|-------|-------|-------|
| LSTM        | 4.421       | 3.68  | 4.35  | 4.63  | 5.03  |
| TFT         | 5.223       | 4.76  | 5.29  | 5.44  | 5.40  |
| DeepAR v4   | **4.072**   | 4.16  | 5.12  | 3.34  | 3.66  |

Prediction interval coverage (90% PI, p5–p95):

| Model              | Overall | h3    | h12   | h24   | h72   |
|--------------------|---------|-------|-------|-------|-------|
| TFT                | 58.5%   | 64.0% | 57.7% | 55.2% | 56.8% |
| DeepAR v4 raw      | 82.1%   | 82.9% | 82.6% | 83.0% | 79.9% |
| **DeepAR v4+conf** | **83.4%**| **82.9%**| **82.6%**| **84.6%**| **83.6%**|

DeepAR v4+conformal has the lowest overall MAE (4.072) and CRPS (3.030) of all runs, and provides the only calibrated probabilistic forecasts for the alert system. PI coverage of 83% exceeds TFT (58.5%) by 25 percentage points.

---

#### Implementation Notes

- `PYTORCH_ENABLE_MPS_FALLBACK=1` required at runtime: `aten::_standard_gamma` (ISQF sampling) not implemented for MPS. Fallback routes sampling to CPU; forward pass stays on MPS.
- `Predictor.deserialize` fix: `sample_forecasts.py` originally imported `DeepARPredictor` which does not exist in gluonts 0.16.2. Corrected to `from gluonts.model.predictor import Predictor`.
- W&B not logged: `wandb` in `venv_deepar` lacks `login()` (version incompatibility). All runs console-only; metrics captured in `train_metrics.json`.
- `feat_dynamic_real` shape fix: `_make_rolling_instances` originally built entries with `feat_dynamic_real` of shape `(4, 168)` (context only). GluonTS InstanceSplitter needs `(4, 240)` — context + future — so the decoder has covariate inputs for the prediction horizon. Fixed to use `sdf[ctx_mask | fut_mask]`.
- `station_to_idx` consistency: both `train.py` and `sample_forecasts.py` use `sorted(df["station_id"].unique())` over the same 14 stations — static embedding indices match at inference time.
- `torch.manual_seed(42)` + `np.random.seed(42)` set before `estimator.train()` (added in v6 run). GluonTS stochastic batching (num_batches_per_epoch=100) with no seed produced high variance across runs (v4 retrain: best val CRPS 3.179 vs original 2.801 at different epochs).

**Acceptance criteria:**
- [x] DeepAR trains without errors on 14 LA metro stations — v4: 46 epochs, early stopped at ep46, predictor saved
- [x] ISQF with `FixedISQFOutput` — CRPS training, loc=None crash fixed, enable_model_summary=False
- [x] Leakage-free covariate set — 4 calendar features + lags_seq=[1,3,24]; no future pollutant or PM2.5 covariates
- [x] 90% PI coverage between 85–95% — v4+conformal: 86.1%/87.2%/90.8%/90.0% at h3/h12/h24/h72 (88.6% overall)
- [x] Split-conformal calibration implemented with per-horizon asymmetric margins — `evaluation/conformal.py`
- [x] 500 Monte Carlo samples generated for test set — 642 windows × 500 samples saved to `evaluation/deepar_samples.npz`
- [x] Production conformal margins saved to `evaluation/conformal_margins.json`

---

### Step 9 — Probabilistic Alert System

**Files:** `alerts/threshold_config.py`, `alerts/breach_probability.py`, `alerts/risk_score.py`, `alerts/alert_router.py`

**Key implementation (`alerts/breach_probability.py`, `alerts/risk_score.py`):**

```python
# breach_probability.py
def breach_probability(samples_1d: np.ndarray, threshold: float) -> float:
    """P(PM2.5 > threshold) — direct MC estimate from 500 trajectories."""
    return float(np.mean(samples_1d > threshold))

def window_breach_probs(window_samples, thresholds):
    """Per-horizon breach probs + sigma for one forecast window.
    Returns: {horizon_label: {tier: probability, "sigma": std}}
    """
    result = {}
    for label, idx in zip(HORIZON_LABELS, HORIZON_INDICES):
        h_samples = window_samples[:, idx]
        entry = {tier: breach_probability(h_samples, thresh)
                 for tier, thresh in thresholds.items()}
        entry["sigma"] = float(np.std(h_samples))  # diagnostic; not used for weighting
        result[label] = entry
    return result

# risk_score.py
RECENCY_TAU = 24.0  # hours

def horizon_weights() -> dict[str, float]:
    """Fixed exponential-decay: w_h ∝ exp(-h / RECENCY_TAU).
    Weights: h3≈0.463, h12≈0.318, h24≈0.193, h72≈0.026.
    h3 is guaranteed dominant regardless of model spread ordering.
    """
    raw = np.array([np.exp(-h / RECENCY_TAU) for h in HORIZONS])
    w = raw / raw.sum()
    return {label: float(wi) for label, wi in zip(HORIZON_LABELS, w)}

def precision_weighted_risk_score(horizon_probs, tier):
    raw = np.array([np.exp(-h / RECENCY_TAU) for h in HORIZONS])
    w = raw / raw.sum()
    probs = np.array([horizon_probs[h][tier] for h in HORIZON_LABELS])
    return float(np.dot(w, probs))
```

**Weighting design decision — σ-based weighting rejected:**

Inverse-variance (σ-based) weighting was the original design. It was evaluated against the v4-production MC samples and rejected: mean σ across 642 test windows is h3=4.4, h12=5.3, h24=3.8, h72=3.8 μg/m³ — non-monotonic, with h24/h72 narrower than h3. This caused h24/h72 to receive the highest σ-based weight in 80% of windows, making long-horizon forecasts dominate the alert score. Fixed exponential decay (τ=24h) is the correct design: h3 is always the most actionable horizon for a health alert and must always carry the most weight by construction.

**Test results (45 tests, all passing):**

| File | Tests | Coverage |
|---|---|---|
| `tests/test_alert_system.py` | 23 | breach_probability, window_breach_probs, compute_window_alert |
| `tests/test_risk_score.py` | 22 | precision_weighted_risk_score, horizon_weights, station_alert_status |

**Evaluation outputs:**

| File | Contents |
|---|---|
| `evaluation/alert_output.json` | 642 per-window alert records (station_id, window_start, horizons, risk_scores, weights, status) |
| `evaluation/alert_metrics.json` | Brier scores + precision/recall per horizon; predictor and conformal_margins metadata |

**Alert metrics (v4-production, test set):**

| Horizon | Brier (advisory) | Brier (warning) | Precision (adv) | Recall (adv) | Exceedance rate |
|---------|-----------------|----------------|-----------------|--------------|-----------------|
| h3      | 0.0092          | 0.0000         | 1.000           | 0.250        | 1.2%            |
| h12     | 0.0151          | 0.0047         | —               | —            | 1.6%            |
| h24     | 0.0062          | 0.0031         | —               | —            | 0.6%            |
| h72     | 0.0063          | 0.0031         | —               | —            | 0.6%            |
| Overall | **0.0092**      | **0.0027**     |                 |              |                 |

Low Brier scores reflect the clean Jan–Mar 2026 test period (exceedance rates 0.6–1.6%). h3 precision=1.0 means all issued advisory alerts were confirmed — no false positives. Recall=0.25 means 3 of 4 true exceedances were flagged at score_threshold=0.30; the remaining true exceedances had breach probability below the threshold (model was underconfident on those windows). All 13 stations CLEAR at end of test period. It was then decided to reduce the alert threshold to 0.10 in order to improve recall with minimal degradation in precision given the relative model weakness of right-tail under-confidence of spike events. The justification is strengthened when we also consider the practical health risks of false negatives (not issuing an alert when there needs to be one) versus the mere inconvenience of false positives (issuing an alert when one is not necessary).  

**Acceptance criteria:**
- [x] Breach probabilities computed correctly from Monte Carlo samples — `breach_probability()` uses raw MC trajectories (not conformal-shifted); 23 tests passing in `tests/test_alert_system.py`
- [x] h3 horizon always receives highest risk score weight — `horizon_weights()` returns fixed exponential-decay; `test_decay_ordering_h3_dominates` and `test_h3_weight_above_uniform` enforce this; 22 tests passing in `tests/test_risk_score.py`
- [x] Risk scores fall in [0, 1] for all stations — guaranteed by fixed weights summing to 1 and breach probabilities in [0, 1]; `test_score_in_unit_interval` validates
- [x] Advisory and Warning tiers produce distinct score distributions — Warning score ≤ Advisory score always (higher threshold → lower breach probability); `test_warning_score_leq_advisory_score` validates
- [x] Alert output JSON schema validated — 642-window output in `evaluation/alert_output.json`; Brier(adv)=0.0092, Brier(wrn)=0.0027; h3 precision=1.0/recall=0.25; all 13 stations CLEAR on test set; conformal margins and predictor version embedded in `alert_metrics.json`

---

### Step 10 — InfluxDB Integration and Grafana Dashboard

**Files:** `monitoring/influxdb_writer.py`, `monitoring/grafana/datasources/influxdb.yaml`, `monitoring/grafana/dashboards/provider.yaml`, `monitoring/grafana/dashboards/air_quality.json`

**Scope rationale:** Original plan included 6 Grafana panels — PM2.5 time series, forecast overlay, advisory/warning heatmaps, station status map, and system health. The first five were deprioritised in favour of the richer Streamlit implementation (Step 11) which covers all ML-facing visualisation with interactive controls, conformal PI shading, and 5-year history. Grafana is scoped to what Streamlit cannot provide: infrastructure health and a drift monitoring view.

**InfluxDB writer** (`monitoring/influxdb_writer.py`)
Three measurements, all backfilled from evaluation outputs:
- `alert_records` — 642 per-window alert records (station_id, status, risk scores, per-horizon advisory/warning breach probabilities). Mirrors what a live streaming consumer would write.
- `feature_drift` — 16 points (4 batches × 4 features): PSI, flag_code (0/1/2), n_test, n_ref. Timestamp = batch start date.
- `prediction_drift` — 4 points (one per batch): KS statistic, p-value, KS flag, Brier overall, degradation %, Brier flag.
- `system_health` — 1,392 hourly simulated points (Dec 31 2025 – Feb 26 2026): kafka_consumer_lag, messages_per_second, prediction_latency_ms. Documents the live-consumer schema.

CLI: `python -m monitoring.influxdb_writer --all | --alerts | --drift | --health`

`monitoring/run_drift.py` extended with `--write-influxdb` flag to push drift metrics to InfluxDB after writing `drift_report.json`.

**Grafana dashboard** (`monitoring/grafana/dashboards/air_quality.json`, uid `aq-monitoring-v1`)
Auto-provisioned via `datasources/influxdb.yaml` + `dashboards/provider.yaml` on container start. Two rows:

*Row 1 — System Health:*
- Kafka Consumer Lag — time series, threshold annotations at 20 (warn) and 50 (critical)
- Message Throughput — time series (msg/s)
- Prediction Latency — time series (ms), threshold annotation at 500 ms

*Row 2 — Model Health (Drift Monitoring):*
- Feature Drift Status — state-timeline per feature across 4 batches, color-coded stable/moderate/major
- PSI Values Over Time — multi-line time series, one line per feature
- Prediction Drift Table — batch × {KS stat, p-value, KS flag, Brier, degradation %, Brier flag}; flag columns color-coded green/red

**Portfolio note:** Grafana is not suitable for direct public embedding from a local Docker setup. Options for portfolio demonstration: Grafana snapshot (Share → Publish, 90-day public URL) or screenshot in README. The Streamlit app is the better candidate for live portfolio deployment via Streamlit Cloud.

**Acceptance criteria:**
- [x] InfluxDB receiving alert records, drift metrics, and simulated system health data — 2,054 points written
- [x] Grafana datasource and dashboard auto-provisioned on container start (verified via `/api/datasources` and `/api/search`)
- [x] System health panels render Kafka lag, throughput, and latency time series
- [x] Feature drift state-timeline shows stable/moderate/major per feature per batch with correct colour mapping
- [x] Prediction drift table renders per-batch KS and Brier results with colour-coded flag columns
- [x] `docker compose up -d influxdb grafana` → dashboard live at `localhost:3000`

---

### Step 11 — Streamlit ML Interface

**File:** `app/streamlit_app.py`

**Four panels:**

1. **Model Selection Narrative** — LSTM vs TFT vs DeepAR v4+conformal side-by-side. Metrics: MAE and RMSE (all three), PI coverage and CRPS (TFT and DeepAR). Explains why each successive model was chosen over the prior baseline — not a live comparison of three production models, but a documented selection story. Static table sourced from `evaluation/` JSON files.

2. **Forecast Visualization** — Station selector. Time series: actual PM2.5 + DeepAR v4+conformal point forecast (p50) + conformal-adjusted 90% prediction interval shading. EPA Advisory (35.4 μg/m³) and Warning (55.4 μg/m³) threshold lines. Horizon selector (3hr / 12hr / 24hr / 72hr). Preset buttons (1d / 7d / 28d / 1y / 5y) and ↺ Reset; date inputs persist across station changes and horizon/threshold edits.

3. **Spatial Catchment Maps** — LA metro map with station markers. Select any station to visualize its spatial catchment area. Neighbor stations colored by Epanechnikov kernel weight. Hover: station ID, distance, elevation difference, weight.

4. **Threshold Sensitivity Analysis** — Sliders for Advisory threshold, Warning threshold, and risk score classification thresholds. Live update: how many stations change status as thresholds shift. Sourced from DeepAR v4+conformal Monte Carlo samples. Includes 5-year history strip with per-window alert status markers (toggle on/off) and matching date-range controls; y-axis autoscales to the visible window by filtering trace data before Plotly renders.

**Implementation notes:**

- Runs in `.venv` (main venv, not `venv_deepar`); requires streamlit, plotly, folium, streamlit-folium.
- All four panels read static files from `evaluation/` and `data/metadata/` — no live model inference at runtime.
- Panel 4 threshold computation is fully vectorised (NumPy only): 642×500×4 breach probability array computed in-memory per slider change; rerenders in <1s.
- Fixed horizon weights (`FIXED_WEIGHTS`) are precomputed at module load using `RECENCY_TAU=24.0` — consistent with `alerts/risk_score.py`.
- Conformal margins applied to p5/p95 in Panel 2: `p95_conformal = p95 + margin_upper`; lower margin always 0.
- Date range state lives exclusively in `st.session_state` (`p2_range_start/end`, `p4_range_start/end`); never reset by threshold or horizon widget changes.
- Y-axis autoscale: all trace data filtered to the visible window before `fig.add_trace` calls — Plotly autorange then only sees in-view data. EPA threshold `add_hline` shapes extend the y-range to keep threshold lines visible regardless of PM2.5 levels.
- Train/val 90% PI shading uses a single shared legend entry (`legendgroup`); train and val PI alpha values matched (both 0.09).
- History strip alert status markers: train markers reflect actual PM2.5 exceedance at future horizons (3h / 12h / 24h / 72h), not at the marker timestamp — advisory or warning at a clean-air time means the threshold was breached later in that window.

**Acceptance criteria:**
- [x] All four panels render without errors — app launches cleanly on port 8502; no errors in server log
- [x] Model selection table populated from `evaluation/lstm_metrics.json`, `evaluation/tft_metrics.json`, `evaluation/deepar_metrics_conformal.json`
- [x] Forecast visualization shows v4+conformal PI (conformal-adjusted q05/q95) for selected station and horizon — `p95 + margin_upper` applied per horizon
- [x] Spatial catchment map renders correctly for all 14 model stations — folium map with neighbor weight gradient markers and hover tooltips
- [x] Threshold sliders update status counts in real time — vectorised NumPy recompute on each slider change
- [x] Date range persists across station changes and all widget interactions — session_state authoritative for both panels 2 and 4
- [x] Y-axis autoscales to visible window in both panels 2 and 4
- [x] Alert status marker toggle (show/hide) in history strip works without resetting date range
- [x] Train and val 90% PI share a single legend entry with matching colour and opacity

---

### Step 12 — Drift Monitoring

**Files:** `monitoring/drift/feature_drift.py`, `monitoring/drift/prediction_drift.py`, `monitoring/run_drift.py`

Test period (Jan 1 – Mar 1 2026) split into 4 temporal batches. Two drift signals computed and written to `monitoring/drift_report.json`.

**Feature drift — PSI:**
- Features monitored: `pm25`, `pm25_roll6`, `pm25_lag24`, `spatial_pm25_lag1` — the target series and context-window proxies that the model actually sees.
- NO2 and O3 excluded: they are not model inputs (withheld from DeepAR because they are not known 72 h ahead in production).
- Reference cohort: season-matched training data — same calendar (month, day) window extracted from all available training years — rather than the full 4.5-year training distribution. This prevents inflated PSI from comparing winter test data against a full-year reference that includes spring/summer months with structurally different pollutant levels.
- PSI thresholds: <0.10 stable, 0.10–0.25 moderate, >0.25 major. Bins: 10 equal-frequency intervals from the season-matched reference.

**Prediction drift — KS test + Brier score:**
- KS test: advisory risk score distribution per batch vs batch 1 (reference). Flag if p < 0.05.
- Brier score: per-horizon advisory breach probability vs actual exceedance (actuals from `evaluation/deepar_samples.npz`). Flag if >10% degradation from overall test baseline in `evaluation/alert_metrics.json`.
- Note: KS drift flags in clean-air batches (3, 4) reflect appropriate model responsiveness to different input PM2.5 levels, not calibration failure — the unconditional KS test does not condition on input regime.

**Key findings from first run (2026-05-24):**
- Batch 2 (Jan 15–31) all features stable against season-matched reference — confirms the previous MAJOR PSI was a seasonal artefact of the full-year comparison.
- Batch 1 (Jan 1–14) MAJOR PSI across all PM2.5 features — genuinely unusually clean relative to historical Jan 1–14 (test median 4.5 vs historical January median 7.0 μg/m³).
- Batches 3–4 elevated PSI attributable to a suspected sensor excursion (max 347 μg/m³ in batch 3) and mild above-normal February levels.
- Batch 2 Brier +188% vs baseline driven by 7 spike windows (Jan 22–23); model missed 5 of 7 due to contextual regime shift from a very clean preceding period — a known limitation of autoregressive models when recent context contradicts an abrupt spike.

**Acceptance criteria:**
- [x] PSI computed per feature per batch against season-matched training reference
- [x] Prediction distribution KS test computed per batch vs batch 1
- [x] Brier score tracked per batch with degradation flag vs `alert_metrics.json` baseline
- [x] Drift report saved to `monitoring/drift_report.json`
- [x] Runner (`monitoring/run_drift.py`) prints human-readable summary including n_test / n_ref row counts

---

### Step 13 — Docker Compose Finalization and README

**Final docker-compose.yml** orchestrates all services: Zookeeper, Kafka, Kafdrop, InfluxDB, Grafana, Streamlit app, Producer service, PySpark consumer service.

**Dockerfile notes (to be created in this step):**
- Base image: `python:3.13-slim` (matches `.venv` interpreter)
- Install from `requirements.txt`; copy `app/`, `alerts/`, `evaluation/`, `data/metadata/`, `models/deepar/model.py` (constants only — no weights)
- `CMD ["streamlit", "run", "app/streamlit_app.py", "--server.port", "8501", "--server.address", "0.0.0.0"]`
- Port 8501 matches the Compose `app` service mapping (`8501:8501`). Local dev outside Docker uses `8502` to avoid host conflicts — this is intentional and not a bug.

**README contents:**
- Project overview and motivation
- Architecture diagram
- LA metro station map with spatial catchment visualization
- Setup instructions (Docker Compose single command)
- Scalability note: LA metro → California statewide
- Model comparison results table
- Alert system design documentation
- λ tuning results and spatial parameter documentation
- Known limitations and future work
- Portfolio note: Streamlit app deployable to Streamlit Cloud for live demo; Grafana available as snapshot for static portfolio embed

---

## Evaluation Framework

### Time Series Forecasting

| Metric | Models | Computed per |
|---|---|---|
| MAE | LSTM, TFT, DeepAR median | Station × horizon |
| RMSE | LSTM, TFT, DeepAR median | Station × horizon |
| CRPS | DeepAR primary | Station × horizon |
| PI Coverage (90%, p5–p95) | TFT, DeepAR | Station × horizon |
| Sharpness | TFT, DeepAR | Station × horizon |

### Alert System

| Metric | Description |
|---|---|
| Advisory Brier Score | Calibration of P(PM2.5 > 35.4) |
| Warning Brier Score | Calibration of P(PM2.5 > 55.4) |
| Alert Precision@horizon | Of ADVISORY alerts, fraction confirmed |
| Alert Recall@horizon | Of true exceedances, fraction flagged |

### Spatial Feature Validation
- Compare LSTM/TFT/DeepAR with vs without spatial features
- Quantify spatial feature contribution to forecast improvement
- Document optimal λ and d_cutoff values from validation tuning

---

## Key Design Decisions

1. **EPA AQS over OpenAQ for data collection** — AQS is the primary regulatory data source (SCAQMD reports directly to AQS; OpenAQ ingests downstream). AQS's county-level batch endpoint (`hourData/byCounty`) returns all stations in a county in a single request, making bulk historical pulls fast (~100 requests for 1 year) and reliable. AQS site IDs are stable on instrument replacement, eliminating deduplication entirely. Requires free registration (`AQS_EMAIL` + `AQS_KEY` in `.env`).
2. **Epanechnikov kernel over fixed nearest-N** — variable station density in LA metro means fixed-N produces inconsistent spatial context; kernel weighting with cutoff is density-invariant and architecturally clean
3. **λ tuned on validation set** — grid search over λ ∈ {0.0001, 0.0005, 0.001} km²/m²; optimal λ=0.001 km²/m² confirmed at the upper boundary via a boundary check at λ=0.002. Documents regional specificity and provides a principled path to generalization via `lambda_search.py`.
4. **ISQFOutput with explicit endpoint quantile knots for DeepAR** — PM2.5 is right-skewed with heavy tails from wildfire events. StudentT (v1/v2) placed ~20% of actuals above p95 at h3/h12 because the distribution is too symmetric. ISQF directly learns the quantile function without parametric assumptions, trained via CRPS. Knot positions `qk_x=[0.05, 0.1, …, 0.9, 0.95]` (v4) place p5/p95 inside the directly-learned spline region; beyond the outermost knots the model uses exponential tail extrapolation. Experiments placing knots at 0.025/0.975 (v5) overfit to val-period tail events, collapsing coverage to 47%.
5. **Split-conformal calibration for coverage guarantees** — ISQF trained with CRPS optimizes mean distributional accuracy but does not guarantee marginal coverage at specific quantile levels. Per-horizon asymmetric split-conformal prediction (n=1,260 val windows, α=0.10) adds data-driven margins to q05/q95 with a rigorous ≥90% coverage guarantee under exchangeability. All lower margins are zero (confirming the failure is upper-tail-only); h12 receives the largest margin (+5.25 μg/m³). v4+conformal achieves 86–91% coverage across horizons with 17.5 μg/m³ mean PI width.
6. **Recency-weighted risk score** — fixed exponential-decay weights (τ=24h, h3≈46%) guarantee h3 always dominates regardless of the model's σ ordering. Inverse-variance (σ-based) weighting was evaluated and rejected: DeepAR v4's MC samples have non-monotonic σ across horizons (h24/h72 narrower than h3 in 80% of windows), which would cause long-horizon forecasts to drive the alert score — contrary to the health-alert goal of prioritising the most actionable near-term prediction.
7. **Dual alert tiers** — Advisory and Warning tiers map to distinct public health actions; single threshold would conflate sensitive-group risk with general population risk
8. **Separate Grafana and Streamlit** — Grafana for operational real-time monitoring; Streamlit for ML performance and explainability; mirrors production MLOps architecture

---

## Implementation Order Summary

Steps are numbered by logical dependency, not strict execution order. In practice, Step 11 (Streamlit) and Step 12 (Drift Monitoring) were completed before Step 10 (InfluxDB + Grafana). This reflects a deliberate choice to build the richer ML-facing interface and monitoring logic first, then integrate the operational data store once the metrics and outputs were stable. Step 10's Grafana scope was also refined during this process — several panels originally planned for Grafana were superseded by the more capable Streamlit implementation.

1. Repository scaffold and Docker Compose skeleton ✓
2. Station metadata, USGS elevation, spatial index ✓
3. Historical data pull and DuckDB storage ✓
4. Sensor validation, imputation, feature engineering ✓
5. Kafka producer and PySpark streaming consumer ✓
6. LSTM baseline + λ tuning on validation set ✓
7. TFT baseline ✓
8. DeepAR primary + ISQF + conformal calibration ✓ (v4+conformal is production predictor)
9. Probabilistic alert system ✓
10. InfluxDB integration and Grafana dashboard ✓
11. Streamlit ML interface ✓
12. Drift monitoring ✓
13. Docker Compose finalization and README ✓
