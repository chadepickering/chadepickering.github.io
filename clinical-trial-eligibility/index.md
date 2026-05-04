# Clinical Trial Eligibility Intelligence System

An end-to-end clinical decision support system that combines retrieval-augmented generation, transformer-based NLP, and Bayesian uncertainty quantification to match oncology patients to clinical trials.

**[Live demo](https://clinical-trial-eligibility-demo.streamlit.app/)** — 1,000 sampled oncology trials, full Bayesian scoring and semantic search. Runs in a browser with no setup.

**[Pipeline walkthrough](PIPELINE_WALKTHROUGH.md)** — traces a real trial (NCT00127920) through every stage of the system end to end.

**[Project plan](README_proj-plan.md)** — full implementation plan with design decisions, evaluation results, and step-by-step build log.

---

## What this demonstrates

**Multi-task NLP classification.** A SciBERT model with three independent output heads classifies each criterion sentence along three axes simultaneously: inclusion vs exclusion (B1), objective vs subjective language (B2), and observable vs unobservable from structured patient data (B3). The three-head design shares a BioBERT encoder while learning task-specific representations, reducing parameters compared to three separate models.

**Bayesian uncertainty quantification.** Criteria are not simply counted — each gets a Beta-distributed prior that reflects its epistemic status. Objective criteria that pass or fail deterministically contribute exact signal. Subjective criteria (e.g. "adequate organ function") contribute hedged signal calibrated to linguistic uncertainty. Unobservable criteria (data absent from the patient profile) are grouped under a shared Beta(3,1) prior — optimistic, reflecting a trial-seeking referral population — and widen the credible interval rather than collapsing the posterior. The result is a calibrated P(eligible) with a 95% highest-density interval that tells the clinician not just *what* the model thinks but *why* it is uncertain.

**Privacy-first LLM integration.** Mistral-7B runs locally via Ollama. No patient data leaves the machine. The LLM is deliberately positioned as a second opinion on the Bayesian result — it reads free-text trial eligibility criteria and produces a plain-language verdict with criterion-by-criterion reasoning, covering nuances (platinum-sensitivity windows, combination drug rules) that the structured Bayesian model cannot represent.

**Reproducible deployment.** A two-service Docker Compose stack (`ollama` + `app`) with automatic Mistral model pull on first run, healthcheck-gated startup, and a CPU-only PyTorch build to keep the image size manageable.

---

## Architecture

```
ClinicalTrials.gov REST API (~15,000 oncology trials)
        ↓
Ingestion Pipeline (Python + requests + DuckDB)
        ↓
┌─────────────────────────────────────────────────────┐
│  NLP Layer (HuggingFace + PyTorch)                  │
│  Multi-task SciBERT — three classification heads    │
│  B1: Inclusion vs Exclusion                         │
│  B2: Objective vs Subjective                        │
│  B3: Observable vs Unobservable                     │
└─────────────────────────────────────────────────────┘
        ↓
DuckDB (structured criteria store — labeled criterion objects)
        ↓
┌──────────────────────────┐   ┌────────────────────────────┐
│  Embedding Layer         │   │  Named Entity              │
│  sentence-transformers   │   │  Recognition               │
│  all-MiniLM-L6-v2        │   │  Conditions, drugs,        │
│  Mean-pooled overlapping │   │  lab values, thresholds,   │
│  chunks → ChromaDB       │   │  demographics              │
└──────────────────────────┘   └────────────────────────────┘
        ↓                                   ↓
ChromaDB (vector store)        DuckDB (structured entity store)
        └─────────────────┬─────────────────┘
                          ↓
        Patient query → semantic retrieval → top-10 trials
                          ↓
┌─────────────────────────────────────────────────────┐
│  Bayesian Eligibility Scorer (PyMC)                 │
│  Per-criterion Beta priors → multiplicative model   │
│  P(eligible) posterior with 95% HDI                 │
│  Tier: disqualified / high / moderate / uncertain   │
└─────────────────────────────────────────────────────┘
        ↓ (parallel)
┌─────────────────────────────────────────────────────┐
│  LLM Second Opinion (Mistral-7B via Ollama)         │
│  Reads full trial text + patient description        │
│  Verdict: ELIGIBLE / NOT ELIGIBLE / UNCERTAIN       │
│  Plain-language reasoning over free-text nuance     │
└─────────────────────────────────────────────────────┘
        ↓
Streamlit interface — trial search, Bayesian gauge,
criterion breakdown table, LLM narrative
```

---

## Quick start

### Local (dev)

```bash
git clone https://github.com/chadepickering/clinical_trial_eligibility.git
cd clinical_trial_eligibility
python -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt

# Populate the data store (requires API access — public, no credentials)
python ingest.py
python embed.py

# Start the app (Ollama optional — AI Narrative section degrades gracefully)
streamlit run app/streamlit_app.py
```

### Docker (full stack — Ollama + Streamlit)

```bash
git clone https://github.com/chadepickering/clinical_trial_eligibility.git
cd clinical_trial_eligibility

# Populate data on the host first (bind-mounted into the container)
python ingest.py && python embed.py

docker compose up --build
```

The `ollama` container pulls Mistral-7B automatically on first run (~4 GB, one-time). The app starts immediately and degrades gracefully until the pull completes. See [deploy/README.md](deploy/README.md) for full instructions.

---

## Project structure

```
clinical_trial_eligibility/
├── app/
│   └── streamlit_app.py            # Streamlit interface
├── bayesian/
│   ├── criterion_evaluator.py      # patient vs criterion matching + routing
│   ├── eligibility_model.py        # PyMC Bayesian model
│   └── uncertainty.py              # posterior summarization, HDI, tiers
├── data/
│   ├── demo/                       # committed — 1k-trial demo subset for Streamlit Cloud
│   ├── processed/                  # gitignored — full corpus (DuckDB + ChromaDB)
│   └── raw/                        # gitignored — API JSON responses
├── deploy/
│   └── README.md                   # local, Docker, and cloud deployment notes
├── docker/
│   └── ollama_start.sh             # waits for API, pulls Mistral on first run
├── ingestion/
│   ├── api_client.py               # ClinicalTrials.gov REST API client + pagination
│   ├── database.py                 # DuckDB schema creation and write helpers
│   └── parser.py                   # JSON → flat trial dict parser
├── nlp/
│   ├── criterion_splitter.py       # split criteria blob into sentence objects
│   ├── weak_labeler.py             # regex/heuristic weak supervision labels
│   ├── multitask_classifier.py     # SciBERT multi-task model (B1/B2/B3)
│   ├── ner_extractor.py            # clinical entity extraction
│   ├── trainer.py                  # training loop with W&B logging
│   └── evaluate.py                 # F1 evaluation per subtask
├── rag/
│   ├── embedder.py                 # chunked mean-pool embedding pipeline
│   ├── vector_store.py             # ChromaDB operations
│   ├── retriever.py                # semantic retrieval + cross-encoder reranking
│   ├── generator.py                # Mistral-7B via Ollama HTTP API
│   ├── pipeline.py                 # end-to-end RAG orchestration
│   └── evaluate.py                 # verdict accuracy evaluation runner
├── scripts/
│   ├── batch_eval_harness.py       # 3-stage Bayesian vs Mistral evaluation harness
│   └── build_demo_subset.py        # builds the Streamlit Cloud demo dataset
├── .dockerignore
├── .env.example
├── Dockerfile                      # python:3.13-slim, CPU-only torch
├── docker-compose.yml              # ollama + app, healthcheck-gated startup
├── embed.py                        # embedding pipeline CLI
├── ingest.py                       # ingestion pipeline CLI
├── label.py                        # criterion splitting and weak labeling CLI
├── requirements.txt
├── API_schema_reference.md
├── PIPELINE_WALKTHROUGH.md
├── README_proj-plan.md
└── README.md
```

---

## Key design decisions

**Bayesian over deterministic.** Eligibility is inherently uncertain. A deterministic yes/no answer conceals whether the model is confident or guessing. Credible intervals surface the structural source of uncertainty — subjective language vs missing patient data — which is clinically actionable.

**Grouped UNOBS prior.** Unobservable criteria initially used independent Beta(3,1) priors, causing multiplicative shrinkage: 6 unobservable criteria × 0.75 each = P ≈ 0.18 regardless of other evidence. The fix: a single shared Beta(3,1) representing the whole unobservable block, correctly modeling that unobservable criteria are correlated (they share the same absent data source) rather than independent.

**Local Ollama over cloud LLM.** Patient profiles are sensitive. Mistral-7B runs entirely on-device via Ollama — no data leaves the machine. This is a hard constraint for any real clinical deployment, not just a cost optimization.

**CPU-only PyTorch in Docker.** The default PyTorch wheel pulls CUDA dependencies (~2.5 GB). Specifying `--index-url https://download.pytorch.org/whl/cpu` reduces the image to ~800 MB with no runtime difference on CPU-only hardware.

**DuckDB as the analytical store.** Native JSON/Parquet support, columnar query performance, and identical syntax for local files vs GCS/S3 means the development-to-production path requires only connection configuration changes, not query rewrites.

---

## Known limitations

- **Cancer-type matching is approximate.** Criteria like "histologically confirmed endometrial adenocarcinoma" are routed as unobservable if the patient's cancer type string doesn't match — the system does not do NLI-style entailment between cancer type descriptions.
- **ULN references are unobservable.** Criteria expressed as multiples of the upper limit of normal (e.g. "AST ≤ 2.5 × ULN") cannot be evaluated without knowing the lab's reference range. These are correctly routed to unobservable rather than compared against the absolute value.
- **Concurrent treatment exclusions.** Exclusion criteria for concurrent chemotherapy are treated as unobservable rather than routing to `prior_chemo`, since concurrent ≠ prior.
- **The demo corpus is 1,000 trials.** The live demo uses a stratified sample from the full ~15,000-trial development corpus. Clone the repo and run `docker compose up` for full-corpus access.

---

## Stack

| Component | Technology |
|---|---|
| Data ingestion | Python, requests, DuckDB |
| NLP classification | SciBERT (HuggingFace), PyTorch, multi-task learning |
| NER | Regex + MeSH dictionary |
| Embeddings | sentence-transformers/all-MiniLM-L6-v2, ChromaDB |
| LLM | Mistral-7B-Instruct via Ollama |
| Bayesian inference | PyMC, ArviZ |
| Interface | Streamlit |
| Containerization | Docker, Docker Compose |
| Data store | DuckDB |
| Experiment tracking | Weights & Biases |
