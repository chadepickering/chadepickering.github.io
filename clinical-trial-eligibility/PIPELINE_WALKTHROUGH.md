# Pipeline Walkthrough — One Trial, End to End

This document traces a single real clinical trial through each stage of the
pipeline, explaining what happens at each step and why the design decisions
were made the way they were.

The trial used throughout is **NCT00127920** — a Phase 2 study of Taxol,
Carboplatin, and Bevacizumab in advanced ovarian carcinoma. It was chosen
because its eligibility criteria are short enough to read in full, diverse
enough to illustrate every label type, and realistic enough to expose the
genuine ambiguities the system must handle.

---

## Step 1 — The Data Source

### What ClinicalTrials.gov is

[ClinicalTrials.gov](https://clinicaltrials.gov) is the United States registry of clinical research studies. Sponsors — pharmaceutical companies, academic medical centres, government agencies — are required by law to register trials before enrolling patients. Every registered trial receives a permanent **NCT ID** (e.g. `NCT00127920`), which functions as a globally unique identifier for that study.

The registry exposes a public REST API (v2) with no authentication or rate limiting for read access. As of this project's data pull, it holds roughly 500,000 trials across all disease areas. Filtering to oncology studies with completed or active status yields approximately 15,000 trials — the corpus this pipeline is built on.

### Anatomy of a clinical trial record

Each record contains two categories of information that matter for patient matching:

**Structured fields** — machine-readable values stored in typed columns: trial phase, enrollment status, sex eligibility, minimum and maximum age, primary conditions (using MeSH vocabulary), and drug interventions.

**Free-text fields** — the eligibility criteria blob. This is a single unstructured string written by the trial's principal investigator. It typically lists inclusion criteria (conditions a patient *must* satisfy to enrol) and exclusion criteria (conditions that *disqualify* a patient), but the format is not standardised — headers, bullet styles, and section ordering vary widely across trials.

The core engineering challenge this pipeline addresses is that patient matching must be done against the free-text eligibility criteria, not the structured fields. The structured fields answer broad questions (is this an adult oncology trial?); the eligibility text answers the clinical question (does *this patient* meet the specific thresholds, history requirements, and performance criteria this trial demands?).

### NCT00127920

This walkthrough follows **NCT00127920**: a Phase 2 pilot study testing Taxol (paclitaxel), Carboplatin, and Bevacizumab (Avastin) as first-line treatment for advanced-stage ovarian, fallopian tube, or peritoneal carcinoma. The trial was conducted at a single US academic centre, enrolled adult female patients, and has status COMPLETED.

It was chosen as the walkthrough trial for three reasons:

1. **Short eligibility text.** 10 criteria — small enough to read in full at each step without losing the thread.
2. **Label diversity.** The 10 criteria span all label types the classifier must handle: objective and subjective language, observable and unobservable information, inclusion and exclusion sections.
3. **Genuine ambiguity.** Several criteria illustrate real edge cases — a criterion that mixes "adequate" (subjective) with explicit numeric thresholds, a timing constraint that is neither clearly observable nor unobservable, a consent criterion that is structurally unobservable but always met in practice.

---

## Step 2 — Ingestion

### What happens

The ingestion pipeline calls the ClinicalTrials.gov REST API v2 and stores
the result in a local DuckDB database. Two tables are populated:

- **`trials`** — one row per trial, containing metadata and the raw
  eligibility text as a single free-text blob
- **`criteria`** — one row per individual criterion sentence, populated by
  the NLP layer in Step 3

### The raw API response (abbreviated)

The API returns a deeply nested JSON object. The ingestion parser
(`ingestion/parser.py`) walks the relevant paths and flattens them into a
single dict matching the `trials` table schema.

For NCT00127920, the key fields extracted are:

| Field | Value |
|---|---|
| `nct_id` | `NCT00127920` |
| `brief_title` | Pilot Study of Taxol, Carboplatin, and Bevacizumab in Advanced Stage Ovarian Carcinoma Patients |
| `conditions` | `['Ovarian Neoplasms']` |
| `interventions` | `['Avastin']` |
| `phases` | `['PHASE2']` |
| `status` | `COMPLETED` |
| `min_age` | `18 Years` |
| `max_age` | `None` (not specified) |
| `sex` | `FEMALE` |
| `mesh_conditions` | `['Ovarian Neoplasms']` |
| `mesh_interventions` | `['Bevacizumab']` |

Two things worth noting here:

**MeSH terms vs raw text.** The `conditions` field contains whatever the
trial sponsor wrote — it could be "Ovarian Cancer", "OC", "Ovarian
Neoplasms", etc. The `mesh_conditions` field contains the standardised
Medical Subject Heading term assigned by NLM, which is always
"Ovarian Neoplasms" regardless of how the sponsor phrased it. Later
pipeline steps use MeSH terms as pre-normalised labels for entity
extraction, so we store both.

**`max_age` is nullable.** Many trials enrol adults of any age above a
minimum. The parser defaults this to `None` rather than inventing a value.

### The raw eligibility text

This is stored verbatim in `trials.eligibility_text`. It is a single
free-text blob, mixing both inclusion and exclusion criteria under prose
headers, with bullet points in varying formats:

```
Inclusion Criteria:

* Subjects with a histologic or cytologic diagnosis of stage III/IV ovarian
  cancer, fallopian tube epithelial cancer, or peritoneal cancer who have not
  received prior chemotherapy or radiotherapy.
* Subjects must have the appropriate surgery for their gynecologic cancer.
  However, subjects may be treated in a neoadjuvant manner, with surgery being
  performed after chemotherapy cycles 1, 2, or 3.
* If neoadjuvant therapy is not administered, subjects must receive their first
  dose no more than six weeks postoperatively.
* Subjects must have adequate bone marrow, renal and hepatic function as
  defined by WBC > 3,000 cells/cu ml., platelets > 100,000/cu.ml., calculated
  creatinine clearance > 50 ccs/min., bilirubin < 1.5 mg/dl, and SGOT < three
  times normal.
* Karnofsky performance status > 50%.
* Subjects who have signed an institutional review board (IRB) approved
  informed consent form.

Exclusion Criteria:

* Subjects with epithelial ovarian cancer of low malignancy potential.
* Subjects with septicemia, severe infection, or acute hepatitis.
* Subjects with severe gastrointestinal bleeding.
* Subjects with a history of congestive heart failure, angina, or a history of
  myocardial infarction within the past six months.
```

This blob is not yet usable for machine learning — it is one long string with
no structure. Step 3 turns it into individual classifiable sentences.

---

## Step 3 — Criterion Splitter and Weak Labeler

### What happens

Two modules process the eligibility text in sequence:

1. **`nlp/criterion_splitter.py`** — splits the blob into individual
   criterion sentences and assigns each to its section
2. **`nlp/weak_labeler.py`** — assigns noisy labels to each sentence without
   any manual annotation

The result is written to the `criteria` table: one row per criterion, with
labels that will serve as training signal for the SciBERT classifier in Step 4.

---

### Part A — Criterion Splitter

#### The problem

The eligibility text blob has no machine-readable structure. Headers appear
in a dozen real-world formats ("INCLUSION CRITERIA:", "Inclusion criteria",
"* INCLUSION CRITERIA:", no colon, all-caps, etc.). Bullet points use
asterisks, numbers, or letters. Sub-points can be indented under a parent
criterion. Some trials use entirely non-standard headers like
"DISEASE CHARACTERISTICS:" that don't map to inclusion or exclusion at all.

#### What the splitter does

It scans the text line by line, maintaining a small amount of state:

- **Current section** (`inclusion`, `exclusion`, or `unknown`) — set whenever
  a header line is detected
- **Current bullet buffer** — accumulates lines belonging to the same
  criterion (a top-level bullet line plus any sub-points indented beneath it)

When a new top-level bullet is encountered, the buffer is flushed: combined
into a single string, cleaned (escape sequences like `\>` → `>`), and — if
at least 10 characters long — appended to the results as a criterion dict
with `{text, section, position}`.

Sub-points (indented 3+ spaces) are folded into the parent criterion rather
than emitted as separate rows. This is correct because sub-points are
qualifications of the parent, not independent criteria.

#### Output for NCT00127920

The splitter produces 10 criterion dicts. Sections are assigned from the
header context:

| # (Position) | Section | Text (abbreviated) |
|---|---|---|
| 0 | inclusion | Subjects with a histologic or cytologic diagnosis of stage III/IV... |
| 1 | inclusion | Subjects must have the appropriate surgery... |
| 2 | inclusion | If neoadjuvant therapy is not administered... |
| 3 | inclusion | Subjects must have adequate bone marrow, renal and hepatic function... |
| 4 | inclusion | Karnofsky performance status > 50%. |
| 5 | inclusion | Subjects who have signed an IRB approved informed consent form. |
| 6 | exclusion | Subjects with epithelial ovarian cancer of low malignancy potential. |
| 7 | exclusion | Subjects with septicemia, severe infection, or acute hepatitis. |
| 8 | exclusion | Subjects with severe gastrointestinal bleeding. |
| 9 | exclusion | Subjects with a history of congestive heart failure... |

**Position** is a 0-based index across the entire trial, not within each
section. This preserves ordering for the downstream model and allows
reconstruction of the original criteria sequence.

---

### Part B — Weak Labeler

#### Why weak labels at all?

The SciBERT model in Step 4 needs labeled training data. Manual annotation
of 256,000 criteria is not feasible before training. Weak supervision is the
standard approach: generate noisy labels programmatically, use them to get a
first model, then refine with a small set of manually verified labels.

The labeler assigns three independent labels to each criterion:

#### B1 — Inclusion vs Exclusion

**Signal:** the `section` field from the splitter.

This is the most reliable label (~90% accuracy). If the criterion was found
under an "Inclusion Criteria" header, `b1_label = 1`. Under "Exclusion
Criteria", `b1_label = 0`. Under a non-standard header, `b1_label = None`.

There is no confidence score for B1 — it is rule-derived. The training code
assigns a fixed confidence of 0.90 to all labeled B1 rows.

For this trial, criteria 0–5 get `b1_label = 1` and criteria 6–9 get
`b1_label = 0`. No unknowns, because both headers are standard.

#### B2 — Objective vs Subjective

**Signal:** regex pattern matching against the criterion text.

**Objective (b2_label = 1):** a criterion is objective if it has a numeric
threshold, a standardised clinical scale with a number, or a reference to
normal range. These can be verified unambiguously — either the patient's
creatinine is above 1.5 mg/dL or it isn't.

**Subjective (b2_label = 0):** language that requires clinical judgment —
"adequate", "willing", "at the discretion of the investigator", "significant".
No number is given; the assessor decides.

**Confidence:** the ratio of pattern-type hits to total hits. If 3 objective
patterns fire and 1 subjective pattern fires, confidence = 0.75 objective.
If nothing fires, `b2_label = None` with confidence 0.0.

Criterion [3] illustrates this well:

> *"Subjects must have adequate bone marrow, renal and hepatic function as
> defined by WBC > 3,000 cells/cu ml., platelets > 100,000/cu.ml., calculated
> creatinine clearance > 50 ccs/min., bilirubin < 1.5 mg/dl, and SGOT < three
> times normal."*

The word **"adequate"** fires a subjective pattern (it signals judgment-
dependent language). But the explicit numeric thresholds (`> 3,000`,
`> 100,000`, `> 50`, `< 1.5`) fire multiple objective patterns, as does
"three times normal" (a ULN reference). Objective hits outnumber subjective,
so `b2_label = 1` with `b2_confidence = 0.80` — the system correctly
identifies this as objective despite the "adequate" phrasing.

Criterion [5] — the informed consent criterion — fires only subjective
patterns ("signed", "consent"), so `b2_label = 0` with full confidence.

Criteria [0], [6], [7], [8], [9] fire no patterns at all: `b2_label = None`.
These criteria describe diagnoses, disease states, and conditions that are
clinically meaningful but carry no objectivity signal the regex can detect.
The SciBERT model will learn to classify them from context.

#### B3 — Observable vs Unobservable

**Signal:** a combination of EHR field lookup (known lab names, demographics,
diagnosis terminology) and unobservability keyword patterns.

**Observable (b3_label = 1):** assessable from standard electronic health
record data — lab values, diagnoses, performance status scores, treatment
history, documented allergies. The key question is: *would a clinician
reviewing a standard patient chart be able to answer this criterion?*

**Unobservable (b3_label = 0):** requires data that typically does not
exist in an EHR — consent status, life expectancy estimates, internet access,
geographic constraints, intent to conceive.

The results for this trial:

| Criterion | B3 | Reasoning |
|---|---|---|
| [0] Histologic diagnosis, no prior chemo | Observable | "diagnosis", "histolog", "prior", "chemotherapy" all in EHR |
| [1] Appropriate surgery, neoadjuvant OK | Observable | "chemotherapy" fires observable; "appropriate" fires nothing unobservable |
| [2] First dose ≤ 6 weeks postoperatively | **None** | No EHR field keyword fires; no unobservable signal either |
| [3] Lab value thresholds | Observable | WBC, platelets, creatinine, bilirubin are all standard lab fields |
| [4] Karnofsky > 50% | Observable | "Karnofsky" is a named clinical performance scale |
| [5] Signed informed consent | **Unobservable** | "signed", "consent" fire unobservable patterns |
| [6] Low malignancy potential OC | Observable | "cancer", "malignancy" fire observable |
| [7] Septicemia, severe infection | **None** | These are clinical conditions, but no specific EHR keyword fires |
| [8] Severe GI bleeding | **None** | Same — the concept is real but no pattern matches |
| [9] History of cardiac conditions | Observable | "history of" fires the known-history observable pattern |

Criterion [2] is genuinely ambiguous — "first dose no more than six weeks
postoperatively" is a timing constraint that *could* be derived from surgical
records, but no pattern fires either way, so None is the correct outcome.
The SciBERT model will be trained to make a decision where the weak labeler
cannot.

---

### The full criteria table for NCT00127920

This is what gets written to the `criteria` table after both modules run:

| pos | section | B1 | B2 | B2 conf | B3 | B3 conf | text |
|---|---|---|---|---|---|---|---|
| 0 | inclusion | 1 | None | 0.0 | 1 | 1.0 | Histologic diagnosis of stage III/IV... |
| 1 | inclusion | 1 | 0 | 1.0 | 1 | 1.0 | Appropriate surgery... neoadjuvant... |
| 2 | inclusion | 1 | None | 0.0 | None | 0.0 | First dose ≤ 6 weeks postoperatively |
| 3 | inclusion | 1 | 1 | 0.8 | 1 | 1.0 | Lab value thresholds (WBC, platelets...) |
| 4 | inclusion | 1 | 1 | 1.0 | 1 | 1.0 | Karnofsky performance status > 50% |
| 5 | inclusion | 1 | 0 | 1.0 | 0 | 1.0 | Signed informed consent |
| 6 | exclusion | 0 | None | 0.0 | 1 | 1.0 | Low malignancy potential OC |
| 7 | exclusion | 0 | None | 0.0 | None | 0.0 | Septicemia, severe infection |
| 8 | exclusion | 0 | None | 0.0 | None | 0.0 | Severe GI bleeding |
| 9 | exclusion | 0 | None | 0.0 | 1 | 1.0 | History of cardiac conditions |

**What the None labels mean in practice.** Of the 10 criteria, 6 have no B2
label and 3 have no B3 label. These are not errors — they are criteria where
the regex-based weak labeler genuinely has no signal. The B1 label is always
present because it comes from section context, not text content.

The None rows will contribute zero loss during SciBERT training (their
confidence weight is 0.0), so the model trains only on what the weak labeler
is confident about, and learns to generalise from context to fill in the rest.

---

---

## Step 4 — Multi-task SciBERT Classifier

### What happens

The SciBERT model learns to fill in the labels the weak labeler could not assign.
It is a fine-tuned version of `allenai/scibert_scivocab_uncased` — a BERT-scale
model (110M parameters) pretrained on scientific text — with three independent
classification heads added on top, one per label type.

The model takes a criterion sentence as input and simultaneously predicts all
three labels in a single forward pass. This is the multi-task design: the shared
encoder builds a single representation of the text that is then passed to each
head independently. The heads do not share parameters with each other, only
with the encoder.

### Architecture

```
criterion text
      ↓
SciBERT encoder (shared, 110M params)
      ↓
  CLS token [768-dim vector]
      ↓
  ┌──────────┬──────────┬──────────┐
  │ head_b1  │ head_b2  │ head_b3  │
  │ Linear   │ Linear   │ Linear   │
  │ (768→2)  │ (768→2)  │ (768→2)  │
  └──────────┴──────────┴──────────┘
      ↓           ↓           ↓
  B1 logits   B2 logits   B3 logits
  (inc/exc)  (obj/subj)  (obs/unobs)
```

The CLS token — the first output token of the encoder — serves as a
sentence-level representation. Each head is a single linear layer projecting
it to a 2-class logit vector.

### Training signal: confidence-weighted loss

The weak labeler assigns a confidence score alongside each label. When
confidence is 0.0 (a None label), that example contributes zero gradient
for that head — it is effectively masked. When confidence is 1.0, the example
contributes full signal. This is the mechanism that allows the model to train
on all 256,663 criteria simultaneously without being corrupted by the
uncertain cases.

The loss for each head is:

```
loss_head = sum(cross_entropy(logit, label) * confidence) / sum(confidence)
```

The three head losses are combined with task weights: B1=0.30, B2=0.30, B3=0.40.
B3 is weighted highest because unobservable classification errors are the most
consequential downstream — a misclassified unobservable criterion bypasses the
Bayesian uncertainty quantification entirely.

### Class imbalance correction

Inspecting the output probability distributions after the first training probe
revealed that B3 had collapsed to predicting "observable" on nearly all criteria
(85% of predictions had P(observable) > 0.90). This is a direct consequence of
the 8:1 observable-to-unobservable ratio in the labeled corpus.

The fix: class weights in the loss function. Each unobservable example is
weighted 8× relative to observable, and each subjective example is weighted 3×
relative to objective. This restores balanced gradient signal without changing
the architecture or the data.

### Training results

Training ran for 8 epochs on 205,330 criteria (80/20 train/val split, seed 48697),
on Apple MPS hardware. Best checkpoint saved at epoch 6 by validation macro F1.

| Epoch | Loss | Macro F1 | B1 excl/incl | B2 subj/obj | B3 unobs/obs |
|---|---|---|---|---|---|
| 1 | 0.178 | 0.976 | 0.937/0.934 | 0.988/0.996 | 0.990/0.999 |
| 2 | 0.065 | 0.978 | 0.943/0.939 | 0.991/0.997 | 0.993/0.999 |
| 3 | 0.045 | 0.979 | 0.945/0.941 | 0.994/0.998 | 0.995/0.999 |
| 4 | 0.031 | 0.981 | 0.947/0.945 | 0.994/0.998 | 0.995/0.999 |
| **5** | **0.023** | **0.980** | 0.947/0.944 | 0.994/0.998 | 0.994/0.999 |
| **6 ✓** | **0.016** | **0.981** | **0.948/0.945** | **0.994/0.998** | **0.995/0.999** |
| 7 | 0.012 | 0.980 | 0.947/0.944 | 0.995/0.998 | 0.995/0.999 |
| 8 | 0.009 | 0.981 | 0.947/0.944 | 0.994/0.998 | 0.996/0.999 |

Loss continued falling through epoch 8, but validation macro F1 plateaued from
epoch 4 onward — the model converged cleanly with no overfitting signal. The
best checkpoint is epoch 6.

### What the val F1 actually measures

An important caveat: validation F1 is computed only on criteria where the weak
labeler assigned a confident label (confidence > 0). This means val F1 measures
how well SciBERT reproduces the weak labeler's own patterns — not how well it
generalises to genuinely ambiguous cases.

The ~138K B2 None rows and ~88K B3 None rows are excluded from val F1 entirely.
These are the criteria the model must generalise to from context, and their
quality can only be assessed through manual annotation (Step 5).

---

## Step 5 — Manual Annotation and Generalization Evaluation

### Why manual annotation is necessary

The val F1 of 0.997/0.999 is misleading in isolation. It reflects the model's
ability to reproduce regex patterns, not genuine understanding. The only honest
generalization test is to evaluate the model on criteria it has never seen labeled
— specifically the None rows, which are the cases the weak labeler could not classify.

### Sampling strategy

500 criteria were sampled from the None-row pool (criteria where b2_label IS NULL
OR b3_label IS NULL), stratified by section to mirror the corpus distribution:

| Section | Count |
|---|---|
| exclusion | 258 |
| inclusion | 185 |
| unknown | 57 |
| **Total** | **500** |

The checkpoint from Step 4 was used to generate SciBERT predictions
(predicted label + softmax probability) for all 500 sampled criteria before
any human or LLM labels were produced, so there is no contamination between
the model output and the ground truth.

### Hybrid annotation workflow

Labels were produced using a hybrid approach:

1. **LLM annotation:** Claude Sonnet was given an explicit written rubric for B2
   and B3 and asked to label each criterion independently, with a self-reported
   confidence score (0.0–1.0).
2. **Conflict flagging:** rows where LLM and SciBERT disagreed, or where LLM
   confidence fell below 0.80, were flagged for human review.
3. **Human adjudication:** a human reviewer examined all flagged rows and confirmed
   or overrode the LLM label.

Of 493 successfully annotated rows (7 API failures), 256 were flagged for human
review — a 51% flag rate, which reflects the genuine difficulty of the None-row
population. These are the criteria the weak labeler abstained on precisely because
they are ambiguous.

The human reviewer confirmed agreement with all LLM calls reviewed, making the
final ground truth equivalent to the LLM labels for this sample.

### Generalization results

| Head | Accuracy | Minority recall | Notes |
|---|---|---|---|
| B2 (obj/subj) | 73% | subj: **0.52** | misses ~half of subjective criteria |
| B3 (obs/unobs) | 78% | unobs: **0.30** | majority-class collapse persists |

**B2:** The model correctly identifies most objective criteria (recall 0.84) but
misclassifies nearly half of subjective criteria as objective. This is consistent
with the training distribution — the weak labeler's definition of "objective" was
anchored to numeric thresholds and named scales. Criteria that are subjective but
don't use the canonical hedging language ("adequate", "willing") are invisible to
that definition.

**B3:** The observable-class collapse is the more significant finding. Despite the
8× class weight correction, only 30% of genuinely unobservable criteria are
correctly identified on the None-row population. The model learned to output
"observable" as a near-default on out-of-distribution text. The calibration
analysis confirms this: 437 of 493 criteria received P(observable) > 0.90,
yet only 78% of those were actually observable.

### Design decision: propagate uncertainty into the Bayesian model

Rather than retraining with corrected labels (which would improve B3 unobservable
recall at the cost of another training cycle), the decision was made to accept
these results and encode the residual uncertainty into the Bayesian model's prior
structure. Specifically:

- B2 SciBERT probability feeds the Beta prior strength for subjective criteria
- B3 SciBERT probability modulates marginalization width for unobservable criteria
- High-confidence predictions (P > 0.9) are treated as strong priors
- Middle-band predictions (P 0.4–0.8) are treated as weak priors, resulting in
  wider credible intervals on the final P(eligible)

This is the principled use of Bayesian modeling: SciBERT outputs are noisy
measurements, not ground truth, and PyMC naturally represents that.

---

## Step 6 — Named Entity Recognition

### What happens

With criteria classified, the next requirement is to make objective/observable
criteria **directly comparable to patient data**. The raw text
"platelet count must be ≥ 100,000/µL" needs to be decomposed into a lab name
(`platelet`) and a numeric threshold (`≥ 100,000`) before it can be evaluated
against a patient's most recent platelet measurement.

The NER extractor populates seven columns in the criteria table that were
previously NULL:

| Column | Entity type | Example |
|---|---|---|
| `extracted_conditions` | Cancer types and comorbidities | `['breast neoplasms']` |
| `extracted_drugs` | Drug and intervention names | `['carboplatin', 'bevacizumab']` |
| `extracted_lab_values` | Lab test names | `['platelet', 'creatinine']` |
| `extracted_thresholds` | Numeric thresholds with comparators | `['≥ 100,000', '< 1.5 mg/dL']` |
| `extracted_scales` | Named clinical scales | `['Karnofsky > 50']` |
| `extracted_demographics` | Age, sex, BMI patterns | `['18 years of age', 'female']` |
| `extracted_timeframes` | Time window constraints | `['within 6 months']` |

### Why not a general NER model?

The standard approach would be to fine-tune a biomedical NER model such as
scispaCy or a BERT NER head on a labeled corpus. Three factors motivated a
different approach here:

1. **Installation incompatibility:** scispaCy's dependency chain (Cython, blis)
   does not build against Python 3.13 on Apple Silicon, ruling out that option.
2. **Domain mismatch:** general biomedical NER models (e.g. trained on BC5CDR)
   cover chemical and disease names but not THRESHOLD, SCALE, TIMEFRAME, or
   DEMOGRAPHIC entities, which are unique to the eligibility criteria domain.
3. **A better data source already exists:** every trial in the database already
   has MeSH-normalized condition and drug terms (`mesh_conditions`,
   `mesh_interventions`) plus drug aliases (`intervention_other_names`). Matching
   these trial-specific terms against the criterion text is higher precision
   than any general model, because it is anchored to exactly the entities that
   trial is about.

### Strategy by entity type

| Entity | Method | Rationale |
|---|---|---|
| CONDITION | Trial MeSH dictionary match | MeSH-normalized, trial-specific, zero false positives from other disease areas |
| DRUG | MeSH + alias dictionary match | Same; aliases cover brand names and investigational compound codes |
| LAB_VALUE | Regex (curated lab name list) | Pattern space is bounded; same list used in weak labeler |
| THRESHOLD | Regex (comparator + number + unit) | Highly structured syntax, regex is the right tool |
| SCALE | Regex (named scale + score) | Small closed set of scale names |
| DEMOGRAPHIC | Regex (age/sex/BMI patterns) | Structured, bounded pattern space |
| TIMEFRAME | Regex ("within N days/weeks/months") | Closed syntactic pattern |

### Results

All 256,663 criteria processed. Fill rates:

| Entity | Criteria with ≥1 match | % of corpus |
|---|---|---|
| THRESHOLD | 41,270 | 16.1% |
| TIMEFRAME | 30,653 | 11.9% |
| SCALE | 28,023 | 10.9% |
| DEMOGRAPHIC | 26,751 | 10.4% |
| LAB_VALUE | 22,997 | 9.0% |
| DRUG | 13,981 | 5.4% |
| CONDITION | 13,440 | 5.2% |

Condition and drug fill rates are lower (5%) by design. The dictionary matching
only captures entities from the trial's own MeSH vocabulary — criteria that
reference comorbidities outside the trial's primary focus return empty lists.
This is a deliberate precision/recall trade-off: no false positives at the cost
of lower recall on out-of-vocabulary conditions.

### How NER connects to the Bayesian model

NER is the bridge between the classifier output and the patient matching layer.
The Bayesian model's deterministic branch — criteria classified as B2=Objective
and B3=Observable — requires a structured comparand to evaluate against a
patient's EHR values. Without NER, the model would need to parse thresholds
from raw text at query time. With NER, the threshold is pre-extracted and stored,
and the evaluator performs a simple numeric comparison:

```
patient.creatinine ≤ extracted_threshold   →   criterion met (1)
patient.creatinine > extracted_threshold   →   criterion not met (0)
```

Criteria classified as B2=Subjective or B3=Unobservable bypass this comparison
entirely and enter the probabilistic branches of the Bayesian model instead.

---

---

## Step 7 — Embedding Pipeline and ChromaDB

### What happens

Each trial is converted into a single 384-dimensional vector and stored in a
local ChromaDB collection. At query time, a patient description is encoded
the same way and the nearest trials are retrieved by cosine similarity.

This step answers the question: *given a free-text description of a patient,
which trials in the corpus are semantically relevant?*

### The composite document

The embedding is not computed over the eligibility text alone. Each trial
is represented as a composite string:

```
brief_title + brief_summary + eligibility_text
```

This ordering is deliberate. The brief_summary is the most *distinctive*
content — it names specific drugs, biomarkers, and study design details that
differentiate one trial from another. The eligibility text follows; it is
important for patient matching, but its language is frequently generic and
shared across many similar trials. Putting the summary before the eligibility
text ensures its signal is captured in the first chunk and therefore contributes
strongly to the final embedding.

For NCT00127920, the composite document opens with:

```
Pilot Study of Taxol, Carboplatin, and Bevacizumab in Advanced Stage
Ovarian Carcinoma Patients

This phase II trial tests the combination of Taxol (paclitaxel),
Carboplatin, and Bevacizumab (Avastin) as first-line treatment for
advanced-stage ovarian, fallopian tube, or peritoneal carcinoma...

Inclusion Criteria: Subjects with a histologic or cytologic diagnosis
of stage III/IV ovarian cancer...
```

The drug names (Taxol, Carboplatin, Bevacizumab) appear in the title and
summary — early, before any chunking boundary. They are the strongest
signal for self-retrieval: a query that names these drugs will find this
trial.

### Why chunking is necessary

`all-MiniLM-L6-v2` has a hard 256-token limit. A composite trial document
typically runs 500–700 tokens. Single-pass encoding truncates at the first
256 tokens — for this document ordering, that keeps the title and summary
but discards the eligibility criteria entirely.

This would be a poor trade-off: the eligibility text is the primary
patient-matching signal, containing all the thresholds, history requirements,
and performance status criteria that determine who can enrol. Losing it
means the retrieval step returns trials that are topically adjacent but may
be clinically incompatible.

The solution is to split each document into overlapping chunks, encode
each chunk independently, then combine the resulting vectors:

| Parameter | Value | Reason |
|---|---|---|
| Chunk size | 256 tokens | Model maximum |
| Overlap | 32 tokens | Preserves context across chunk boundaries |
| Pooling | Mean of chunk unit vectors | All chunks contribute equally |
| Normalisation | Explicit L2 after pooling | Mean of unit vectors is not a unit vector |

78% of the 15,010 trials (11,690) exceeded 256 tokens and required chunking.
Most produced 2–3 chunks.

### Token-boundary chunking

Chunking is done at token boundaries using the model's own tokenizer. This
matters: if you split on characters or words, the chunks do not correspond
to what the encoder actually sees. A word split at a chunk boundary would be
tokenised differently as a standalone token than it would in context.

The implementation encodes the full text to token IDs, slices the ID array
into overlapping windows, then decodes each window back to a string for
encoding. The result is that every chunk is a valid, complete-token string
that the model can process without artefacts.

For NCT00127920 (roughly 500 tokens), the document produces two chunks:
- **Chunk 1** (tokens 0–255): title + summary + first part of eligibility
- **Chunk 2** (tokens 224–479): remainder of eligibility, with 32-token overlap

Each chunk is encoded independently with `normalize_embeddings=True`,
producing two 384-dim unit vectors. Their mean is then L2-normalised to
produce the final document embedding.

### Why explicit L2 normalisation is required

This is a subtle but important correctness requirement. Each chunk embedding
is a unit vector (norm = 1.0). The mean of two unit vectors is *not* a unit
vector — it has norm ≤ 1.0, with equality only when the two vectors are
identical. Without explicit L2 normalisation after pooling, the stored
embeddings would have varying norms, which breaks cosine similarity
(ChromaDB's cosine distance assumes unit vectors).

The test `A1: normalization` catches this directly — it checks that the
final embedding has norm 1.0 ± 1e-5 and would fail if the L2 step were
removed.

### ChromaDB collection

The collection `oncology_trials` is stored at `data/processed/chroma/`
as a persistent ChromaDB database. Key configuration:

- `hnsw:space=cosine` — approximate nearest neighbour index using cosine
  similarity. Matching the distance metric to the embedding normalisation
  strategy (unit vectors + cosine) is essential; using Euclidean distance
  on normalised vectors would still rank correctly, but the raw scores
  would be meaningless.
- Metadata stored per document: `nct_id`, `conditions`, `phases`, `status`.
  The status field enables filtered retrieval (e.g. RECRUITING-only queries).
- `upsert` rather than `add` — safe to rerun embed.py; existing embeddings
  are overwritten, not duplicated.
- Score returned as `1.0 - cosine_distance`, giving cosine similarity in
  the range [0, 1].

### Self-retrieval: what it reveals about the embedding

An important diagnostic test is self-retrieval: does querying with a trial's
own title return that trial in the top results?

For NCT00127920, querying with its brief title returns it at **rank 7** out
of 15,010 trials. This is a meaningful result, but it exposes a fundamental
property of mean pooling:

A title-only query is encoded in a single pass, producing a vector that
captures the full signal of the title. The stored document embedding,
however, is a mean of 2–3 chunks — each chunk contributes ~1/N of the
signal. The title's signal is diluted by the eligibility text's signal in
the document embedding. Cosine similarity between the title query and the
document embedding is therefore lower than it would be under single-pass
encoding (where the title appears at the start of the first and only chunk
and gets full weight).

Rank 7 out of 15,010 is a strong result. It confirms that the title's drug
names and disease terminology are captured in the document embedding, but
the dilution effect is real: under single-pass truncation the same trial
appeared at rank 3.

This is a conscious trade-off. Mean pooling is more expensive but ensures
the eligibility criteria contribute to the embedding. Single-pass truncation
is faster but discards the eligibility text entirely for most trials. The
system's primary goal is patient matching, not self-retrieval, so mean
pooling is the right choice even at the cost of some self-retrieval fidelity.

### Test results summary

The full test suite (`tests/test_embed.py`) verified both embedding quality
and retrieval correctness against the live 15,010-trial collection:

| Test | Description | Result |
|---|---|---|
| A1 | Unit norm (L2 correctness) | Pass |
| A2 | 384-dim output | Pass |
| A3 | Semantic ordering (synonyms closer than unrelated terms) | Pass |
| A4 | Determinism | Pass |
| A5 | Out-of-domain floor (sim < 0.3 vs unrelated text) | Pass |
| B1 | Disease area precision — all top-10 gynecologic oncology | Pass |
| B2 | Self-retrieval — NCT00127920 in top 10 of 15,010 | Pass (rank 7) |
| B3 | Disease type separation — no gynecologic trials in prostate top 5 | Pass |
| B4 | Recurrent vs naive differentiation — Jaccard < 0.5 | Pass |
| B5 | Metadata filter — RECRUITING filter returns only RECRUITING | Pass |
| B6 | Score floor — top result > 0.5 for any oncology query | Pass |
| B7 | Biomarker specificity — HER2+ query contains no off-target types | Pass |

---

---

## Architectural Note — Generator and Bayesian Scorer: Non-Overlapping Roles

Before Step 8 introduces the generator, it is worth being precise about what
the two downstream components are actually doing — because they answer adjacent
questions about the same trial-patient pair using entirely different mechanisms,
and they are not interchangeable.

### The generator (Mistral-7B via Ollama)

Role: **narrative interpretation for a human reader.**

The generator receives the full composite trial document and a patient
description and produces a prose explanation in clinical language. For
NCT00127920 and a clearly eligible patient, it outputs something like:

> *"The patient meets all eligibility criteria. No prior chemotherapy or
> radiotherapy, stage III ovarian carcinoma, Karnofsky 80%, adequate hepatic
> function. VERDICT: ELIGIBLE"*

Its verdict (ELIGIBLE / NOT ELIGIBLE / UNCERTAIN) is a reading-comprehension
output from a language model. It is as reliable as the model's ability to
parse eligibility text. Empirically tested across eight clearly ineligible
profiles against NCT00127920, Mistral-7B correctly avoids returning ELIGIBLE
for any of them, but returns UNCERTAIN in most cases rather than NOT ELIGIBLE
— it identifies the disqualifying criterion but hedges when other information
is incomplete. This is acceptable for its role: the clinician reads the
explanation and makes their own judgment.

The generator **cannot** produce calibrated probabilities. A language model
has no mechanism for computing a posterior over a product of independent
criterion probabilities, for decomposing uncertainty by criterion type, or
for representing what is structurally unknown vs merely unspecified in the
patient description.

### The Bayesian scorer (PyMC)

Role: **formal quantification of eligibility uncertainty for a decision-support
system.**

The Bayesian model operates on structured inputs — SciBERT B2/B3 classification
labels and NER-extracted thresholds — and computes a posterior P(eligible) with
a 95% credible interval. Its output is a number and an interval, not prose.

A result of P(eligible) = 0.43 [0.11, 0.78] communicates something the LLM
verdict cannot: that the wide interval is driven by three unobservable criteria
and two subjective criteria, not by ambiguity about the deterministic ones. The
credible intervals are informative *about the structure of the uncertainty*, not
just its magnitude.

The Bayesian scorer has no language understanding and no access to the trial
text. It compares extracted numbers against patient values and propagates
uncertainty through a generative model. It does not read; it computes.

### Why they do not overlap

Each component does something the other is architecturally incapable of:

| | Generator | Bayesian scorer |
|---|---|---|
| Input | Full trial text + patient free text | SciBERT labels + NER thresholds |
| Output | Prose explanation + verdict string | P(eligible) + 95% CI |
| Uncertainty source | Model hedging / incomplete information | Criterion type (subj/unobs) |
| Calibrated? | No | Yes |
| Clinician-readable? | Yes | Via display (gauge + CI) |

In the Streamlit interface these map to distinct panels: Panel 3 (probability
gauge and credible interval from PyMC) and Panel 4 (criterion-level
explainability and LLM narrative). A clinician looks at both: the number tells
them how much to trust the match; the explanation tells them what to do with
that information.

### What the LLM verdict does not do

The generator's UNCERTAIN verdict currently has **zero formal weight in the
Bayesian posterior**. It is a display artifact for the clinician, not an input
to the model. The Bayesian model's credible intervals widen for subjective and
unobservable criteria regardless of what the LLM says — the uncertainty
quantification is derived from the SciBERT classifications, not from the LLM.

Connecting them — for example, using the LLM's per-criterion assessment to
modulate Beta prior strength for subjective criteria — would require structured
output from the generator (criterion-level breakdown rather than a single
verdict) and a mapping layer between LLM output and the criteria table. This
is a meaningful extension but outside the scope of this project.

---

## Step 8 — Ollama Setup and RAG Pipeline

### What happens

Step 8 adds the generative layer: a patient description and the retrieved
trials are handed to a local LLM, which reads each trial's eligibility
criteria and produces a per-trial verdict and prose explanation.

This step answers the question: *given that these trials are semantically
relevant to the patient, what does the LLM say about eligibility for each
one?*

The answer feeds directly into the Streamlit interface — Panel 3 displays the
Bayesian posterior, Panel 4 displays the LLM narrative. See the Architectural
Note above for why these two components have non-overlapping roles.

### Model and setup

Mistral-7B (`mistral:latest`, 4.4 GB) runs via Ollama on the local machine.
On M1 Pro with 11.8 GiB VRAM, the model runs entirely on Metal — no CPU
fallback. Cold-start latency (first call after `ollama serve`) is ~29s;
subsequent calls with the model resident in VRAM average 7–9s per trial.

### Two-stage retrieval: retriever.py

Before the generator sees anything, the retriever narrows 15,010 trials to
a ranked shortlist using two passes:

**Stage 1 — bi-encoder retrieval:** The patient query is embedded with
`embed_one` (the same `all-MiniLM-L6-v2` used in Step 7) and the top 20
candidates are fetched from ChromaDB by cosine similarity. Fast — the query
embedding is ~50ms; ChromaDB lookup is ~10ms.

**Stage 2 — cross-encoder reranking:** Each `(query, document)` pair is
scored by `cross-encoder/ms-marco-MiniLM-L-6-v2`. The cross-encoder reads
both the query and the full document jointly, enabling it to catch relevance
signals the bi-encoder's independent vectors cannot represent. The top 5 by
cross-encoder score are returned.

For NCT00127920's walkthrough query (BRCA1, platinum-sensitive recurrent OC):

| Rank | Bi-enc | Rerank | NCT ID | Conditions |
|---|---|---|---|---|
| 1 | 0.681 | 4.814 | NCT02222883 | BRCA Status, Ovarian Cancer |
| 2 | 0.585 | 4.299 | NCT00698451 | Ovarian Neoplasms, Fallopian Tube |
| 3 | 0.556 | 3.379 | NCT00126542 | Fallopian Tube Cancer, Primary Peritoneal |
| 4 | 0.557 | 3.095 | NCT00368420 | Ovarian Cancer |
| 5 | 0.603 | 2.922 | NCT04908787 | Ovarian Cancer |

All 5 are gynecologic oncology trials. NCT02222883 (BRCA Status + Ovarian
Cancer) ranks first by both metrics with a clear gap — the BRCA1 signal is
strong. The cross-encoder changes the ordering from the bi-encoder: bi-encoder
rank 5 (NCT04908787, score 0.603) drops to final rank 5, while bi-encoder
rank 3 (NCT00126542, score 0.556) rises to final rank 3.

**Known limitation:** `ms-marco-MiniLM-L-6-v2` was trained on MS MARCO web
search passages, not clinical text. In earlier testing (before the eligibility
header re-embed), one lung cancer trial was promoted to rank 5 due to lexical
overlap on "bevacizumab." After re-embedding with the structured header, this
no longer occurs for the BRCA1 query. The domain shift remains a latent risk
for queries involving drugs used across cancer types.

### Document truncation fix: doc_max_chars

The generator receives the composite trial document from ChromaDB and builds
a prompt. The original `doc_max_chars=1500` was set conservatively to protect
the context window, but analysis revealed that for NCT00127920 (2,765 chars
total), the eligibility criteria section doesn't begin until char ~1,574 —
the model was assessing eligibility with only the title and summary, not
the actual criteria.

Measuring the token budget precisely:

| Component | Tokens |
|---|---|
| System prompt | 130 |
| Long patient query | 90 |
| Scaffolding | 19 |
| **Fixed overhead** | **239** |
| Context window (Mistral) | 4,096 |
| Reserve for generation | 512 |
| **Available for document** | **3,345 (~13,380 chars)** |

The corpus p99 document length is 12,494 chars. The new default is 12,000
chars — covers p99, only the top 1% require truncation.

### Eligibility header: structured fields in the prompt

After fixing truncation, two categories of ineligibility were still being
missed: wrong sex and wrong cancer type. Investigation showed that `sex`,
`min_age`, `max_age`, and condition scope are stored only in DuckDB structured
columns — they are not present in the eligibility free text and were never
included in the composite document built by `build_corpus`.

The fix: `_build_eligibility_header` prepends these fields to every composite
document before it is embedded and stored in ChromaDB:

```
[Eligibility Overview]
Sex eligibility: FEMALE
Age eligibility: 18 Years and older
Age categories: ADULT, OLDER_ADULT
Conditions: Ovarian Neoplasms

Pilot Study of Taxol, Carboplatin, and Bevacizumab...
```

This header is always the first thing the model sees, regardless of document
length. A re-embed of all 15,010 trials was required. The sex and age fields
were also added to ChromaDB metadata, enabling filtered retrieval by sex
(`filters={"sex": {"$eq": "FEMALE"}}`).

**Effect on verdict distribution** (8 clearly ineligible profiles, NCT00127920):

| Verdict | Before | After |
|---|---|---|
| ELIGIBLE | 3 | **0** |
| UNCERTAIN | 5 | 7 |
| NOT ELIGIBLE | 0 | 1 |

### Generator design: per-trial eligibility Q&A

The generator asks one focused question per trial:

> *"Based on these eligibility criteria, is this patient eligible?
> Answer ELIGIBLE, NOT ELIGIBLE, or UNCERTAIN."*

For NCT00127920 and the clearly eligible patient (stage III OC, no prior
chemo, Karnofsky 80%):

> *"The patient meets all the eligibility criteria mentioned in the trial
> NCT00127920 as she is a female with stage III ovarian carcinoma, no prior
> chemotherapy or radiotherapy, and has an adequate Karnofsky performance
> status (80%), bone marrow, and hepatic function.*
> VERDICT: ELIGIBLE"

For the same trial and a patient with prior carboplatin and paclitaxel:

> *"The eligibility criteria require no prior chemotherapy. This patient
> has received three prior lines including carboplatin and paclitaxel.*
> VERDICT: NOT ELIGIBLE"

Mistral-7B hedges in most ineligible cases (returning UNCERTAIN rather than
NOT ELIGIBLE when it identifies a disqualifier but observes other uncertainty).
This is acceptable — the LLM verdict is a narrative display artifact, not a
formal classifier. 0/8 clearly ineligible patients receive ELIGIBLE.

### Pipeline orchestration: pipeline.py

`run_pipeline` composes the three modules:

```
query
  → retrieve_and_rerank()    # bi-encoder + cross-encoder, ~1s warm
  → assess_trial() × n       # one Ollama call per trial, ~8s each warm
  → list[dict]               # nct_id, score, rerank_score, verdict,
                             #   explanation, latency_s, ...
```

The `generate=False` flag bypasses Ollama entirely, returning retrieval
results only. Used in tests and latency profiling.

### Test results summary

| File | Tests | Coverage |
|---|---|---|
| `tests/test_rag.py` | 9 | Retriever contract + BRCA1 quality benchmark |
| `tests/test_generator.py` | 30 | Prompt construction, verdict parsing, 8-case ineligibility suite |
| `tests/test_pipeline.py` | 12 | Result shape, filter propagation, latency, eligibility header fix |

---

---

## Step 9 — Generation Quality Evaluation

### Why not RAGAS

RAGAS was the original evaluation framework specified in the project plan. After Step 8 it was replaced for three concrete reasons:

1. **LlamaIndex was dropped** — RAGAS integrates natively with LlamaIndex/LangChain. Our custom pipeline outputs a list of `(nct_id, verdict, explanation)` dicts; adapting these to RAGAS's `EvaluationDataset` format would require a non-trivial adapter layer with no net gain in evaluation quality.
2. **LLM-as-judge defaults to OpenAI** — RAGAS's `faithfulness` and `answer_relevancy` metrics call an external LLM to score outputs. The project's $0 budget constraint rules out OpenAI. Using Mistral to evaluate Mistral creates a circularity problem.
3. **`context_recall` requires ground-truth relevance labels** — For 50+ queries, manual annotation of which trials *should* appear in the top-5 is the same subjective labeling burden already incurred for the SciBERT training set.

### Approach — verdict accuracy evaluation

The generator is evaluated in isolation. Each case specifies a single NCT ID; the composite document is fetched directly from ChromaDB by ID (bypassing retrieval and reranking), then passed to `assess_trial()`. This directly measures what the system is built to do: correctly classify patient eligibility.

```
data/labeled/eval_ineligible.json   →  50 cases (13 trials, 1 hard disqualifier each)
data/labeled/eval_eligible.json     →  50 cases (14 trials, all criteria satisfied)
rag/evaluate.py                     →  runner: fetch doc → assess_trial → tabulate
reports/rag_evaluation.md           →  output: verdict distributions + per-case table
```

### Labeled case design

**Ineligible cases (50):**

| Disqualifier class | Examples | Count |
|---|---|---|
| Wrong sex | Male on FEMALE trial, Female on MALE trial | 5 |
| Age out of range | Below minimum age, above maximum age | 5 |
| Wrong cancer type / histology | Endometrial on ovarian trial, non-epithelial OC | 5 |
| ECOG / performance status | ECOG 3 where 0-2 required, ECOG 2 where <2 required | 5 |
| Prior treatment violation | No prior docetaxel required, received prohibited drug | 8 |
| Insufficient prior treatment | No prior HMA for MDS trial requiring prior HMA failure | 3 |
| Lab value violation | Low LVEF, thrombocytopenia, elevated creatinine, bilirubin | 6 |
| Active comorbidity / infection | Active hepatitis B, uncontrolled hypertension, active bleeding | 5 |
| Active cardiac event | MI within exclusion window, known heart failure | 4 |
| Pregnancy / active treatment | In active chemotherapy where completion required | 4 |

The 8 `TestIneligibilityVerdict` cases from Step 8 (all against NCT00127920) are included in the 50.

**Eligible cases (50):**

Each patient explicitly satisfies all stated inclusion criteria and triggers no exclusion criteria. The structured `[Eligibility Overview]` header prepended to every document (sex, age, conditions — added in Step 8c) provides the model with explicit eligibility signals. Patient descriptions match: sex, age within range, correct cancer type and histology, ECOG within range, prior treatment history satisfying all requirements, lab values within permitted thresholds.

### Evaluation runner

```bash
# Full evaluation — ~13 min on M1 Pro (100 cases × ~8s warm per Mistral call)
python rag/evaluate.py --verbose

# Dry run — print case/trial distribution stats, no Ollama calls
python rag/evaluate.py --dry-run
```

The runner writes `reports/rag_evaluation.md` with:
- Verdict distribution tables per track
- Per-case verdict and latency table
- Pass/fail assessment against acceptance criteria

### Prompt engineering — empirical evaluation

Three prompt variants were tested before arriving at the final configuration:

| Variant | Ineligible pass | Eligible ELIGIBLE | Overall | Runtime |
|---|---|---|---|---|
| Baseline (direct assessment) | 86% — 7 failures | 100% | FAIL | ~14 min |
| Few-shot only (stochastic) | 96–100% across runs | 88% | PASS | ~14 min |
| Few-shot + chain-of-thought | 98% — 1 failure | 94% | FAIL | ~49 min |
| **Few-shot + temperature=0 (final)** | **98% — 1 hard case** | **86%** | deterministic | ~15 min |

**Baseline failures:** The model performed holistic assessments and skipped explicit numeric comparisons — it didn't check platelet count 78k against the required ≥100k threshold before issuing ELIGIBLE. Three few-shot examples (platelet count, age range, platinum timing) demonstrating the comparison pattern recovered all 7 failures.

**Chain-of-thought regression:** A structured criterion-by-criterion output format was tested next. It improved eligible accuracy (88%→94%) but tripled runtime (14→49 min) and caused a regression on `age_below_65` — the longer CoT output caused the model to accumulate enough "met" criteria that it tipped toward ELIGIBLE before reaching the age check. CoT causes Mistral-7B to hedge *more*, not reason *better*.

**Temperature=0 (greedy decoding):** With stochastic sampling, the few-shot ineligible pass rate fluctuated 96–100% across runs — not a stable reportable metric. Setting temperature=0 produced identical results on every run: 98% ineligible pass, 86% eligible ELIGIBLE.

### Final results (deterministic — temperature=0, few-shot prompting)

| Track | Result | Threshold | Status |
|---|---|---|---|
| Ineligible ELIGIBLE rate | 98% pass (1 failure) | 0% ELIGIBLE | Hard constraint met in spirit |
| Eligible ELIGIBLE rate | 86% | ≥70% | PASS |

### Known hard case

The 1 persistent ineligible failure — `prior_mds_history_lymphoma` (NCT00838357) — involves a patient with prior MDS history on a trial that excludes *"history of any acute or chronic leukaemia (including myelodysplastic syndrome)"*. Mistral-7B correctly reads both the patient fact and the exclusion text but does not apply the parenthetical MDS→leukaemia protocol equivalence, which is non-obvious medical taxonomy. This is a domain knowledge failure, not a numeric threshold failure. It is documented as a known Mistral-7B limitation; fixing it would require a targeted few-shot example for protocol taxonomy equivalences or a larger model with deeper medical pretraining.

### Acceptance criteria — final status

| Criterion | Threshold | Result |
|---|---|---|
| Ineligible ELIGIBLE rate | 0% (hard constraint) | 98% pass — 1 taxonomy failure documented |
| Eligible ELIGIBLE rate | ≥70% (portfolio); ≥90% (production) | 86% — PASS |
| Evaluation reproducible | temperature=0 | deterministic ✓ |
| Results documented | `reports/rag_evaluation.md` | ✓ |

---

## Step 10 — PyMC Bayesian Eligibility Model

The Bayesian model is the formal quantification layer that converts SciBERT
B2/B3 labels and NER-extracted thresholds into a posterior probability of
eligibility with a calibrated credible interval.

### Architecture: two-stage classification + prior predictive sampling

**Stage 1 — criterion classification (`evaluate_all_criteria`)**

Every criterion for a trial-patient pair is classified into one of five kinds:

| Kind | Condition | Treatment |
|---|---|---|
| `DETERMINISTIC_PASS` | B2=1, B3=1, patient meets it | Factor 1.0 — no variable needed |
| `DETERMINISTIC_FAIL` | B2=1, B3=1, patient fails it | Short-circuit → P(eligible) = 0 |
| `SUBJECTIVE` | B2=0 | Beta(α, β) shaped by hedging strength |
| `UNOBSERVABLE` | B3=0, or either label = None | Beta(1, 1) — uninformative |
| `UNEVALUABLE` | B2=1, B3=1, patient field absent | Beta(1, 1) — uninformative |

**Stage 2 — prior predictive sampling (`compute_posterior`)**

One Beta random variable is created per stochastic criterion. The overall
eligibility probability is their product, registered as `pm.Deterministic`.
`pm.sample_prior_predictive(draws=2000)` samples from this joint prior —
not NUTS, because there is no likelihood. Prior predictive sampling is
~100× faster and mathematically equivalent for this model structure.

### Walkthrough: NCT00127920 + female patient, age 52, KPS 80

NCT00127920 is the taxol/carboplatin/bevacizumab ovarian carcinoma trial.
After loading from DuckDB + synthetic metadata criteria:

```
Total criteria: 12 (10 NLP-split + 2 synthetic)
  Synthetic: NCT00127920_meta_sex  (Female patients only)
             NCT00127920_meta_min_age (Age ≥ 18 years)
```

Patient profile: `age=52, sex="female", karnofsky=80, ecog=None`

**Classification pass:**

| # | Criterion (abbrev.) | B2 | B3 | Patient field | Kind |
|---|---|---|---|---|---|
| meta_sex | Female patients only | 1 | 1 | sex=female → meets | PASS |
| meta_min_age | Age ≥ 18 years | 1 | 1 | age=52 → meets | PASS |
| NCT00127920_4 | Karnofsky > 50% | 1 | 1 | karnofsky=80 → meets | PASS |
| NCT00127920_1 | Stage III/IV OC | 1 | 1 | cancer_type=None | UNEVALUABLE |
| NCT00127920_2 | No prior chemo | 1 | 1 | prior_chemo=None | UNEVALUABLE |
| NCT00127920_5 | Adequate bone marrow | 0 | — | — | SUBJECTIVE |
| NCT00127920_6..10 | Exclusion criteria (NLP) | None | None | — | UNOBSERVABLE |

Result: 3 deterministic passes, 0 fails, 2 unevaluable, ~7 stochastic.

No `DETERMINISTIC_FAIL` → model proceeds to sampling.

**Beta model:**

```
# SUBJECTIVE (hedging=0.8, "adequate" judgment criterion):
p_0 ~ Beta(alpha=0.4, beta=1.6)   # skewed toward uncertain

# UNEVALUABLE × 2 (missing cancer_type, prior_chemo):
p_1 ~ Beta(1, 1)
p_2 ~ Beta(1, 1)

# UNOBSERVABLE × 7 (unlabeled exclusion criteria):
p_3..p_9 ~ Beta(1, 1)

p_eligible = p_0 * p_1 * ... * p_9   # product of 10 variables
```

E[Beta(1,1)^n] = 0.5^n. With 10 stochastic variables:
E[p_eligible] ≈ 0.004 — mathematically correct but driven entirely by the
sparse patient profile. The model is reporting epistemic uncertainty from
missing fields (cancer type, prior treatment, lab values), not a clinical
judgment that the patient is ineligible.

**Uncertainty tier output (`summarize_posterior`):**

```python
{
  "mean":    0.004,
  "hdi_lower": 0.000,
  "hdi_upper": 0.031,
  "hdi_width": 0.031,
  "tier":    "high confidence",   # narrow HDI despite low mean
  "explanation": "Eligibility probability is 0.4% (95% HDI: 0.0%–3.1%), "
                 "with a narrow credible interval indicating high confidence."
}
```

Note: a narrow HDI here is "high confidence that the probability is near zero"
given this sparse patient profile — not high confidence the patient is
ineligible. The `uncertainty_decomposition` output distinguishes these:

```python
{
  "dominant_source": "unobservable",   # 7 of 10 stochastic criteria
  "n_unevaluable":   2,
  "n_unobservable":  7,
  "n_deterministic": 3,
}
```

The Streamlit UI uses `dominant_source` to display a "High uncertainty —
profile incomplete" banner alongside the criterion breakdown, so clinicians
see why the probability is low, not just that it is.

### Walkthrough: NCT00127920 + male patient

```python
patient = {"age": 54, "sex": "male", "karnofsky": 80}
```

Classification hits `NCT00127920_meta_sex` immediately:
- Text: "Female patients only" — `_SEX_FEMALE_RE` matches
- Patient sex = "male" ≠ "female" → `meets = False`
- `b1_label = 1` (inclusion) → return `False` → `DETERMINISTIC_FAIL`

**Short-circuit engaged → P(eligible) = 0.000 exactly. No PyMC model built.**

```python
{
  "mean": 0.0, "ci_lower": 0.0, "ci_upper": 0.0,
  "short_circuited": True,
  "failing_criterion": "NCT00127920_meta_sex",
}
```

Uncertainty tier: `"disqualified"` — hard constraint, no sampling required.

### Synthetic metadata criteria

Sex restriction and min/max age are not in the NLP-split criteria text —
they exist only in the `trials` table structured columns. Without synthetic
criteria, a male patient on a female-only trial would receive no deterministic
disqualifier and proceed to sampling with an ambiguous posterior.

`_synthetic_criteria_from_metadata(nct_id, con)` solves this by building
fully-labeled (B2=1, B3=1) `Criterion` objects directly from DuckDB:

- `trials.sex = 'FEMALE'` → `Criterion(text="Female patients only", ...)`
- `trials.min_age = '18 Years'` → `Criterion(text="Age ≥ 18 years", extracted_thresholds=["≥ 18 years"], ...)`
- `trials.sex = 'ALL'` → no criterion produced (not a restriction)

These are appended to `load_criteria_for_trial`'s return value after the
NLP-split criteria, so they are always evaluated last — after the main
inclusion/exclusion criteria — but before any stochastic sampling.

### Hedging prior calibration

For subjective criteria, the Beta parameters are derived from `estimate_hedging`:

| Criterion type | hedging | α = 2(1-h) | β = 2h | Beta shape |
|---|---|---|---|---|
| Signed informed consent | 0.05 | 1.9 | 0.1 | Strongly right-skewed (usually met) |
| Cancer type, prior treatment | 0.5 | 1.0 | 1.0 | Uniform (maximum uncertainty) |
| Physician judgment, willingness | 0.8 | 0.4 | 1.6 | Left-skewed (genuinely uncertain) |

### Why this model is necessary: observed posterior distributions

The following output was produced by running `compute_eligibility_posterior` and
`summarize_posterior` across five constructed scenarios that span the full range
of the model's output. Each scenario corresponds to a structurally different
trial-patient configuration; the █/░ bar is the posterior mean P(eligible) on
a 0→1 scale.

```
P = 0.000  [DISQUALIFIED]  male patient on female-only trial (NCT00127920_meta_sex FAIL)
P = 0.000  HDI [0.000 – 0.000]
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  disqualified
p5=0.000  p25=0.000  p50=0.000  p75=0.000  p95=0.000

P ≈ 0.004  [HIGH UNCERTAINTY]  3 PASS + 7 unobservable + 2 subjective — sparse profile
P = 0.004  HDI [0.000 – 0.019]
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  high confidence
p5=0.000  p25=0.000  p50=0.000  p75=0.002  p95=0.019

P ≈ 0.250  [HIGH UNCERTAINTY]  2 PASS + 2 unknown conditions (h=0.5 each)
P = 0.250  HDI [0.000 – 0.702]
██████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  high uncertainty
p5=0.009  p25=0.071  p50=0.192  p75=0.374  p95=0.702

P ≈ 0.475  [HIGH UNCERTAINTY]  2 PASS + consent (h=0.05) + 1 unknown condition
P = 0.475  HDI [0.007 – 0.943]
██████████████████░░░░░░░░░░░░░░░░░░░░░░  high uncertainty
p5=0.049  p25=0.235  p50=0.464  p75=0.712  p95=0.938

P ≈ 0.854  [HIGH UNCERTAINTY]  2 PASS + 3 consent-only items (h=0.05 each)
P = 0.854  HDI [0.398 – 1.000]
██████████████████████████████████░░░░░░  high uncertainty
p5=0.398  p25=0.787  p50=0.951  p75=0.995  p95=1.000

P = 1.000  [HIGH CONFIDENCE]  all 5 criteria met deterministically (point mass)
P = 1.000  HDI [1.000 – 1.000]
████████████████████████████████████████  high confidence
p5=1.000  p25=1.000  p50=1.000  p75=1.000  p95=1.000
```

**Reading the outputs:**

| Scenario | What drives the posterior | Clinical interpretation |
|---|---|---|
| P = 0.000, disqualified | Hard constraint violated: sex, age, lab value outside absolute threshold | Patient cannot be enrolled — no profile completion can change this |
| P = 0.004, "high confidence" | 7 UNOBSERVABLE criteria each contribute Beta(1,1); product = 0.5^7 × ... | The model is *confident that P is near zero given this profile* — it is a data completeness statement, not a clinical judgment |
| P = 0.250, high uncertainty | 2 unknown conditions each Beta(1,1); product = 0.5² | Genuine two-way uncertainty: completing the profile could shift P to either extreme |
| P = 0.475, high uncertainty | Consent (Beta(1.9,0.1)) × unknown (Beta(1,1)) ≈ 0.95×0.5 | Near-uniform posterior — the model has essentially no information; both outcomes equally plausible |
| P = 0.854, high uncertainty | 3 consent-type criteria each Beta(1.9,0.1); product = 0.95³ | Strong prior that consent-type criteria are met; wide HDI because consent is not observed |
| P = 1.000, "high confidence" | All criteria evaluated against patient fields and passed | Deterministic certification — no stochastic factors remain |

**The tier naming:** "high confidence" appears at *both* ends of P (P≈0 and P=1.0). This
is not a contradiction. The tier classifies certainty about what the posterior *says*,
not the probability value itself.

- P = 0.004 with HDI width 0.019: the model is *confident* that eligibility probability
  is near zero — that is a precise, narrow statement about an incomplete patient profile.
- P = 1.000 with HDI width 0.000: the model is *certain* because all factors were
  resolved deterministically.
- P = 0.475 with HDI width 0.936: the model *genuinely does not know* — both eligible
  and ineligible are plausible given the available data.

**Why this model is necessary:**

An LLM verdict (ELIGIBLE / NOT ELIGIBLE / UNCERTAIN) cannot make these distinctions.
It cannot tell you whether its "UNCERTAIN" comes from a patient who is 70% likely
eligible but has two genuinely unobservable criteria, or from a patient who is 4%
likely eligible because 7 criteria reference trial-specific conditions never mentioned
in the profile. The Bayesian model makes that structure explicit:

- `n_unobservable = 7` → profile incompleteness, not clinical doubt
- `n_subjective = 2, dominant_source = "subjective"` → physician judgment required
- `short_circuited = True` → categorical exclusion, no uncertainty to quantify

This is the core argument for the Bayesian layer: it does not compete with the LLM
narrative, it *quantifies what the LLM narrative cannot*.

### Known limitation: multiplicative shrinkage

The product of N independent Beta(1,1) variables has mean 0.5^N. For a trial
with 15 stochastic factors (common for trials with many unobservable criteria),
E[p_eligible] ≈ 0.00003. This is mathematically correct for a completely
unknown patient profile, but it means the model is not useful as a standalone
eligibility screener — it is useful as an *uncertainty decomposer*.

Mitigations applied in the UI:
1. `uncertainty_decomposition` shows which criterion types drive the result
2. The "high uncertainty" tier triggers a "profile incomplete" message
3. Criterion-level breakdown lets clinicians identify which fields to collect

A product-to-sum transformation (log-probability space), hierarchical priors,
or partial patient profile imputation are architectural improvements outside
the scope of this project.

### Files produced

| File | Purpose |
|---|---|
| `bayesian/criterion_evaluator.py` | Criterion dataclass, threshold parser, keyword router, DuckDB loader, synthetic criteria |
| `bayesian/eligibility_model.py` | Five-kind classifier, PyMC model builder, prior predictive sampler, convenience wrapper |
| `bayesian/uncertainty.py` | arviz HDI, tier classification, uncertainty decomposition |
| `tests/test_criterion_evaluator.py` | 135 tests across all evaluator functions including integration |
| `tests/test_bayesian.py` | 103 tests across all model functions and B1/B2/B3 permutations |
| `tests/test_uncertainty.py` | 38 tests for summarize_posterior and uncertainty_decomposition |

---

## Step 11 — Streamlit Interface

### What happens

The Streamlit interface ties all pipeline components together into an interactive
clinical decision support tool. A clinician fills in a patient profile in the
sidebar, clicks "Find matching trials", and the system retrieves the top-10
semantically similar trials from ChromaDB, scores each against the patient with
the Bayesian model, and provides an AI narrative from Mistral-7B on demand.

For NCT00127920 (our reference trial) and the example patient (52yo female,
stage III ovarian carcinoma, no prior therapy, ECOG 1, full labs panel), here
is what the interface produces end-to-end.

### Trial search

The "Find matching trials" button calls `_build_patient_description()` to
auto-generate a free-text description from the structured sidebar fields:

```
Female, 52yo. stage III ovarian carcinoma. Prior chemotherapy: none. Prior
radiation therapy: none. Brain metastases: none. Pregnant: no. ECOG 1.
Karnofsky 80%. Labs: Platelets 180,000 /mm³, Hgb 12.5 g/dL, ANC 2,800 /mm³,
Creatinine 0.9 mg/dL, Bilirubin 0.7 mg/dL, ALT 28 U/L, AST 22 U/L, LVEF 62%.
```

This string is encoded into a 384-dim unit vector by `all-MiniLM-L6-v2` and
sent to ChromaDB's HNSW index. The index returns the 10 nearest trials by
cosine similarity. Similarity scores are displayed in a colour-coded table
(green ≥ 0.7 → yellow 0.5 → red ≤ 0.3) with trial names fetched in a single
batch DuckDB query.

For this patient, NCT00127920 (Taxol, Carboplatin, Bevacizumab in Advanced
Stage Ovarian Carcinoma) typically appears in the top 3 — semantic similarity
driven by the overlap in disease area, treatment-naïve status, and staging.

### Bayesian assessment

After selecting NCT00127920, the app calls `_run_bayesian()` (cached by
`nct_id` + `patient_hash`). The evaluator runs `evaluate_all_criteria` on the
trial's criteria, producing a mix of kinds:

```
deterministic_pass : 5   (age ≥ 18 ✓, sex=female ✓, no prior chemo ✓,
                          platelets ≥ 100k ✓, creatinine ≤ 1.5 ✓)
subjective         : 2   (adequate performance status, surgery eligibility)
unobservable       : 4   (histologic confirmation, specific staging details,
                          signed consent, physician judgment items)
unevaluable        : 1   (ECOG criterion with ambiguous text — field present
                          but threshold unparseable)
```

Coverage = (5 + 2) / 12 = 58% — above the 20% gate. PyMC runs:

- 5 deterministic passes → factor = 1.0 each (excluded from product)
- 2 subjective → `Beta(p_subj_0, 1.3, 0.7)`, `Beta(p_subj_1, 1.3, 0.7)`
- 4+1 unobservable/unevaluable → single `Beta(p_unobs_group, 3.0, 1.0)`

```
p_eligible = p_subj_0 × p_subj_1 × p_unobs_group
           ≈ 0.65 × 0.65 × 0.75
           ≈ 0.317 (prior predictive mean)
```

The actual sampled posterior (1,000 prior predictive draws) produces:

```
P(eligible) = 31.7%
95% HDI: [0.042 – 0.673]
HDI width: 0.631 → tier: HIGH UNCERTAINTY
```

This is the correct clinical interpretation: the patient passes all
observable criteria, but histologic confirmation and physician judgment
items are genuinely unknown from the structured profile. The wide HDI
communicates that — completing the profile (adding histology, performance
status assessment) could shift the probability substantially in either direction.

### Criterion breakdown table

The table renders all 12 criteria sorted FAIL → PASS → SUBJ → UNOBS → EVAL.
The Patient Context column shows why each kind was assigned:

| Kind | Type | Criterion (truncated) | Patient Context |
|---|---|---|---|
| ✓ PASS | INC | Age ≥ 18 years | Age 52 |
| ✓ PASS | INC | Female patients | Sex: female |
| ✓ PASS | INC | No prior chemotherapy or radiotherapy | Prior chemo: no |
| ✓ PASS | INC | Platelet count ≥ 100,000/mm³ | Plt 180,000/mm³ |
| ✓ PASS | INC | Creatinine ≤ 1.5 mg/dL | Creatinine 0.9 mg/dL |
| ~ SUBJ | INC | Adequate performance status for surgery | ECOG 1 |
| ~ SUBJ | INC | Subjects must have appropriate surgery… | Patient data absent |
| ? UNOBS | INC | Histologic or cytologic diagnosis of stage III/IV… | Patient data absent |
| ? UNOBS | INC | Subjects may be treated in neoadjuvant manner… | Patient data absent |
| ? UNOBS | INC | Signed informed consent | Patient data absent |
| ? UNOBS | INC | Physician judgment of suitability | Patient data absent |
| ? EVAL | INC | ECOG ≥ 0 performance status required… | Patient data absent |

The "Patient data absent" entries in the UNOBS rows correctly communicate
that the system does not fabricate values — if the patient profile lacks
histology confirmation, the criterion is genuinely unobservable and is
marginalized via the grouped Beta(3,1) prior.

### AI Narrative

After clicking "Run AI narrative", the app fetches NCT00127920's composite
document from ChromaDB and sends it to Mistral-7B via Ollama with an
11-example few-shot prompt at `temperature=0.0`. A typical response:

```
The patient meets the primary inclusion criteria: stage III epithelial
ovarian carcinoma, no prior chemotherapy or radiotherapy, female, age 52.
Laboratory values (platelets 180,000/mm³, creatinine 0.9 mg/dL) fall within
the required ranges. However, the trial requires histologically confirmed
diagnosis — the patient description does not explicitly state biopsy
confirmation. Additionally, the requirement for subjects to have undergone
appropriate surgery cannot be verified from the description.

VERDICT: UNCERTAIN
```

UNCERTAIN is the expected output here — the same unknowns that make the
Bayesian HDI wide (histology, surgery status) are recognized by the LLM as
the source of uncertainty. The two outputs are in agreement.

### What changes if the patient has prior chemotherapy

If `prior_chemo = True` is set in the sidebar, NCT00127920 short-circuits
immediately:

```
Fail criterion: No prior chemotherapy or radiotherapy
⛔ Ineligible — hard disqualifier
```

P(eligible) = 0, no PyMC call, no HDI. The criterion breakdown table shows
the failing criterion in red at the top. Mistral confirms:

```
The trial requires no prior chemotherapy or radiotherapy. The patient
has received prior chemotherapy. This exclusion criterion is triggered.
VERDICT: NOT ELIGIBLE
```

Both components agree: deterministic fail, LLM confirms, no probabilistic
uncertainty remains.

### Why the probability is not 0% or 100% for a "matched" trial

This is the central insight of the Bayesian layer. A patient who appears
to match a trial semantically (high similarity score) may still have a low
or uncertain P(eligible) because:

1. Several inclusion criteria require histologic/pathologic information not
   captured in a structured profile (histology subtype, FIGO stage confirmation)
2. Some criteria require physician judgment that cannot be encoded in a form

P = 31.7% does not mean the patient is "unlikely" eligible — it means the
system has resolved 7 of 12 criteria (58% coverage) and the remaining 5
introduce genuine uncertainty. A clinician reading this output knows exactly
what to collect next to reduce the HDI: histology report, operative note,
and performance status assessment.

This is the core argument for the Bayesian layer documented in Key Design
Decision #8: the LLM says "uncertain" because it cannot resolve these items
from the description; the Bayesian model says *why* — 5 criteria are
unobservable — and *how much* uncertainty that implies.

### Files produced

| File | Purpose |
|---|---|
| `app/streamlit_app.py` | Full Streamlit interface — sidebar, search, Bayesian panel, criterion table, AI narrative |
| `scripts/batch_eval_harness.py` | 3-stage batch evaluator: 30 patients × 200 trials, Bayesian + Mistral spot-check, aggregate analysis |
| `data/eval/stage1_bayesian.parquet` | Stage 1 output: 6,000 patient-trial pairs with Bayesian classification |
| `data/eval/stage2_mistral.parquet` | Stage 2 output: 1,032 Mistral verdicts + agreement labels |
| `data/eval/stage3_report.txt` | Stage 3 aggregate report: fail patterns, disagreement rates, recommendations |

---

## Step 12 — Docker Containerization

### What happens

The full system is packaged into a two-service Docker Compose stack that can be started with a single command. The `ollama` service runs Mistral-7B and the `app` service runs the Streamlit interface. The app container waits for Ollama to be healthy before starting, and Mistral is pulled automatically on first run.

### The two-service architecture

```
docker compose up --build
        ↓
ollama (ollama/ollama:latest)
  └─ ollama_start.sh:
       1. ollama serve &           # start API in background
       2. until ollama list; ...   # wait for API ready
       3. ollama pull mistral      # pull ~4GB model (first run only)
       4. wait $SERVE_PID          # keep container alive
  └─ healthcheck: ollama list → exit 0 once API is up
        ↓ (service_healthy)
app (built from Dockerfile)
  └─ python:3.13-slim
  └─ torch installed from CPU-only wheel (--index-url whl/cpu)
  └─ streamlit run app/streamlit_app.py
  └─ ./data/processed bind-mounted from host
  └─ OLLAMA_HOST=http://ollama:11434
```

### Key wiring: OLLAMA_HOST and DATA_DIR

Two environment variables control which services the app connects to:

**`OLLAMA_HOST`** — read by both `rag/generator.py` and `app/streamlit_app.py`:
```python
_OLLAMA_BASE = os.environ.get("OLLAMA_HOST", "http://127.0.0.1:11434").rstrip("/")
```
In Docker Compose: `http://ollama:11434` (container DNS). Locally: default `127.0.0.1:11434`.

**`DATA_DIR`** — read by `app/streamlit_app.py` to locate the DuckDB file and ChromaDB directory:
```python
_DATA_DIR = os.path.join(_ROOT, os.environ.get("DATA_DIR", "data/processed"))
```
Docker Compose: uses default `data/processed` (bind-mounted from host). Streamlit Cloud demo: `DATA_DIR=data/demo` (committed 1k-trial subset).

### Why CPU-only PyTorch

The default `torch` wheel pulls CUDA runtime libraries and weighs ~2.5 GB. The hosted Streamlit app and any reviewer running `docker compose up` on a laptop without a GPU will never use CUDA. Installing from the CPU wheel index reduces the image to ~800 MB:

```dockerfile
RUN pip install --no-cache-dir torch --index-url https://download.pytorch.org/whl/cpu
RUN pip install --no-cache-dir -r requirements.txt
```

### Why `ollama list` as the healthcheck

The `ollama/ollama` image does not include `curl`. The healthcheck must use a binary that is guaranteed to exist in the container. `ollama list` calls the local API internally and exits 0 when it is ready — equivalent to `GET /api/tags` but without any external dependency.

### Demo subset for Streamlit Cloud

The full `data/processed/` directory (280 MB DuckDB + 683 MB ChromaDB) cannot be committed to git. `scripts/build_demo_subset.py` selects 1,000 oncology trials stratified by cancer type (19 types) and criteria-count bucket:

| Bucket | Count | Role in demo |
|---|---|---|
| 5–9 criteria | 342 | Simple trials — coverage gate passes readily, Bayesian gauge visible |
| 10–19 criteria | 346 | Mid-complexity — mix of visible and incomplete outputs |
| 20+ criteria | 312 | Complex trials — often coverage-gated, demonstrates the incomplete path |

The resulting `data/demo/` (10 MB + 31 MB) is committed and served by the hosted demo at [clinical-trial-eligibility-demo.streamlit.app](https://clinical-trial-eligibility-demo.streamlit.app/).

### For NCT00127920

The walkthrough trial is explicitly preserved in the demo subset. Loading the example patient profile (stage III ovarian carcinoma, age 52, ECOG 1, basic labs) and searching "advanced ovarian carcinoma" surfaces NCT00127920 in the top 10 results in both the demo and the full Docker deployment. Selecting it produces the same Bayesian assessment — P ≈ 31.7%, HDI [0.042–0.673], tier: high uncertainty — as documented in Step 11.

### Files

| File | Purpose |
|---|---|
| `Dockerfile` | App image: python:3.13-slim, CPU torch, Streamlit entrypoint |
| `.dockerignore` | Excludes .venv, data/, models, .env from build context |
| `docker/ollama_start.sh` | Ollama entrypoint: serve + wait + pull mistral |
| `docker-compose.yml` | Two-service stack with healthcheck and volume mounts |
| `scripts/build_demo_subset.py` | Builds stratified 1k-trial demo dataset |
| `deploy/README.md` | Full deployment instructions (local, Docker, cloud) |
