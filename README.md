Kyabirwa Surgical Center (KSC) — Chronic Post-Surgical Pain (CPSP) Study

Analysis Repository (2019–2024 CCS Dataset)

This repository contains a fully reproducible analysis pipeline for the Kyabirwa Surgical Center (KSC) study of chronic post-surgical pain (CPSP) following ambulatory inguinal hernia repair in rural Uganda.

The workflow implements:
	•	Carolina Comfort Scale (CCS) scoring
	•	Pain, mesh sensation, and functional limitation domains
	•	CCS ≥75% completion rule and score rescaling
	•	Domain-level prevalence estimation
	•	Crosstab association testing (Fisher/Chi-square)
	•	GLM logic with event-count gating
	•	Item-level (“question-by-question”) analysis
	•	Automated tables and figures for presentation and manuscripts

⸻

Project Structure (R Project)

KSC_CPSP/
│
├── data/                         # Raw CCS survey data (PHI; *not* tracked by Git)
│
├── scripts/
│     ├── 01_core_setup.R         # Data load, cleaning, helper functions
│     ├── 02_pain_analysis.R      # Pain domain scoring & plots
│     ├── 03_mesh_analysis.R      # Mesh domain analyses
│     ├── 04_limit_analysis.R     # Limitation domain analyses
│     ├── 06_cpsp_protocol_pipeline.R
│     ├── 07_mesh_limit_crosstabs.R
│     ├── 08_question_by_question_analysis.R
│     └── ZZ_master_notes_and_outline.R   # Internal long-form documentation
│
├── outputs/                      # Domain prevalence tables, crosstabs, etc.
├── figures/                      # Plots for manuscript/slides
│
└── README.md


⸻

Carolina Comfort Scale (CCS) Domain Definitions

Domain	Items	Score Range	Threshold
Pain	8	8–40	≥16
Mesh sensation	8	8–40	≥16
Functional limitation	7	7–35	≥14

CCS ≥75% completion rule

A domain is scored only if the respondent answered at least 75% of items:
	•	Pain: ≥6 of 8
	•	Mesh: ≥6 of 8
	•	Limitation: ≥6 of 7 (because limitation_lying is missing in the Excel file)

CCS rescaling

When ≥75% items are completed:

rescaled_sum = raw_sum × (total_items / items_answered)

This rescales partially completed domains to the full item count.

⸻

Final Valid N by Domain (After CCS Rules)

Domain	Valid N	Positive (n, %)	Notes
Pain (CPSP ≥16)	186	2 (1.1%)	Full dataset
Mesh sensation ≥16	184	1 (0.5%)	Mild missingness
Limitation ≥14	163	1 (0.6%)	More missingness

Limitation has the fewest analyzable respondents due to the 75% completion requirement for a 7-item domain.

⸻

Overlap Between Domains

To check whether the same patients are positive across domains:

df_flags %>%
  filter(cpsp == "yes" | mesh_positive == "yes" | limit_positive == "yes") %>%
  select(subject_id, sum_pain, cpsp, sum_mesh, mesh_positive, sum_limit, limit_positive)

Example from this dataset:

subject_id	sum_pain	cpsp	sum_mesh	mesh_positive	sum_limit	limit_positive
1	30	yes	21	yes	27	yes
76	19	yes	11	no	NA	NA

Interpretation:
	•	CPSP: 2 patients
	•	Mesh-positive: 1 patient
	•	Limitation-positive: 1 patient
These are not all the same individuals.

⸻

Crosstab & GLM Logic (IRB-Aligned)
	1.	Compute CPSP prevalence
	2.	Perform crosstab tests (Fisher/Chi-square) for each predictor
	3.	Screen predictors by p < 0.20
	4.	Fit GLMs only if ≥10 events
	5.	Because CPSP events = 2, GLMs are appropriately not run

Manuscript wording:

“Manual labor met p < 0.20, but multivariable models were not advanced due to insufficient positive events.”

⸻

Table 2 Example (Manual Labor Associations)

Outcome	Level	Yes/Total (%)	p-value
CPSP (Pain ≥16)	No	2 / 82 (2.4%)	0.19
	Yes	0 / 104 (0.0%)	—
Mesh Sensation ≥16	No	1 / 82 (1.2%)	0.44
	Yes	0 / 104 (0.0%)	—
Limitation ≥14	No	1 / 72 (1.4%)	0.44
	Yes	0 / 91 (0.0%)	—


⸻

Item-Level (“Question by Question”) Analysis

The scripts generate:
	•	% of respondents with score ≥2 for each item
	•	Rankings of most frequently bothersome activities
	•	Faceted Likert histograms

Example (Pain domain):

Item	% scoring ≥2
Daily activities	8.6%
Lying down	7.5%
Bending	7.0%

Equivalent outputs are produced for mesh sensation and functional limitation.

⸻

Reproducible Workflow

Run in this order:

source("scripts/01_core_setup.R")
source("scripts/02_pain_analysis.R")
source("scripts/03_mesh_analysis.R")
source("scripts/04_limit_analysis.R")
source("scripts/06_cpsp_protocol_pipeline.R")
source("scripts/07_mesh_limit_crosstabs.R")
source("scripts/08_question_by_question_analysis.R")

Outputs appear automatically in outputs/ and figures/.

⸻

Notes
	•	This project does not use renv by default (simple global package library).
	•	All helper functions live in 01_core_setup.R.
	•	data/ is excluded from GitHub for PHI compliance.
	•	Missingness in mesh and limitation domains is expected and documented.

⸻

Contact

Primary contact: Tanner Frahm
Clinical oversight: Dr. Linda Zhang, Director of Global Surgery
