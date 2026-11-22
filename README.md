
# Cardiometabolic Risk Profiles and Obesity in Mexican Adults: An Exploratory Analysis of ENSANUT 2022

## 1. Project overview

This project examines the association between obesity-related profiles and hypertension among Mexican adults, using data from Mexico's 2022 National Health and Nutrition Survey (ENSANUT 2022).

The analysis focuses on the joint classification of body mass index (BMI) and abdominal obesity, and its association with hypertension, accounting for relevant demographic characteristics and the complex survey design.

The primary comparison is **Obesity II/III with abdominal obesity** versus **Normal BMI without abdominal obesity**.

The project is exploratory and observational. Its findings should not be interpreted as causal effects.

## 2. Research question

Among adults included in the analytical sample, how is the profile of Obesity II/III with abdominal obesity associated with hypertension, relative to normal BMI without abdominal obesity, after adjustment for age, age squared, sex, and the `estrato` adjustment category?

## 3. Data and provenance

The analysis uses ENSANUT 2022 adult-health and anthropometric information.

The original source datasets include:

- `ensadul2022_entrega_w.dta` — adult-health microdata.
- `ensaantro2022_entrega_w.dta` — anthropometry microdata.
- `SALUD/NUTRICION/DICCIONARIOS/` — supporting questionnaires, methodology documentation, Stata files, and exploratory materials.

The final R model reads the prepared analytical file:

`ENSANUT2022_cleaned_analytical.csv.gz`

The compressed file is located in the repository root directory.

**Data-publication policy:** Individual-level ENSANUT survey data and restricted analytical datasets are not included in this public repository. Researchers must obtain the original datasets through the appropriate official channels and comply with their applicable terms of use.

**Data-provenance limitation:** The prepared CSV is an input to the final R analysis. The complete, verified sequence of steps that constructs this CSV from the original ENSANUT files is not yet documented in this repository. Therefore, the current workflow reproduces the analysis conditional on the prepared CSV being available; it should not yet be described as a fully automated, end-to-end reconstruction from the original microdata.

Before claiming full end-to-end reproducibility, the data-preparation workflow must be documented, including variable derivations, merge keys, exclusions, and the treatment of missing observations. These steps should be verified against the actual preparation code and source documentation rather than inferred from variable names alone.

## 4. Analytical sample and survey design

The complete-case analytical sample used for the reported model contains **8,044 adults**.

The analysis uses:

- 432 primary sampling units (PSUs), identified within sampling strata;
- 94 sampling strata;
- 338 survey-design degrees of freedom.

The primary model uses the anthropometry expansion weight, `ponde_f_anthro`. The adult survey weight, `ponde_f_adult`, is used in a sensitivity analysis.

The survey design is specified in R using the PSU, sampling-stratum, and weight variables, with nested PSUs and the `survey.lonely.psu = "adjust"` option.

The variable `est_sel` identifies the sampling stratum in the survey design. The variable `estrato` is included separately as an adjustment covariate. These variables serve different purposes and should not be conflated.

## 5. Variables and statistical specification

**Outcome:** `hypertension`.

Hypertension is operationally defined using either:

- A prior affirmative report of physician-diagnosed hypertension; or
- Elevated measured blood pressure, defined as mean systolic blood pressure ≥140 mmHg or mean diastolic blood pressure ≥90 mmHg.

This is an operational research definition based on self-reported diagnosis and measured blood pressure. It should not be interpreted as an independent clinical diagnosis.

**Exposure:** `model_category`, a joint BMI and abdominal-obesity profile.

**Reference category:** `Normal | No abdominal obesity`.

**Main comparison category:** `Obesity II/III | Abdominal obesity`.

Abdominal obesity is classified using sex-specific waist-circumference thresholds.

**Adjustment covariates:** `edad`, age squared, `sexo`, and `estrato`.

The primary estimator is a survey-weighted linear probability model fitted with the R `survey` package using `svyglm(..., family = gaussian())`. Variance estimation uses Taylor-series linearization under the specified complex survey design.

The reported coefficient is the conditional best linear projection coefficient for the comparison category relative to the reference category, holding the included covariates fixed. It is not a causal effect, an exact conditional-mean contrast, or a standardized population-average marginal prevalence difference.

## 6. Main results

For the comparison of Obesity II/III with abdominal obesity against the reference category, the primary model estimates:

- **Coefficient:** 0.2535 (approximately 25.35 percentage points).
- **Standard error:** 0.0300 (approximately 3.00 percentage points).
- **95% confidence interval:** 0.1945 to 0.3126 (approximately 19.45 to 31.26 percentage points).

The sensitivity model using `ponde_f_adult` estimates approximately **25.59 percentage points** for the same contrast and model specification.

The similarity between the primary and sensitivity estimates indicates that the result is not substantially changed by this alternative survey-weight specification.

These are adjusted observational associations. They should not be interpreted as evidence that the anthropometric profile causes hypertension.

## 7. Project files and repository structure

The planned public repository will contain the following structure:

```text
ENSANUT2022-Cardiometabolic-Risk/
├── README.md
├── data/
│   └── README.md
├── src/
│   └── Ensanut_AP6001.R
├── report/
│   ├── ENSANUT2022_AP6001_report.qmd
│   └── ENSANUT2022_AP6001_report.pdf
├── outputs/
│   ├── primary_model_results.csv
│   └── ENSANUT_R_sessionInfo.txt
└── .gitignore
```

The principal project files are:

| File or directory | Role |
|---|---|
| `Ensanut_AP6001.R` | Main R analysis, data-quality audits, and adult-weight sensitivity analysis |
| `ENSANUT2022_AP6001_report.qmd` | Quarto source for the research report |
| `ENSANUT2022_AP6001_report.pdf` | Rendered report generated from the Quarto source |
| `ENSANUT2022_cleaned_analytical.csv.gz` | Compressed prepared analytical dataset consumed by the main R script; redistribution is subject to the applicable data-use permissions. |
| `ensadul2022_entrega_w.dta` | Original adult ENSANUT public-use microdata; not included in this repository |
| `ensaantro2022_entrega_w.dta` | Original anthropometry ENSANUT public-use microdata; not included in this repository |
| `SALUD/NUTRICION/DICCIONARIOS/` | Supporting documentation and exploratory materials; individual files must be reviewed before publication |
| `ensanut2022.R` | Introductory R exercises; not part of the principal analytical workflow |

The existing `test_model.py` and older exploratory materials are not identified as steps required to reproduce the reported R model.

Individual-level survey data, restricted analytical datasets, and any other files containing potentially identifiable or confidential information will not be published.

## 8. Reproducing the current analysis

### Requirements

- R and RStudio, or an R command-line installation.
- Quarto, to render the report.
- R packages `survey`, `dplyr`, and `knitr`. The report may require additional packages specified in its setup chunk.

Install the core packages if needed:

```r
install.packages(c("survey", "dplyr", "knitr"))
```

### Run the R analysis

The current `Ensanut_AP6001.R` script defines the project directory using an absolute path on the original author's computer. If running the project elsewhere, update `base_dir` in the script to the local project directory before execution.

From the project root in R:

```r
source("Ensanut_AP6001.R")
```

The prepared analytical dataset must be available locally in the repository root directory:

ENSANUT2022_cleaned_analytical.csv.gz

The R script reads the compressed CSV directly; manual decompression is not required.

Researchers will need to obtain the original ENSANUT datasets through the appropriate official channels and prepare the analytical dataset before running the model.

### Render the report

From the project root in a terminal:

```bash
quarto render ENSANUT2022_AP6001_report.qmd
```

The report's data path and the R script's output paths should be checked when running from a different working directory. Confirm that the rendered report corresponds to the current version of the R script and analytical dataset before submission.

## 9. Scope and limitations

The primary outcome in the final reported analysis is hypertension alone. A composite cardiometabolic outcome was part of the initial research motivation, but it was not evaluated in the reported final analysis. No composite-outcome estimate or complete-case sample size is claimed.

Other limitations include:

- The cross-sectional observational design does not establish causality.
- Diabetes and dyslipidemia have not yet been analyzed as outcomes.
- The operational definition of hypertension combines self-reported diagnosis and measured blood pressure.
- Possible residual confounding and measurement or classification error in anthropometric variables.
- Restrictions imposed by the available survey variables and analytical sample.

Exploratory data-quality audits, including the investigation of discrepancies between age variables, are retained in the R script to document checks performed during analysis. They should be distinguished from the primary model and its reported sensitivity analysis.

## 10. Future research

This exploratory analysis focuses on hypertension as the cardiometabolic outcome. Further work will extend the analysis to diabetes and dyslipidemia as separate outcomes (estimands), examining their associations with obesity and abdominal-obesity profiles.

This subsequent analysis will broaden the assessment of cardiometabolic risk among Mexican adults using ENSANUT data.

## 11. Reproducibility status

The current project supports reproduction of the reported model **provided that the prepared analytical CSV is available and the project paths are configured correctly**.

Full end-to-end reproducibility requires:

- Adding and documenting the verified data-preparation workflow from the original ENSANUT microdata.
- Documenting variable definitions, data merges, exclusions, and missing-data handling.
- Checking all report outputs against a fresh execution of the analysis.
- Recording the R and package environment, for example, with `sessionInfo()`.

The repository documents an exploratory academic research project using ENSANUT 2022. It is not a clinical decision-support tool and does not provide individual medical recommendations.
