install.packages("survey")
library(survey)
library(dplyr)


# ============================================================
# 1. FILE LOCATION AND DATA IMPORT
# ============================================================

# Define the working directory
base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

# Define the input file
input_file <- file.path(
  base_dir,
  "processed_data",
  "ENSANUT2022_cleaned_analytical.csv"
)

# Verify that the file exists
if (!file.exists(input_file)) {
  stop("ERROR: Input file not found: ", input_file)
}

# Import the dataset
ensanut <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN")
)

# Confirm successful import
cat("File successfully imported.\n")
cat("Number of observations:", nrow(ensanut), "\n")
cat("Number of variables:", ncol(ensanut), "\n")

# Inspect the variables
str(ensanut)


# 2. Import the cleaned analytical dataset

# Check that the required variables exist
required_vars <- c(
  "hypertension",
  "model_category",
  "edad",
  "sexo",
  "estrato",
  "est_sel",
  "upm",
  "ponde_f_anthro",
  "ponde_f_adult"
)

missing_vars <- setdiff(required_vars, names(ensanut))

if (length(missing_vars) > 0) {
  stop(
    paste(
      "Missing variables:",
      paste(missing_vars, collapse = ", ")
    )
  )
}

# Reconstruct the complete-case sample used by the Python model
model_data <- ensanut %>%
  filter(
    !is.na(hypertension),
    !is.na(model_category),
    !is.na(edad),
    !is.na(sexo),
    !is.na(estrato),
    !is.na(est_sel),
    !is.na(upm),
    !is.na(ponde_f_anthro),
    ponde_f_anthro > 0
  ) %>%
  droplevels()

# Convert categorical variables to factors
model_data$model_category <- factor(model_data$model_category)
model_data$sexo <- factor(model_data$sexo)
model_data$estrato <- factor(model_data$estrato)

# Define the reference category
reference <- "Normal | No abdominal obesity"

if (!reference %in% levels(model_data$model_category)) {
  stop("The reference category is not present in the analytical sample.")
}

model_data$model_category <- relevel(
  model_data$model_category,
  ref = reference
)

# Basic sample diagnostics
cat("\n========== ANALYTICAL SAMPLE ==========\n")
cat("Number of adults:", nrow(model_data), "\n")
cat("Number of strata:", n_distinct(model_data$est_sel), "\n")
cat("Number of distinct PSUs:",
    n_distinct(interaction(model_data$est_sel,
                           model_data$upm)), "\n")

cat("\nHypertension outcome:\n")
print(table(model_data$hypertension, useNA = "ifany"))

cat("\nJoint BMI categories:\n")
print(table(model_data$model_category, useNA = "ifany"))

cat("\nSex:\n")
print(table(model_data$sexo))

cat("\nLocality size:\n")
print(table(model_data$estrato))

### 4. ensanut survey desugn
# Confirm that hypertension is coded 0/1
print(table(model_data$hypertension, useNA = "ifany"))

if (!all(na.omit(unique(model_data$hypertension)) %in% c(0, 1))) {
  stop("Hypertension must be coded as 0 and 1.")
}

# Count PSUs within each sampling stratum
psus_per_stratum <- model_data %>%
  group_by(est_sel) %>%
  summarise(
    n_psu = n_distinct(upm),
    .groups = "drop"
  )

cat("\n========== PSU COUNTS PER STRATUM ==========\n")
print(table(psus_per_stratum$n_psu))

cat(
  "Singleton strata:",
  sum(psus_per_stratum$n_psu == 1),
  "\n"
)

# Handle singleton PSUs using the adjustment method
options(survey.lonely.psu = "adjust")

# Declare the survey design
ensanut_design <- svydesign(
  ids = ~upm,
  strata = ~est_sel,
  weights = ~ponde_f_anthro,
  data = model_data,
  nest = TRUE
)

cat("\n========== SURVEY DESIGN ==========\n")
print(ensanut_design)

cat(
  "\nSurvey design degrees of freedom:",
  degf(ensanut_design),
  "\n"
)

####Estimate primary model
# Primary survey-weighted linear probability model
fit_primary <- svyglm(
  hypertension ~
    model_category +
    edad +
    I(edad^2) +
    sexo +
    estrato,
  design = ensanut_design,
  family = gaussian()
)

# Full model summary
cat("\n========== PRIMARY MODEL SUMMARY ==========\n")
print(summary(fit_primary))

# Confidence intervals for all coefficients
cat("\n========== COEFFICIENT CONFIDENCE INTERVALS ==========\n")
print(confint(fit_primary))

###Extract principal comparison
# Identify the coefficient for the principal comparison
comparison <- "Obesity II/III | Abdominal obesity"

coefficient_names <- names(coef(fit_primary))

target_coefficient <- grep(
  comparison,
  coefficient_names,
  fixed = TRUE,
  value = TRUE
)

if (length(target_coefficient) != 1) {
  stop(
    paste(
      "Could not identify exactly one coefficient for:",
      comparison,
      "\nAvailable coefficients:",
      paste(coefficient_names, collapse = "\n")
    )
  )
}

# Extract estimate and standard error
estimate <- coef(fit_primary)[target_coefficient]

standard_error <- sqrt(
  vcov(fit_primary)[target_coefficient, target_coefficient]
)

# Survey-adjusted confidence interval
ci <- confint(
  fit_primary,
  parm = target_coefficient,
  level = 0.95
)

# Report results in percentage points
cat("\n========== PRINCIPAL CONTRAST ==========\n")

cat("Comparison:", comparison, "\n")
cat("Reference:", reference, "\n")

cat(
  "Adjusted prevalence difference:",
  round(as.numeric(estimate) * 100, 2),
  "percentage points\n"
)

cat(
  "Standard error:",
  round(as.numeric(standard_error) * 100, 2),
  "percentage points\n"
)

cat(
  "95% confidence interval: [",
  round(ci[1, 1] * 100, 2),
  ", ",
  round(ci[1, 2] * 100, 2),
  "] percentage points\n",
  sep = ""
)


#####Additional diagnostics and save the results
cat("\n========== WEIGHTED HYPERTENSION PREVALENCE ==========\n")

weighted_prevalence <- svymean(
  ~hypertension,
  design = ensanut_design,
  na.rm = TRUE
)

print(weighted_prevalence)
print(confint(weighted_prevalence))

cat("\n========== FINAL SAMPLE INFORMATION ==========\n")
cat("Adults:", nrow(model_data), "\n")
cat("Strata:", n_distinct(model_data$est_sel), "\n")
cat(
  "PSUs:",
  n_distinct(interaction(model_data$est_sel, model_data$upm)),
  "\n"
)

cat("\n========== MODEL INFORMATION ==========\n")
cat("Weight: ponde_f_anthro\n")
cat("Variance estimation: Taylor linearization\n")
cat("Singleton PSU treatment: adjust\n")
cat("Model: survey-weighted linear probability model\n")


####save in atext file
sink("ENSANUT_survey_verification.txt")

cat("ENSANUT 2022 - SURVEY VERIFICATION\n\n")

cat("ANALYTICAL SAMPLE\n")
cat("Adults:", nrow(model_data), "\n")
cat("Strata:", n_distinct(model_data$est_sel), "\n")
cat("PSUs:",
    n_distinct(interaction(model_data$est_sel, model_data$upm)),
    "\n\n")

cat("SURVEY DESIGN\n")
print(ensanut_design)

cat("\nPRIMARY MODEL\n")
print(summary(fit_primary))

cat("\nCOEFFICIENT CONFIDENCE INTERVALS\n")
print(confint(fit_primary))

cat("\nPRINCIPAL CONTRAST\n")
cat("Estimate:", estimate, "\n")
cat("Standard error:", standard_error, "\n")
cat("95% CI:\n")
print(ci)

cat("\nWEIGHTED HYPERTENSION PREVALENCE\n")
print(weighted_prevalence)
print(confint(weighted_prevalence))

sink()


####verify directory
getwd()

####Coefficient & SE


# ============================================================
# PRIMARY MODEL: PRINCIPAL RESULT
# ============================================================

target <- "model_categoryObesity II/III | Abdominal obesity"

if (!target %in% names(coef(fit_primary))) {
  stop("Primary contrast not found in the fitted model.")
}

primary_coefficient <- unname(coef(fit_primary)[target])

primary_se <- sqrt(
  vcov(fit_primary)[target, target]
)

primary_ci <- confint(
  fit_primary,
  parm = target
)

#####Change principal model
exists("fit_primary")
exists("design_anthro")
exists("model_data")
exists("target")
exists("primary_coefficient")

primary_coefficient <- unname(coef(fit_primary)[target])

primary_se <- sqrt(vcov(fit_primary)[target, target])

primary_ci <- confint(fit_primary, parm = target)

primary_df <- degf(design_anthro)

primary_coefficient
primary_se
primary_ci
primary_df

exists("primary_coefficient")
exists("primary_results")
file.exists(file.path(base_dir, "outputs", "primary_model_results.csv"))



primary_df <- degf(design_anthro)

cat("\n====================================\n")
cat("PRINCIPAL MODEL RESULT\n")
cat("====================================\n")

cat("Analytical sample:", nrow(model_data), "\n")

cat("Coefficient:", primary_coefficient, "\n")

cat("Coefficient (percentage points):",
    primary_coefficient * 100, "\n")

cat("Standard error:", primary_se, "\n")

cat("Standard error (percentage points):",
    primary_se * 100, "\n")

cat("95% confidence interval:\n")
print(primary_ci)

cat("95% confidence interval (percentage points):\n")
print(primary_ci * 100)

cat("Survey degrees of freedom:", primary_df, "\n")


# ============================================================
# SAVE REPRODUCIBILITY OUTPUTS
# ============================================================

output_dir <- file.path(base_dir, "outputs")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

saveRDS(
  fit_primary,
  file.path(output_dir, "ENSANUT_survey_primary_model.rds")
)

writeLines(
  capture.output(sessionInfo()),
  file.path(output_dir, "ENSANUT_R_sessionInfo.txt")
)

primary_results <- data.frame(
  outcome = "hypertension",
  contrast = target,
  estimate = primary_coefficient,
  standard_error = primary_se,
  ci_lower = primary_ci[1],
  ci_upper = primary_ci[2],
  survey_df = primary_df,
  sample_size = nrow(model_data)
)

write.csv(
  primary_results,
  file.path(output_dir, "primary_model_results.csv"),
  row.names = FALSE
)


####Auditing the sample
# ============================================================
# STEP 1. AUDIT THE CURRENT ANALYTICAL SAMPLE
# ============================================================

cat("\n========== SAMPLE AUDIT ==========\n")

# 1. Sample size
cat("\nNumber of observations:", nrow(model_data), "\n")

# 2. Age distribution
cat("\nAGE DISTRIBUTION\n")
print(summary(model_data$edad))

# 3. Sex distribution
cat("\nSEX DISTRIBUTION\n")
print(table(model_data$sexo, useNA = "ifany"))

# 4. Locality category distribution
cat("\nLOCALITY CATEGORY DISTRIBUTION\n")
print(table(model_data$estrato, useNA = "ifany"))

# 5. Joint BMI / abdominal-obesity categories
cat("\nJOINT EXPOSURE CATEGORIES\n")
print(table(model_data$model_category, useNA = "ifany"))

# 6. Hypertension by joint exposure category
cat("\nHYPERTENSION BY EXPOSURE CATEGORY\n")
print(
  with(
    model_data,
    table(model_category, hypertension, useNA = "ifany")
  )
)

# 7. Survey weight
cat("\nANTHROPOMETRY WEIGHT SUMMARY\n")
print(summary(model_data$ponde_f_anthro))

cat("\nNumber of non-positive weights:\n")
print(sum(model_data$ponde_f_anthro <= 0, na.rm = TRUE))

# 8. Survey design structure
cat("\nSURVEY DESIGN\n")
cat("Strata:", n_distinct(model_data$est_sel), "\n")
cat("PSUs:", n_distinct(interaction(
  model_data$est_sel,
  model_data$upm
)), "\n")
cat("Design degrees of freedom:", degf(ensanut_design), "\n")

cat("\n========== END OF AUDIT ==========\n")

# ============================================================
# STEP 2. VERIFY VARIABLE DEFINITIONS AND AVAILABILITY
# ============================================================

cat("\n========== VARIABLE AUDIT ==========\n")

# Check whether alternative variables exist
variables_to_check <- c(
  "edad", "aedad",
  "sexo", "asexo",
  "h0302",
  "hypertension",
  "model_category",
  "joint_obesity",
  "ponde_f_anthro",
  "ponde_f_adult"
)

for (v in variables_to_check) {
  cat(
    v, ":",
    if (v %in% names(ensanut)) "AVAILABLE" else "NOT AVAILABLE",
    "\n"
  )
}

# Inspect the relevant variables
cat("\nDATA STRUCTURE\n")

available <- intersect(variables_to_check, names(ensanut))

str(ensanut[, available, drop = FALSE])

cat("\n========== END OF VARIABLE AUDIT ==========\n")

# ============================================================
# STEP 3. COMPARE ALTERNATIVE VARIABLE DEFINITIONS
# ============================================================

cat("\n========== COMPARISON OF VARIABLES ==========\n")

# 1. AGE: edad versus aedad
cat("\n1. AGE COMPARISON\n")

cat("Different age values:",
    sum(model_data$edad != model_data$aedad, na.rm = TRUE), "\n")

cat("Missing edad:",
    sum(is.na(model_data$edad)), "\n")

cat("Missing aedad:",
    sum(is.na(model_data$aedad)), "\n")

cat("Age differences by value:\n")
print(table(
  model_data$edad - model_data$aedad,
  useNA = "ifany"
))

# 2. SEX: sexo versus asexo and h0302
cat("\n2. SEX COMPARISON\n")

cat("sexo versus asexo differences:",
    sum(model_data$sexo != model_data$asexo, na.rm = TRUE), "\n")

cat("sexo versus h0302 differences:",
    sum(model_data$sexo != model_data$h0302, na.rm = TRUE), "\n")

cat("\nsexo versus asexo cross-tabulation:\n")
print(table(model_data$sexo, model_data$asexo, useNA = "ifany"))

cat("\nsexo versus h0302 cross-tabulation:\n")
print(table(model_data$sexo, model_data$h0302, useNA = "ifany"))

# 3. EXPOSURE: model_category versus joint_obesity
cat("\n3. EXPOSURE CATEGORY COMPARISON\n")

cat("model_category distribution:\n")
print(table(model_data$model_category, useNA = "ifany"))

cat("\njoint_obesity distribution:\n")
print(table(model_data$joint_obesity, useNA = "ifany"))

cat("\nCross-tabulation:\n")
print(table(
  model_data$model_category,
  model_data$joint_obesity,
  useNA = "ifany"
))

# 4. WEIGHTS: anthropometry versus adult
cat("\n4. WEIGHT COMPARISON\n")

cat("Anthropometry weight summary:\n")
print(summary(model_data$ponde_f_anthro))

cat("\nAdult weight summary:\n")
print(summary(model_data$ponde_f_adult))

cat("\nRatio: anthropometry weight / adult weight\n")
print(summary(
  model_data$ponde_f_anthro / model_data$ponde_f_adult
))

# 5. AGE ELIGIBILITY
cat("\n5. AGE ELIGIBILITY\n")

cat("Adults aged 20+ according to edad:",
    sum(model_data$edad >= 20, na.rm = TRUE), "\n")

cat("Adults aged 20+ according to aedad:",
    sum(model_data$aedad >= 20, na.rm = TRUE), "\n")

cat("\n========== END OF COMPARISON ==========\n")

# ============================================================
# STEP 4. INVESTIGATE AGE DISCREPANCIES
# ============================================================

cat("\n========== AGE DISCREPANCY AUDIT ==========\n")

# 1. Show records with different age values
age_diff <- model_data[
  model_data$edad != model_data$aedad,
  c("FOLIO_INT", "edad", "aedad")
]

cat("\nNumber of records with different ages:",
    nrow(age_diff), "\n")

cat("\nLargest age discrepancies:\n")

age_diff$age_difference <- age_diff$edad - age_diff$aedad

print(
  age_diff[
    order(-abs(age_diff$age_difference)),
  ][1:min(20, nrow(age_diff)), ]
)

# 2. Examine age ranges
cat("\nSummary of edad:\n")
print(summary(model_data$edad))

cat("\nSummary of aedad:\n")
print(summary(model_data$aedad))

# 3. Identify records classified differently
# around the age-60 anthropometric threshold

cat("\nAGE GROUP COMPARISON (20-59 versus 60+):\n")

age_group_edad <- ifelse(model_data$edad < 60, "20-59", "60+")
age_group_aedad <- ifelse(model_data$aedad < 60, "20-59", "60+")

print(table(
  age_group_edad,
  age_group_aedad
))

# 4. Inspect the extreme age discrepancy
cat("\nRecords with age discrepancy greater than 50 years:\n")

print(
  age_diff[
    abs(age_diff$age_difference) > 50,
  ]
)

cat("\n========== END OF AGE AUDIT ==========\n")

# ============================================================
# STEP 5. QUANTIFY AGE DISCREPANCIES
# ============================================================

cat("\n========== AGE DIFFERENCE DISTRIBUTION ==========\n")

# Absolute age differences
age_diff$absolute_difference <- abs(
  age_diff$age_difference
)

cat("\nSummary of absolute differences:\n")
print(summary(age_diff$absolute_difference))

cat("\nFrequency of absolute differences:\n")
print(table(age_diff$absolute_difference))

cat("\nNumber with differences greater than 1 year:\n")
print(sum(age_diff$absolute_difference > 1))

cat("\nNumber with differences greater than 5 years:\n")
print(sum(age_diff$absolute_difference > 5))

cat("\nNumber with differences greater than 10 years:\n")
print(sum(age_diff$absolute_difference > 10))

# Inspect the extreme observation in the original imported data
cat("\nEXTREME OBSERVATION IN ORIGINAL DATA:\n")

print(
  ensanut[
    ensanut$FOLIO_INT == "2022_21147020_01",
    c("FOLIO_INT", "edad", "aedad", "sexo", "asexo", "h0302"),
    drop = FALSE
  ]
)

cat("\n========== END OF AGE DIFFERENCE AUDIT ==========\n")

# ============================================================
# STEP 6. IDENTIFY ORIGINAL AGE-RELATED VARIABLES
# ============================================================

cat("\n========== AGE-RELATED VARIABLES ==========\n")

# Search variable names for possible age or birth-date information
age_variables <- names(ensanut)[
  grepl(
    "edad|nac|fecha|anio|año|birth",
    names(ensanut),
    ignore.case = TRUE
  )
]

cat("\nPotential age-related variables:\n")
print(age_variables)

# Inspect the extreme observation
cat("\nEXTREME OBSERVATION:\n")

extreme <- ensanut[
  ensanut$FOLIO_INT == "2022_21147020_01",
  ,
  drop = FALSE
]

print(
  extreme[, age_variables, drop = FALSE]
)

# Inspect the eight observations with discrepancies > 10 years
large_diff_ids <- age_diff$FOLIO_INT[
  age_diff$absolute_difference > 10
]

cat("\nOBSERVATIONS WITH AGE DIFFERENCES > 10 YEARS:\n")

print(
  ensanut[
    ensanut$FOLIO_INT %in% large_diff_ids,
    c("FOLIO_INT", age_variables),
    drop = FALSE
  ]
)

cat("\n========== END OF AGE SOURCE AUDIT ==========\n")


# ============================================================
# STEP 7. SENSITIVITY ANALYSIS: ADULT WEIGHT
# ============================================================

# Create alternative survey design
design_adult <- svydesign(
  ids = ~upm,
  strata = ~est_sel,
  weights = ~ponde_f_adult,
  data = model_data,
  nest = TRUE
)

# Fit identical model using adult weight
fit_adult <- svyglm(
  hypertension ~ model_category + edad + I(edad^2) +
    sexo + estrato,
  design = design_adult,
  family = gaussian()
)

# Principal comparison
target <- "model_categoryObesity II/III | Abdominal obesity"

cat("\n========== WEIGHT SENSITIVITY ==========\n")

cat("\nANTHROPOMETRY WEIGHT (PRIMARY)\n")

cat("Coefficient:",
    coef(fit_primary)[target], "\n")

cat("Standard error:",
    sqrt(vcov(fit_primary)[target, target]), "\n")

print(confint(fit_primary, parm = target))

cat("\nADULT WEIGHT (SENSITIVITY)\n")

cat("Coefficient:",
    coef(fit_adult)[target], "\n")

cat("Standard error:",
    sqrt(vcov(fit_adult)[target, target]), "\n")

print(confint(fit_adult, parm = target))

cat("\n========== END OF WEIGHT SENSITIVITY ==========\n")


####Missing values

cat("PRIMARY MODEL SAMPLE:", nrow(model_data), "\n")

cat("\nMISSING VALUES IN MODEL VARIABLES:\n")

vars <- c(
  "hypertension",
  "model_category",
  "edad",
  "sexo",
  "estrato",
  "upm",
  "est_sel",
  "ponde_f_anthro",
  "ponde_f_adult"
)

print(colSums(is.na(model_data[, vars])))

cat("\nCOMPLETE CASES:\n")
print(sum(complete.cases(model_data[, vars])))


cat("MODEL CATEGORY COUNTS:\n")
print(table(model_data$model_category))

cat("\nJOINT OBESITY COUNTS:\n")
print(table(model_data$joint_obesity))


base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

list.files(
  base_dir,
  recursive = TRUE
)


base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

# 1. Inspect the README
cat(readLines(file.path(base_dir, "README.md")),
    sep = "\n")

# 2. Check the sizes and dates of the R scripts
file.info(file.path(
  base_dir,
  c("Ensanut_AP6001.R", "ensanut2022.R")
))[, c("size", "mtime")]

base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

# 1. README
cat("\n========== README.md ==========\n")
cat(readLines(file.path(base_dir, "README.md")),
    sep = "\n")

# 2. Main script: first 100 lines
cat("\n========== Ensanut_AP6001.R: first 100 lines ==========\n")
x <- readLines(file.path(base_dir, "Ensanut_AP6001.R"))
cat(sprintf("%3d: %s", seq_len(min(100, length(x))), x[1:min(100, length(x))]),
    sep = "\n")

# 3. Identify the main sections and important operations
cat("\n========== Script structure ==========\n")
grep(
  "^(#|##|###)|read_csv|read_dta|write_csv|write\\.csv|svyglm|svydesign|ggplot|ggsave|write\\.csv|write\\.dta",
  x,
  value = TRUE
)

cat(readLines(file.path(base_dir, "README.md")), sep = "\n")

x <- readLines(file.path(base_dir, "Ensanut_AP6001.R"))

cat(
  sprintf("%3d: %s", 1:min(120, length(x)), x[1:min(120, length(x))]),
  sep = "\n"
)

if (length(x) > 120) {
  cat(
    sprintf(
      "%3d: %s",
      121:length(x),
      x[121:length(x)]
    ),
    sep = "\n"
  )
}

base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

# Total number of lines in the main R script
x <- readLines(file.path(base_dir, "Ensanut_AP6001.R"))
cat("R script lines:", length(x), "\n")

# Last 80 lines of the R script
cat(
  sprintf(
    "%3d: %s",
    (max(1, length(x) - 79)):length(x),
    tail(x, 80)
  ),
  sep = "\n"
)

# Files and folders at the project root
cat("\nPROJECT FILES:\n")
print(list.files(base_dir, all.files = FALSE))

# Check whether the README contains references to the old Python project
readme <- readLines(file.path(base_dir, "README.md"))
cat("\nREADME final 30 lines:\n")
cat(tail(readme, 30), sep = "\n")

base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

x <- readLines(file.path(base_dir, "Ensanut_AP6001.R"))

cat("R script lines:", length(x), "\n\n")

cat("LAST 180 LINES OF MAIN SCRIPT:\n")
cat(
  sprintf(
    "%3d: %s",
    (max(1, length(x) - 179)):length(x),
    tail(x, 180)
  ),
  sep = "\n"
)

base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

x <- readLines(file.path(base_dir, "ensanut2022.R"))

cat("Lines in ensanut2022.R:", length(x), "\n\n")

cat("FIRST 100 LINES:\n")
cat(
  sprintf("%3d: %s", seq_len(min(100, length(x))), head(x, 100)),
  sep = "\n"
)

base_dir <- "/Users/amadoespinosa/Documents/ENSANUT2022"

cat("FILES IN processed_data:\n")
print(list.files(
  file.path(base_dir, "processed_data"),
  all.files = TRUE
))

cat("\nFILES IN SALUD/NUTRICION/DICCIONARIOS:\n")
print(list.files(
  file.path(base_dir, "SALUD", "NUTRICION", "DICCIONARIOS"),
  all.files = FALSE
))

cat("\nFIRST 40 LINES OF README:\n")
readme <- readLines(file.path(base_dir, "README.md"))
cat(head(readme, 40), sep = "\n")


