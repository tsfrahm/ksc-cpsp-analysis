## 01_core_setup.R
## Core data load + cleaning + helper functions for CPSP project

# ---------------------- Packages ---------------------- #
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(here)

# ---------------------- Paths & Options ---------------------- #
data_path   <- here::here("data", "chronic_pain_dates.xlsx")
pain_thresh <- 2      # threshold for "has pain" (Likert ≥ this)
min_prop    <- 0.75   # minimum proportion of items required to rescale summed scores (≥75%)

# ---------------------- Read Data ---------------------- #
df <- read_excel(data_path)

# ---------------------- Column Names ---------------------- #
colnames(df) <- c(
  "id","surgery_date","start_time","completion_time","email","name","patient_number",
  "age","sex","education","religion","marital_status","district","sub_county","employed",
  "occupation","manual_labor","manual_labor_kind",
  # Mesh sensation items (1–5)
  "pain_lying_mesh","pain_lying","pain_bending_mesh","pain_bending","limitation_bending",
  "pain_sitting_mesh","pain_sitting","limitation_sitting","pain_adl_mesh","pain_adl",
  "limitation_adl","pain_cough_mesh","pain_cough","limitation_cough","pain_walk_mesh",
  "pain_walk","limitation_walk","pain_stairs_mesh","pain_stairs","limitation_stairs",
  "pain_exercise_mesh","pain_exercise","limitation_exercise",
  # Preop & periop
  "preop_hernia_pain_2w","preop_chronic_pain_other","preop_opioids_self",
  "preop_opioids_duration_months","malignancy","bmi","other_disease","other_disease_which",
  "smoke","smoke_freq","alcohol","alcohol_freq","substance_other","substance_other_which",
  "preop_opioids_prescribed","intraop_complication","intraop_complication_what",
  "preop_chronic_pain_any","preop_painkillers_before_surgery","anesthesia_type",
  "analgesics_type","analgesic_duration_days","surgery_duration_minutes",
  # RAH & discharge
  "rah_pain_2w","rah_pain_1m","incision_length_prev_surg_cm",
  "discharge_pain_education","followed_2w","followed_1m","discharge_anesth_visit",
  "periop_regional","preventive_preop","preventive_postop"
)

# ---------------------- Variable Sets ---------------------- #
pain_vars <- c(
  "pain_lying","pain_bending","pain_sitting","pain_adl",
  "pain_cough","pain_walk","pain_stairs","pain_exercise"
)
mesh_vars <- c(
  "pain_lying_mesh","pain_bending_mesh","pain_sitting_mesh","pain_adl_mesh",
  "pain_cough_mesh","pain_walk_mesh","pain_stairs_mesh","pain_exercise_mesh"
)
limit_vars <- c(
  "limitation_bending","limitation_sitting","limitation_adl",
  "limitation_cough","limitation_walk","limitation_stairs","limitation_exercise"
)

pain_labels <- c(
  pain_lying   = "Lying down",
  pain_bending = "Bending",
  pain_sitting = "Sitting up",
  pain_adl     = "Daily activities (ADLs)",
  pain_cough   = "Coughing/deep breathing",
  pain_walk    = "Walking",
  pain_stairs  = "Walking up stairs",
  pain_exercise= "Exercising"
)
mesh_labels <- c(
  pain_lying_mesh   = "Lying down",
  pain_bending_mesh = "Bending",
  pain_sitting_mesh = "Sitting up",
  pain_adl_mesh     = "Daily activities (ADLs)",
  pain_cough_mesh   = "Coughing/deep breathing",
  pain_walk_mesh    = "Walking",
  pain_stairs_mesh  = "Walking up stairs",
  pain_exercise_mesh= "Exercising"
)
limit_labels <- c(
  limitation_bending  = "Bending",
  limitation_sitting  = "Sitting up",
  limitation_adl      = "Daily activities (ADLs)",
  limitation_cough    = "Coughing/deep breathing",
  limitation_walk     = "Walking",
  limitation_stairs   = "Walking up stairs",
  limitation_exercise = "Exercising"
)

# ---------------------- Data Cleaning ---------------------- #
date_cols <- c("surgery_date","start_time","completion_time")
num_cols  <- c(
  "age", mesh_vars, pain_vars, limit_vars, "bmi",
  "preop_opioids_duration_months","analgesic_duration_days",
  "surgery_duration_minutes","incision_length_prev_surg_cm"
)
char_cols <- c(
  "patient_number","email","name","sex","education","religion","marital_status","district","sub_county",
  "employed","occupation","manual_labor","manual_labor_kind",
  "preop_hernia_pain_2w","preop_chronic_pain_other","preop_opioids_self",
  "other_disease","other_disease_which","smoke","smoke_freq","alcohol","alcohol_freq",
  "substance_other","substance_other_which","preop_opioids_prescribed",
  "intraop_complication","intraop_complication_what","preop_chronic_pain_any",
  "preop_painkillers_before_surgery","anesthesia_type","analgesics_type",
  "rah_pain_2w","rah_pain_1m","discharge_pain_education","followed_2w","followed_1m",
  "discharge_anesth_visit","periop_regional","preventive_preop","preventive_postop","malignancy"
)

# Dates
df <- df %>%
  mutate(across("surgery_date", ymd)) %>%
  mutate(across(date_cols[2:3], ymd_hms))

# Numerics (handle "N/A")
df[num_cols] <- lapply(df[num_cols], function(x) {
  x[x == "N/A"] <- NA
  as.numeric(x)
})

# Lowercase character fields
df <- df %>%
  mutate(across(all_of(char_cols), ~ ifelse(is.na(.x), NA, tolower(.x))))

# Follow-up flags as logical
df <- df %>%
  mutate(
    followed_2w = case_when(followed_2w == "yes" ~ TRUE,
                            followed_2w == "no"  ~ FALSE,
                            TRUE ~ NA),
    followed_1m = case_when(followed_1m == "yes" ~ TRUE,
                            followed_1m == "no"  ~ FALSE,
                            TRUE ~ NA)
  ) %>%
  ungroup()

# ---------------------- Helper Functions ---------------------- #

# Summed/rescaled domain score (used for pain, mesh, limit)
sum_rescaled <- function(data, vars, min_prop = 0.75) {
  n_items   <- length(vars)
  min_items <- ceiling(n_items * min_prop)
  data %>%
    mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(.x))),
           .n_answered = rowSums(!is.na(pick(all_of(vars)))),
           .raw_sum    = rowSums(pick(all_of(vars)), na.rm = TRUE),
           .sum_scaled = ifelse(.n_answered >= min_items,
                                round(.raw_sum * n_items / .n_answered),
                                NA_real_)) %>%
    pull(.sum_scaled)
}

# Simple categorical summary (for tables if needed)
cat_summary <- function(data, var, label = NULL) {
  var  <- rlang::ensym(var)
  name <- ifelse(is.null(label), rlang::as_name(var), label)
  x    <- dplyr::pull(data, !!var)
  tot  <- sum(!is.na(x))
  tibble::tibble(Level = x) %>%
    count(Level, name = "n") %>%
    mutate(Variable = name, prop = ifelse(!is.na(Level) & tot > 0, n / tot, NA_real_)) %>%
    select(Variable, Level, n, prop)
}

num_summary <- function(data, var, label = NULL) {
  var  <- rlang::ensym(var)
  name <- ifelse(is.null(label), rlang::as_name(var), label)
  v    <- dplyr::pull(data, !!var)
  tibble::tibble(
    Variable = name, Level = "Mean (SD)", n = sum(!is.na(v)), prop = NA_real_,
    summary  = sprintf("%.1f (%.1f)", mean(v, na.rm = TRUE), sd(v, na.rm = TRUE))
  )
}

binary_compare <- function(var, label) {
  vnm <- rlang::as_name(rlang::ensym(var))
  tmp <- df %>%
    select(manual_labor, .y = all_of(vnm)) %>%
    filter(!is.na(.y)) %>%
    mutate(manual_labor = as.factor(manual_labor))
  sumtab <- tmp %>%
    group_by(manual_labor) %>%
    summarise(n = n(), n_yes = sum(.y, na.rm = TRUE), prop = n_yes / n, .groups = "drop") %>%
    mutate(Variable = label, value = paste0(n_yes, "/", n, " (", round(100*prop, 1), "%)")) %>%
    select(Variable, manual_labor, n, value, prop)
  xt <- with(tmp, table(manual_labor, .y))
  pval <- NA_real_
  if (nrow(xt) >= 2 && ncol(xt) == 2 && sum(xt) > 0 && all(rowSums(xt) > 0) && all(colSums(xt) > 0)) {
    pval <- if (all(xt >= 5)) suppressWarnings(chisq.test(xt)$p.value) else suppressWarnings(fisher.test(xt)$p.value)
  }
  sumtab$p_value <- pval
  sumtab
}

###############################################################################
# HELPER FUNCTIONS FOR CPSP PROJECT
# These support:
# - Summed/rescaled CCS scores
# - Likert distribution plots
# - Item-level summaries
# - Crosstab logic (used in pain, mesh, limitation scripts)
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---------------------------------------------------------------------------
# 1. sum_rescaled()
# ---------------------------------------------------------------------------
# Computes a summed and *rescaled* CCS domain score, enforcing the 75%
# item-completion rule. If fewer than min_prop of items are answered,
# score = NA (excluded from domain analysis).
# ---------------------------------------------------------------------------

sum_rescaled <- function(data, vars, min_prop = 0.75) {
  n_items   <- length(vars)
  min_items <- ceiling(n_items * min_prop)
  
  data %>%
    mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(
      .n_answered = rowSums(!is.na(pick(all_of(vars)))),
      .raw_sum    = rowSums(pick(all_of(vars)), na.rm = TRUE),
      .sum_scaled = ifelse(.n_answered >= min_items,
                           round(.raw_sum * n_items / .n_answered),
                           NA_real_)
    ) %>%
    pull(.sum_scaled)
}

# ---------------------------------------------------------------------------
# 2. plot_likert_hist()
# ---------------------------------------------------------------------------
# Creates Likert bar plots (Likert 1–5) for each item in a domain.
# Matches the style used in your Google Slides figures.
# ---------------------------------------------------------------------------

plot_likert_hist <- function(data, vars, labels = NULL, title = "Likert distributions") {
  longdat <- data %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "score") %>%
    mutate(
      score = suppressWarnings(as.numeric(score)),
      variable_label = if (!is.null(labels)) labels[variable] else variable
    )
  
  plotdat <- longdat %>%
    group_by(variable_label, score) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(variable_label) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  ggplot(plotdat, aes(x = factor(score), y = prop, fill = factor(score))) +
    geom_col(color = "black") +
    facet_wrap(~ variable_label, ncol = 2) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_brewer(palette = "Blues", name = "Score") +
    labs(title = title, x = "Likert score", y = "Proportion") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# ---------------------------------------------------------------------------
# 3. item_summary()
# ---------------------------------------------------------------------------
# Generates per-item summaries:
# - n_nonmiss: number of respondents
# - n_mild_or_worse: number scoring ≥ threshold
# - pct: percent scoring ≥ threshold
# This is what produced statements like:
#   "Most frequent pain was mild pain with daily activities (8.6%)..."
# ---------------------------------------------------------------------------

item_summary <- function(data, vars, labels, threshold = 2, domain = "Domain") {
  
  data %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "item", values_to = "score") %>%
    mutate(
      score = suppressWarnings(as.numeric(score)),
      item_label = labels[item],
      mild_or_worse = ifelse(!is.na(score) & score >= threshold, 1, 0)
    ) %>%
    group_by(item, item_label) %>%
    summarise(
      n_nonmiss = sum(!is.na(score)),
      n_mild_or_worse = sum(mild_or_worse, na.rm = TRUE),
      pct = round(100 * n_mild_or_worse / n_nonmiss, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(pct))
}

# ---------------------------------------------------------------------------
# 4. Crosstab function for binary outcomes
# ---------------------------------------------------------------------------
# Safe wrapper around Fisher and Chi-squared tests.
# Avoids errors for tables <2x2.
# Used by:
# - 06_cpsp_protocol_pipeline.R
# - 07_mesh_limit_crosstabs.R
# ---------------------------------------------------------------------------

crosstab_binary_outcome <- function(outcome_var, predictor_var, data) {
  
  out_nm  <- outcome_var
  pred_nm <- predictor_var
  
  tmp <- data %>%
    select(all_of(c(out_nm, pred_nm))) %>%
    rename(
      outcome   = all_of(out_nm),
      predictor = all_of(pred_nm)
    ) %>%
    filter(!is.na(outcome), !is.na(predictor))
  
  if (nrow(tmp) == 0) {
    return(tibble(
      outcome       = out_nm,
      variable      = pred_nm,
      test          = NA_character_,
      p_value       = NA_real_,
      any_zero_cell = NA,
      note          = "No non-missing data"
    ))
  }
  
  tab <- table(tmp$predictor, tmp$outcome)
  
  # Need at least 2x2 with nonzero total
  if (nrow(tab) < 2 || ncol(tab) < 2 || sum(tab) == 0) {
    return(tibble(
      outcome       = out_nm,
      variable      = pred_nm,
      test          = NA_character_,
      p_value       = NA_real_,
      any_zero_cell = NA,
      note          = "Table not 2x2 (insufficient variation)"
    ))
  }
  
  any_zero <- any(tab == 0)
  
  # Choose test
  if (all(tab >= 5)) {
    test_obj  <- suppressWarnings(chisq.test(tab))
    test_name <- "Chi-squared"
  } else {
    test_obj  <- suppressWarnings(fisher.test(tab))
    test_name <- "Fisher"
  }
  
  tibble(
    outcome       = out_nm,
    variable      = pred_nm,
    test          = test_name,
    p_value       = unname(test_obj$p.value),
    any_zero_cell = any_zero,
    note          = ifelse(any_zero, "Zero cell(s) present", "No zero cells")
  )
}

###############################################################################
# END OF HELPER FUNCTIONS
###############################################################################

cat("Core setup complete. df, pain_vars, mesh_vars, limit_vars, and helper functions are ready.\n")