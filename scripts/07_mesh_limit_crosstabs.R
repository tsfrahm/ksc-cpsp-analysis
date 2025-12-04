## 07_mesh_limit_crosstabs.R
## Cross-tabulations for mesh sensation and functional limitation
## using the same predictor set and logic as CPSP.

library(dplyr)
library(purrr)
library(readr)
library(here)

# -------------------------------------------------------------------
# 0. Load core setup and compute domain scores / binary flags
# -------------------------------------------------------------------
source(here::here("scripts", "01_core_setup.R"))

# Thresholds
thr_pain  <- 16
thr_mesh  <- 16
thr_limit <- 14   # 40% of max 35 given 7 limitation items in the Excel

df_all <- df %>%
  mutate(
    # summed domain scores (rescaled if ≥75% answered)
    sum_pain  = sum_rescaled(cur_data_all(), pain_vars,  min_prop = min_prop),
    sum_mesh  = sum_rescaled(cur_data_all(), mesh_vars,  min_prop = min_prop),
    sum_limit = sum_rescaled(cur_data_all(), limit_vars, min_prop = min_prop),
    # CPSP
    cpsp = case_when(
      is.na(sum_pain)      ~ NA_character_,
      sum_pain >= thr_pain ~ "yes",
      TRUE                 ~ "no"
    ),
    # Mesh sensation (clinically significant)
    mesh_positive = case_when(
      is.na(sum_mesh)      ~ NA_character_,
      sum_mesh >= thr_mesh ~ "yes",
      TRUE                 ~ "no"
    ),
    # Functional limitation (clinically significant)
    limit_positive = case_when(
      is.na(sum_limit)       ~ NA_character_,
      sum_limit >= thr_limit ~ "yes",
      TRUE                   ~ "no"
    )
  )

# -------------------------------------------------------------------
# 1. Derive categorical predictors: age_cat, followup_cat
# -------------------------------------------------------------------
df_all <- df_all %>%
  mutate(
    age_cat = case_when(
      age < 50                  ~ "<50",
      age >= 50 & age < 65      ~ "50–64",
      age >= 65                 ~ "65+",
      TRUE                      ~ NA_character_
    ),
    followup_days = as.numeric(difftime(start_time, surgery_date, units = "days")),
    followup_cat = case_when(
      is.na(followup_days)      ~ NA_character_,
      followup_days < 120       ~ "<4 months",
      followup_days < 240       ~ "4–8 months",
      TRUE                      ~ "8+ months"
    )
  )

# Same predictor set as CPSP pipeline
predictors_cat <- c(
  "sex",
  "education",
  "manual_labor",
  "employed",
  "age_cat",
  "followup_cat",
  "preop_hernia_pain_2w",
  "preop_chronic_pain_other",
  "preop_opioids_self",
  "malignancy",
  "smoke",
  "alcohol"
)

# -------------------------------------------------------------------
# 2. Generic crosstab helper (string-based, no rlang)
# -------------------------------------------------------------------
crosstab_binary_outcome <- function(outcome_var, predictor_var, data) {
  # outcome_var and predictor_var are plain strings like "mesh_positive", "sex"
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
  
  tab <- table(tmp$predictor, tmp$outcome, dnn = c(pred_nm, out_nm))
  
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

# -------------------------------------------------------------------
# 3. Run crosstabs for mesh_positive
# -------------------------------------------------------------------
mesh_events <- sum(df_all$mesh_positive == "yes", na.rm = TRUE)
mesh_total  <- sum(!is.na(df_all$mesh_positive))

cat("Mesh-positive events: ", mesh_events, " of ", mesh_total,
    " patients (", round(100 * mesh_events / mesh_total, 1), "%)\n\n", sep = "")

mesh_crosstabs <- map_dfr(
  predictors_cat,
  function(pred) crosstab_binary_outcome("mesh_positive", pred, df_all)
) %>%
  arrange(p_value)

cat("Crosstab results: mesh_positive vs predictors\n")
print(mesh_crosstabs, n = nrow(mesh_crosstabs))
cat("\n")

# -------------------------------------------------------------------
# 4. Run crosstabs for limit_positive
# -------------------------------------------------------------------
limit_events <- sum(df_all$limit_positive == "yes", na.rm = TRUE)
limit_total  <- sum(!is.na(df_all$limit_positive))

cat("Limit-positive events: ", limit_events, " of ", limit_total,
    " patients (", round(100 * limit_events / limit_total, 1), "%)\n\n", sep = "")

limit_crosstabs <- map_dfr(
  predictors_cat,
  function(pred) crosstab_binary_outcome("limit_positive", pred, df_all)
) %>%
  arrange(p_value)

cat("Crosstab results: limit_positive vs predictors\n")
print(limit_crosstabs, n = nrow(limit_crosstabs))
cat("\n")

# -------------------------------------------------------------------
# 5. Save outputs
# -------------------------------------------------------------------
if (!dir.exists(here::here("outputs"))) {
  dir.create(here::here("outputs"), recursive = TRUE)
}

write_csv(mesh_crosstabs,
          here::here("outputs", "mesh_crosstab_summary.csv"))
write_csv(limit_crosstabs,
          here::here("outputs", "limit_crosstab_summary.csv"))

cat(
  "Mesh and limitation crosstab summaries saved to:\n",
  "  - outputs/mesh_crosstab_summary.csv\n",
  "  - outputs/limit_crosstab_summary.csv\n"
)