## 06_cpsp_protocol_pipeline.R
## Implements the IRB analysis plan for CPSP as far as data allow:
## - Descriptives
## - Cross-tabulations (bivariable)
## - GLM steps gated by low event count

library(dplyr)
library(broom)
library(purrr)
library(readr)
library(here)

# -------------------------------------------------------------------
# 0. Load core setup and define CPSP (sum_pain ≥ 16)
# -------------------------------------------------------------------
source(here::here("scripts", "01_core_setup.R"))

thr_pain <- 16

df_cpsp <- df %>%
  mutate(
    sum_pain = sum_rescaled(cur_data_all(), pain_vars, min_prop = min_prop),
    cpsp = case_when(
      is.na(sum_pain)      ~ NA_character_,
      sum_pain >= thr_pain ~ "yes",
      TRUE                 ~ "no"
    )
  )

cpsp_events <- sum(df_cpsp$cpsp == "yes", na.rm = TRUE)
cpsp_total  <- sum(!is.na(df_cpsp$cpsp))

cat("CPSP events: ", cpsp_events, " of ", cpsp_total,
    " patients (", round(100 * cpsp_events / cpsp_total, 1), "%)\n\n", sep = "")

# -------------------------------------------------------------------
# 1. Descriptive: frequency of CPSP
# -------------------------------------------------------------------
cpsp_freq <- df_cpsp %>%
  count(cpsp) %>%
  mutate(
    prop = n / sum(n),
    pct  = round(100 * prop, 1)
  )

cat("CPSP frequency table:\n")
print(cpsp_freq)
cat("\n")

# -------------------------------------------------------------------
# 2. Derive categorical predictors for age and follow-up time
# -------------------------------------------------------------------
df_cpsp <- df_cpsp %>%
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

# -------------------------------------------------------------------
# 3. Cross-tabulations for independent variables vs CPSP
# -------------------------------------------------------------------
# These are the predictors we’ll examine in cross-tab form.
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

# Helper to compute cross-tab + p-value + zero-cell flag
crosstab_cpsp <- function(var_name) {
  vnm <- rlang::as_name(rlang::ensym(var_name))
  
  tmp <- df_cpsp %>%
    select(cpsp, !!rlang::sym(vnm)) %>%
    rename(predictor = !!rlang::sym(vnm)) %>%
    filter(!is.na(cpsp), !is.na(predictor))
  
  # No usable data
  if (nrow(tmp) == 0) {
    return(tibble(
      variable      = vnm,
      test          = NA_character_,
      p_value       = NA_real_,
      any_zero_cell = NA,
      note          = "No non-missing data"
    ))
  }
  
  tab <- table(tmp$predictor, tmp$cpsp, dnn = c(vnm, "cpsp"))
  
  # If the table is not at least 2x2, we cannot run chi-square or Fisher
  if (nrow(tab) < 2 || ncol(tab) < 2 || sum(tab) == 0) {
    return(tibble(
      variable      = vnm,
      test          = NA_character_,
      p_value       = NA_real_,
      any_zero_cell = NA,
      note          = "Table not 2x2 (insufficient variation)"
    ))
  }
  
  any_zero <- any(tab == 0)
  
  # Choose chi-squared vs Fisher
  if (all(tab >= 5)) {
    test_obj  <- suppressWarnings(chisq.test(tab))
    test_name <- "Chi-squared"
  } else {
    test_obj  <- suppressWarnings(fisher.test(tab))
    test_name <- "Fisher"
  }
  
  tibble(
    variable      = vnm,
    test          = test_name,
    p_value       = unname(test_obj$p.value),
    any_zero_cell = any_zero,
    note          = ifelse(any_zero, "Zero cell(s) present", "No zero cells")
  )
}

crosstab_results <- map_dfr(predictors_cat, crosstab_cpsp) %>%
  arrange(p_value)

cat("Bivariable cross-tab results (CPSP vs predictors):\n")
print(crosstab_results, n = nrow(crosstab_results))
cat("\n")

# Save cross-tab summary for Table / manuscript use
if (!dir.exists(here::here("outputs"))) {
  dir.create(here::here("outputs"), recursive = TRUE)
}
write_csv(crosstab_results,
          here::here("outputs", "cpsp_crosstab_summary.csv"))

# -------------------------------------------------------------------
# 4. Bivariable & multivariable GLM steps (gated by event count)
# -------------------------------------------------------------------
# IRB plan:
#  - Bivariable GLM for variables without zero cells
#  - Select variables with p < 0.20 for multivariable GLM
#  - However, with only 2 CPSP events, this is not statistically stable.

if (cpsp_events < 10) {
  cat("NOTE: Only ", cpsp_events,
      " CPSP events detected. This is below the commonly\n",
      "      recommended minimum of ~10 events per predictor for logistic\n",
      "      regression. To avoid unstable or uninterpretable estimates,\n",
      "      bivariable and multivariable GLMs are NOT being fit.\n\n", sep = "")
} else {
  # If you had enough events, this code would run:
  
  # 1) Select predictors with no zero cells for GLM
  glm_candidates <- crosstab_results %>%
    filter(any_zero_cell == FALSE, !is.na(p_value))
  
  # 2) Fit univariable logistic regression models
  run_univ_glm <- function(vnm) {
    vnm <- rlang::as_name(rlang::ensym(vnm))
    tmp <- df_cpsp %>%
      select(cpsp, !!rlang::sym(vnm)) %>%
      filter(!is.na(cpsp), !is.na(.data[[vnm]])) %>%
      mutate(cpsp_bin = ifelse(cpsp == "yes", 1, 0))
    
    fit <- try(
      glm(cpsp_bin ~ .data[[vnm]], data = tmp, family = binomial()),
      silent = TRUE
    )
    
    if (inherits(fit, "try-error")) {
      return(tibble(
        variable = vnm,
        level    = NA_character_,
        OR       = NA_real_,
        conf.low = NA_real_,
        conf.high= NA_real_,
        p_value  = NA_real_,
        note     = "Model failed to converge"
      ))
    }
    
    tidy_fit <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        variable = vnm,
        level    = term
      ) %>%
      select(variable, level, OR = estimate, conf.low, conf.high, p_value = p.value)
    
    tidy_fit
  }
  
  glm_univ_results <- map_dfr(glm_candidates$variable, run_univ_glm)
  
  # 3) Variables with p < 0.20 as candidates for multivariable model
  glm_screened <- glm_univ_results %>%
    group_by(variable) %>%
    summarise(
      min_p = min(p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(min_p < 0.20)
  
  # Placeholder: multivariable GLM would go here IF cpsp_events allowed.
  # With your current data, this block is intentionally not reached.
}

cat(
  "CPSP protocol pipeline completed.\n",
  "- Descriptive CPSP prevalence computed\n",
  "- Cross-tabulations vs key predictors saved to 'outputs/cpsp_crosstab_summary.csv'\n",
  "- GLM stages gated by low event count to reflect protocol but avoid invalid models.\n"
)