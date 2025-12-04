# CPSP cross-tabs with p-values for YES/NO predictors ---------------------
library(dplyr)
library(tidyr)
library(lubridate)

# --- If not already defined earlier in your script -----------------------
pain_vars <- c("pain_lying","pain_bending","pain_sitting","pain_adl",
               "pain_cough","pain_walk","pain_stairs","pain_exercise")

sum_rescaled <- function(data, vars, min_prop = 0.75) {
  n_items   <- length(vars)
  min_items <- ceiling(n_items * min_prop)
  data %>%
    mutate(
      across(all_of(vars), ~ suppressWarnings(as.numeric(.x))),
      .n_ans = rowSums(!is.na(pick(all_of(vars)))),
      .raw   = rowSums(pick(all_of(vars)), na.rm = TRUE),
      sum_pain = ifelse(.n_ans >= min_items, round(.raw * n_items / .n_ans), NA_real_)
    ) %>%
    pull(sum_pain)
}

if (!("cpsp" %in% names(df))) {
  df <- df %>%
    mutate(
      surgery_date    = as_date(surgery_date),
      start_time      = as_datetime(start_time),
      followup_months = time_length(interval(as_date(surgery_date), as_datetime(start_time)), "months"),
      sum_pain        = if ("sum_pain" %in% names(df)) sum_pain else sum_rescaled(cur_data_all(), pain_vars, 0.75),
      cpsp            = ifelse(!is.na(sum_pain) & !is.na(followup_months) & followup_months >= 3,
                               sum_pain >= 16, NA)
    )
}

# --- Helper: robust yes/no coercion (returns factor with levels no/yes) ---
as_yesno_factor <- function(x) {
  if (is.logical(x)) return(factor(ifelse(x, "yes", "no"), levels = c("no","yes")))
  x_chr <- tolower(as.character(x))
  y <- case_when(
    x_chr %in% c("yes","y","true","1")  ~ "yes",
    x_chr %in% c("no","n","false","0")  ~ "no",
    TRUE ~ NA_character_
  )
  factor(y, levels = c("no","yes"))
}

# --- Cross-tab function with p-values (χ² or Fisher) ---------------------
xtab_cpsp_yesno <- function(data, var, label = NULL) {
  var_sym  <- rlang::ensym(var)
  var_name <- ifelse(is.null(label), rlang::as_name(var_sym), label)
  
  d <- data %>%
    transmute(
      pred = as_yesno_factor(!!var_sym),
      cpsp = cpsp
    ) %>%
    filter(!is.na(pred), !is.na(cpsp))
  
  # Handle degenerate cases
  if (nrow(d) == 0 || length(unique(d$pred)) < 2 || length(unique(d$cpsp)) < 2) {
    return(tibble(
      Variable = var_name, level = c("no","yes"), n_yes = NA_integer_,
      n_no = NA_integer_, n_total = NA_integer_, prop_yes = NA_real_,
      p_value = NA_real_, test = NA_character_
    ))
  }
  
  # Counts table with completed cells
  ct <- d %>%
    count(pred, cpsp, name = "n") %>%
    complete(pred = factor(c("no","yes"), levels = c("no","yes")),
             cpsp = c(FALSE, TRUE), fill = list(n = 0))
  
  # 2x2 matrix for testing
  xt <- xtabs(n ~ pred + cpsp, data = ct)
  
  # Choose test: Pearson χ² if all cells >=5, otherwise Fisher's exact
  if (all(xt >= 5)) {
    p <- suppressWarnings(chisq.test(xt, correct = FALSE)$p.value)
    test_name <- "Chi-square"
  } else {
    p <- suppressWarnings(fisher.test(xt)$p.value)
    test_name <- "Fisher's exact"
  }
  
  # Row-wise props
  out <- ct %>%
    group_by(pred) %>%
    summarise(
      n_yes    = sum(n[cpsp == TRUE]),
      n_no     = sum(n[cpsp == FALSE]),
      n_total  = n_yes + n_no,
      prop_yes = ifelse(n_total > 0, n_yes / n_total, NA_real_),
      .groups  = "drop"
    ) %>%
    mutate(
      Variable = var_name,
      level    = as.character(pred),
      p_value  = p,
      test     = test_name
    ) %>%
    select(Variable, level, n_yes, n_no, n_total, prop_yes, p_value, test)
  
  out
}

# --- List of yes/no variables (edit as needed for your df) ----------------
yesno_vars <- c(
  "employed","manual_labor","preop_hernia_pain_2w","preop_chronic_pain_other",
  "preop_opioids_self","malignancy","other_disease","smoke","alcohol",
  "substance_other","preop_opioids_prescribed","intraop_complication",
  "preop_chronic_pain_any","preop_painkillers_before_surgery",
  "discharge_pain_education","followed_2w","followed_1m","discharge_anesth_visit",
  "periop_regional","preventive_preop","preventive_postop"
)

# --- Run for all and bind -------------------------------------------------
xtabs_all <- bind_rows(lapply(yesno_vars, function(v) {
  tryCatch(xtab_cpsp_yesno(df, !!sym(v), v), error = function(e) NULL)
})) %>%
  arrange(Variable, desc(level))  # optional ordering

print(xtabs_all, n = Inf)

# --- Optional compact summary: one row per predictor ----------------------
# Shows props in "yes" vs "no" and the p-value/test used
xtabs_summary <- xtabs_all %>%
  select(Variable, level, prop_yes, p_value, test) %>%
  pivot_wider(names_from = level, values_from = prop_yes, names_prefix = "prop_") %>%
  distinct(Variable, .keep_all = TRUE) %>%
  arrange(p_value)

print(xtabs_summary, n = Inf)


# Assuming df is your dataset with cpsp (0/1) and manual_labor (yes/no or 0/1)

# Make sure manual_labor is a factor with a reference level
df$manual_labor <- factor(df$manual_labor, levels = c("no", "yes"))

# Run logistic regression (GLM with binomial family)
fit <- glm(cpsp ~ manual_labor, data = df, family = binomial)

# Summarize coefficients
summary(fit)

# Get odds ratios with confidence intervals
exp(cbind(OR = coef(fit), confint(fit)))