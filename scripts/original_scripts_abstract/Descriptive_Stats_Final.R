## Statistics Work for Chronic Pain Study 3 ---------------------------------

# Packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)

# Paths & Options ---------------------------------------------------------
data_path   <- "Library/CloudStorage/GoogleDrive-tanner.frahm@icahn.mssm.edu/My Drive/Research/Zhang/Chronic Pain/chronic_pain_dates.xlsx"
pain_thresh <- 2      # threshold for "has pain" (Likert ≥ this)
min_prop    <- 0.75   # minimum proportion of items required to rescale summed scores (≥75%)

# Read Data ---------------------------------------------------------------
df <- read_excel(data_path)

# Column names (final) ----------------------------------------------------
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

# Variable sets -----------------------------------------------------------
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

# Labels for figures ------------------------------------------------------
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

# Data Cleaning -----------------------------------------------------------
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

# Parse dates
df <- df %>%
  mutate(across("surgery_date", ymd)) %>%
  mutate(across(date_cols[2:3], ymd_hms))

# Make numerics (handle "N/A")
df[num_cols] <- lapply(df[num_cols], function(x) {
  x[x == "N/A"] <- NA
  as.numeric(x)
})

# Lowercase character fields
df <- df %>%
  mutate(across(all_of(char_cols), ~ ifelse(is.na(.x), NA, tolower(.x))))

# Logical follow-up flags
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

# Helpers: tables & tests -------------------------------------------------
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

cont_compare_ttest <- function(var, label) {
  var <- rlang::ensym(var); vnm <- rlang::as_name(var)
  s <- df %>%
    group_by(manual_labor) %>%
    summarise(n = sum(!is.na(!!var)), m = mean(!!var, na.rm = TRUE), sd = sd(!!var, na.rm = TRUE), .groups = "drop") %>%
    mutate(Variable = label, value = sprintf("%.1f (%.1f)", m, sd)) %>%
    select(Variable, manual_labor, n, value)
  pv <- try(t.test(df[[vnm]] ~ df$manual_labor)$p.value, silent = TRUE)
  s$p_value <- ifelse(inherits(pv, "try-error"), NA, pv)
  s
}

ordinal_compare <- function(var, label, round_quantiles = TRUE) {
  var <- rlang::ensym(var); vnm <- rlang::as_name(var)
  y <- as.numeric(df[[vnm]])
  s <- df %>%
    mutate(.y = y) %>%
    group_by(manual_labor) %>%
    summarise(
      n = sum(!is.na(.y)),
      med = median(.y, na.rm = TRUE),
      q1  = quantile(.y, 0.25, na.rm = TRUE),
      q3  = quantile(.y, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Variable = label,
      value = if (round_quantiles) sprintf("%d [%d–%d]", round(med), round(q1), round(q3))
      else sprintf("%.1f [%.1f–%.1f]", med, q1, q3)
    ) %>%
    select(Variable, manual_labor, n, value)
  pv <- try(wilcox.test(y ~ df$manual_labor)$p.value, silent = TRUE)
  s$p_value <- ifelse(inherits(pv, "try-error"), NA, pv)
  s
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

# Figure helper: Likert (stacked) ----------------------------------------
plot_likert_hist <- function(data, vars, labels = NULL, title = "Likert distributions") {
  longdat <- data %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "score") %>%
    mutate(score = suppressWarnings(as.numeric(score)),
           variable_label = if (!is.null(labels)) labels[variable] else variable)
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
    labs(title = title, x = "Likert score", y = "Proportion") +
    scale_fill_brewer(palette = "Blues", name = "Score") +
    theme_minimal(base_size = 12)
}

# Utility: summed/rescaled domain score -----------------------------------
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

summed_domain <- function(data, vars, min_prop = 0.75, domain_label = "Domain", fill_color = "skyblue") {
  n_items   <- length(vars)
  min_items <- ceiling(n_items * min_prop)
  
  d <- data %>%
    mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(
      na_count     = rowSums(is.na(pick(all_of(vars)))),
      n_answered   = rowSums(!is.na(pick(all_of(vars)))),
      raw_sum      = rowSums(pick(all_of(vars)), na.rm = TRUE),
      sum_rescaled = ifelse(n_answered >= min_items,
                            round(raw_sum * n_items / n_answered),
                            NA_real_)
    )
  
  included_n <- sum(!is.na(d$sum_rescaled))
  excluded_n <- sum(is.na(d$sum_rescaled))
  
  # Distribution (counts + proportions + clean labels)
  dist <- d %>%
    filter(!is.na(sum_rescaled)) %>%
    count(sum_rescaled, name = "n") %>%
    mutate(
      prop  = n / sum(n),
      label = paste0("N=", n, "\n", round(prop * 100, 1), "%")
    ) %>%
    arrange(sum_rescaled)
  
  # Plot
  p <- ggplot(dist, aes(x = sum_rescaled, y = prop)) +
    geom_col(color = "black", fill = fill_color) +
    geom_text(aes(label = label),
              vjust = -0.3, size = 3.5, lineheight = 0.95) +
    scale_y_continuous(
      labels = function(x) paste0(x * 100, "%"),
      expand = expansion(mult = c(0, 0.15))  # extra headroom for labels
    ) +
    scale_x_continuous(breaks = dist$sum_rescaled) +
    labs(
      title    = paste0("Summed ", tolower(domain_label), " score (rescaled)"),
      subtitle = paste0("Included: ", included_n, " (≥", min_items, "/", n_items, " answered); ",
                        "Excluded: ", excluded_n, " (<", min_items, ")"),
      x = paste0("Summed ", tolower(domain_label), " score (", n_items, "–", n_items * 5, " equivalent)"),
      y = "Proportion of patients"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    ) +
    coord_cartesian(clip = "off")  # prevent label clipping
  
  list(data = d, dist = dist, plot = p)
}

# Table 1 (Cohort) --------------------------------------------------------
row_N   <- tibble::tibble(Variable = "Total N", Level = "", n = nrow(df), prop = NA_real_, summary = "")
row_age <- num_summary(df, age, "Age")
row_bmi <- num_summary(df, bmi, "BMI")

tab_sex        <- cat_summary(df, sex,                      "Sex")
tab_employed   <- cat_summary(df, employed,                 "Employed")
tab_manual     <- cat_summary(df, manual_labor,             "Manual labor")
tab_educ       <- cat_summary(df, education,                "Education level")
tab_district   <- cat_summary(df, district,                 "District")
tab_subcounty  <- cat_summary(df, sub_county,               "Sub-county")
tab_pain_h2w   <- cat_summary(df, preop_hernia_pain_2w,     "Preop hernia pain (2w)")
tab_pain_other <- cat_summary(df, preop_chronic_pain_other, "Preop chronic pain (non-hernia)")
tab_opioids    <- cat_summary(df, preop_opioids_self,       "Preop opioids (self)")
tab_malign     <- cat_summary(df, malignancy,               "Malignancy")
tab_smoke      <- cat_summary(df, smoke,                    "Smokes")
tab_alcohol    <- cat_summary(df, alcohol,                  "Alcohol")

table1 <- dplyr::bind_rows(
  row_N, row_age, row_bmi,
  tab_sex, tab_employed, tab_manual, tab_educ, tab_district, tab_subcounty,
  tab_pain_h2w, tab_pain_other, tab_opioids, tab_malign, tab_smoke, tab_alcohol
) %>%
  mutate(
    Level = case_when(
      Level %in% c(TRUE, "TRUE", "True")   ~ "Yes",
      Level %in% c(FALSE, "FALSE", "False")~ "No",
      TRUE ~ as.character(Level)
    )
  )

# Table 2 (Manual vs Non-manual) -----------------------------------------
table2 <- bind_rows(
  cont_compare_ttest(age, "Age"),
  ordinal_compare(pain_lying,    "Pain lying",    round_quantiles = TRUE),
  ordinal_compare(pain_bending,  "Pain bending",  round_quantiles = TRUE),
  ordinal_compare(pain_sitting,  "Pain sitting",  round_quantiles = TRUE),
  ordinal_compare(pain_adl,      "Pain ADLs",     round_quantiles = TRUE),
  ordinal_compare(pain_cough,    "Pain coughing/deep breathing", round_quantiles = TRUE),
  ordinal_compare(pain_walk,     "Pain walking",  round_quantiles = TRUE),
  ordinal_compare(pain_stairs,   "Pain stairs",   round_quantiles = TRUE),
  ordinal_compare(pain_exercise, "Pain exercising", round_quantiles = TRUE),
  binary_compare(followed_2w, "Followed up at 2 weeks"),
  binary_compare(followed_1m, "Followed up at 1 month")
)

# Figure 0 — Follow-up timing from surgery date ---------------------------

# Compute follow-up timing
df_timing <- df %>%
  mutate(
    surgery_date    = as_date(surgery_date),
    start_time      = as_datetime(start_time),
    followup_days   = as.numeric(difftime(start_time, surgery_date, units = "days")),
    followup_weeks  = followup_days / 7,
    followup_months = time_length(interval(surgery_date, start_time), unit = "months"),
    timing_missing  = is.na(surgery_date) | is.na(start_time),
    timing_negative = !timing_missing & followup_days < 0
  )

# Diagnostics
timing_diag <- tibble::tibble(
  n_total          = nrow(df_timing),
  n_missing_either = sum(df_timing$timing_missing, na.rm = TRUE),
  n_negative       = sum(df_timing$timing_negative, na.rm = TRUE),
  n_valid          = sum(!df_timing$timing_missing & !df_timing$timing_negative, na.rm = TRUE)
)
print(timing_diag)

# Histogram of days since surgery (valid rows)
timing_valid <- df_timing %>% filter(!timing_missing, !timing_negative)

ggplot(timing_valid, aes(x = followup_days)) +
  geom_histogram(binwidth = 7, boundary = 0, color = "black", fill = "skyblue") +
  labs(
    title = "Histogram of days from surgery to follow-up interview",
    subtitle = paste0("Valid records: ", nrow(timing_valid),
                      " | Missing/invalid: ", timing_diag$n_total - nrow(timing_valid)),
    x = "Days since surgery", y = "Number of patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Two-month bins: 0–2, 2–4, …, 10–12, 12+
breaks_mo  <- seq(0, 12, by = 2)
labels_mo  <- paste0(head(breaks_mo, -1), "–", tail(breaks_mo, -1), " mo")
labels_all <- c(labels_mo, "12+ mo")

timing_binned <- timing_valid %>%
  mutate(
    followup_month_bin = cut(
      followup_months,
      breaks = c(breaks_mo, Inf),
      labels = labels_all,
      right  = FALSE
    )
  )

bin_tab <- timing_binned %>%
  count(followup_month_bin, name = "n") %>%
  mutate(prop = n / sum(n))
print(bin_tab %>% mutate(prop = round(prop, 3)), n = Inf)

ggplot(bin_tab, aes(x = followup_month_bin, y = prop)) +
  geom_col(color = "black", fill = "green") +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")), vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Figure 0: Follow-up timing binned by months since surgery",
    subtitle = paste0("2-month bins; left-closed/right-open. Valid n = ", nrow(timing_valid)),
    x = "Months since surgery (2-month bins)", y = "Proportion of patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Figure 1 — % reporting pain by activity (overall) -----------------------
fig1_pain <- df %>%
  select(all_of(pain_vars)) %>%
  pivot_longer(everything(), names_to = "activity", values_to = "score") %>%
  mutate(score = suppressWarnings(as.numeric(score)),
         has_pain = ifelse(!is.na(score) & score >= pain_thresh, TRUE, FALSE)) %>%
  group_by(activity) %>%
  summarise(n_nonmiss = sum(!is.na(score)),
            n_pain = sum(has_pain, na.rm = TRUE),
            prop_pain = ifelse(n_nonmiss > 0, n_pain / n_nonmiss, NA_real_), .groups = "drop") %>%
  mutate(activity_label = pain_labels[activity]) %>%
  arrange(desc(prop_pain))

fig1_pain %>%
  transmute(Activity = activity_label, N = n_nonmiss,
            `n with pain` = n_pain, Proportion = round(prop_pain, 3)) %>%
  print(n = Inf)

ggplot(fig1_pain, aes(x = prop_pain, y = reorder(activity_label, prop_pain))) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(prop_pain * 100, 1), "%")), hjust = -0.1, size = 3.5) +
  scale_x_continuous(limits = c(0, max(fig1_pain$prop_pain, na.rm = TRUE) * 1.1),
                     labels = function(x) paste0(x * 100, "%")) +
  labs(title = "Figure 1. Proportion reporting pain by activity",
       subtitle = paste0("Threshold: score ≥ ", pain_thresh, " (Likert 1–5)"),
       x = "Percent with pain", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Likert distributions (pain / mesh / limitations) ------------------------
plot_likert_hist(df, pain_vars,  pain_labels,  "Distribution of pain scores by activity")
plot_likert_hist(df, mesh_vars,  mesh_labels,  "Distribution of mesh sensation scores by activity")
plot_likert_hist(df, limit_vars, limit_labels, "Distribution of movement limitation scores by activity")

# Summed scores (rescaled if ≥ 75% answered) ------------------------------
sum_pain  <- summed_domain(df, pain_vars,  min_prop = min_prop, domain_label = "Pain",                fill_color = "skyblue")

# using your sum_pain object
sum_pain$plot +
  labs(
    title = "Carolina Pain Score After Inguinal Hernia Repair",
    subtitle = "Distribution of summed pain scores "
  )

sum_mesh  <- summed_domain(df, mesh_vars,  min_prop = min_prop, domain_label = "Mesh sensation",      fill_color = "orange")
sum_limit <- summed_domain(df, limit_vars, min_prop = min_prop, domain_label = "Movement limitation", fill_color = "purple")

print(sum_mesh$plot)
print(sum_limit$plot)

# Follow-up summary (overall) ---------------------------------------------
followup_overall <- tibble::tibble(
  variable = c("Followed at 2 weeks","Followed at 1 month"),
  n_nonmiss = c(sum(!is.na(df$followed_2w)), sum(!is.na(df$followed_1m))),
  n_yes     = c(sum(df$followed_2w %in% TRUE, na.rm = TRUE),
                sum(df$followed_1m %in% TRUE, na.rm = TRUE))
) %>%
  mutate(prop = ifelse(n_nonmiss > 0, n_yes / n_nonmiss, NA_real_))
print(followup_overall)

# Compare scores by time (boxplots per domain) ----------------------------
timing_valid <- timing_valid %>%
  mutate(
    sum_pain  = sum_rescaled(cur_data_all(), pain_vars,  min_prop = min_prop),   # 8–40
    sum_mesh  = sum_rescaled(cur_data_all(), mesh_vars,  min_prop = min_prop),   # 8–40
    sum_limit = sum_rescaled(cur_data_all(), limit_vars, min_prop = min_prop)    # 7–35
  )

plot_box_by_bin <- function(data, score_col, y_label, title) {
  ggplot(data, aes(x = followup_month_bin, y = .data[[score_col]])) +
    geom_boxplot(outlier.alpha = 0.6, color = "black", fill = "white") +
    labs(title = title,
         subtitle = "Boxes show median and IQR; whiskers ~1.5×IQR",
         x = "Months since surgery (2-month bins)", y = y_label) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

p_box_pain <- timing_binned %>%
  mutate(sum_pain = timing_valid$sum_pain[match(id, timing_valid$id)]) %>%
  filter(!is.na(sum_pain), !is.na(followup_month_bin)) %>%
  plot_box_by_bin("sum_pain", "Summed pain score (8–40, rescaled)", "Pain vs. time since surgery")

p_box_mesh <- timing_binned %>%
  mutate(sum_mesh = timing_valid$sum_mesh[match(id, timing_valid$id)]) %>%
  filter(!is.na(sum_mesh), !is.na(followup_month_bin)) %>%
  plot_box_by_bin("sum_mesh", "Summed mesh sensation score (8–40, rescaled)", "Mesh sensation vs. time since surgery")

p_box_limit <- timing_binned %>%
  mutate(sum_limit = timing_valid$sum_limit[match(id, timing_valid$id)]) %>%
  filter(!is.na(sum_limit), !is.na(followup_month_bin)) %>%
  plot_box_by_bin("sum_limit", "Summed movement limitation score (7–35, rescaled)", "Movement limitation vs. time since surgery")

print(p_box_pain)
print(p_box_mesh)
print(p_box_limit)

# N per bin (optional)
n_bin_pain  <- timing_binned %>% mutate(sum_pain  = timing_valid$sum_pain[match(id, timing_valid$id)])  %>%
  filter(!is.na(sum_pain),  !is.na(followup_month_bin)) %>% count(followup_month_bin, name = "n")
n_bin_mesh  <- timing_binned %>% mutate(sum_mesh  = timing_valid$sum_mesh[match(id, timing_valid$id)])  %>%
  filter(!is.na(sum_mesh),  !is.na(followup_month_bin)) %>% count(followup_month_bin, name = "n")
n_bin_limit <- timing_binned %>% mutate(sum_limit = timing_valid$sum_limit[match(id, timing_valid$id)]) %>%
  filter(!is.na(sum_limit), !is.na(followup_month_bin)) %>% count(followup_month_bin, name = "n")

n_bin_pain; n_bin_mesh; n_bin_limit

# Scatter: incision length & surgery duration vs pain ---------------------
# Ensure summed pain on df (for direct scatterplots)
if (!"sum_pain" %in% names(df)) {
  df <- df %>%
    mutate(sum_pain = sum_rescaled(cur_data_all(), pain_vars, min_prop = min_prop))
}

p_incision <- df %>%
  filter(!is.na(incision_length_prev_surg_cm), !is.na(sum_pain)) %>%
  ggplot(aes(x = incision_length_prev_surg_cm, y = sum_pain)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Summed pain score vs. incision length",
       x = "Incision length (cm)", y = "Summed pain score (8–40, rescaled)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

p_duration <- df %>%
  filter(!is.na(surgery_duration_minutes), !is.na(sum_pain)) %>%
  ggplot(aes(x = surgery_duration_minutes, y = sum_pain)) +
  geom_point(alpha = 0.6, color = "tomato") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Summed pain score vs. surgery duration",
       x = "Surgery duration (minutes)", y = "Summed pain score (8–40, rescaled)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p_incision)
print(p_duration)



# range of dates ----------------------------------------------------------

df_summary <- df %>%
  mutate(
    # make sure dates are in Date format
    surgery_date = as_date(surgery_date),
    survey_date  = as_date(start_time),   # or use the column that is the follow-up date
    postop_day   = as.numeric(survey_date - surgery_date)
  )

# summarize
survey_timing <- df_summary %>%
  summarise(
    mean_day = mean(postop_day, na.rm = TRUE),
    min_day  = min(postop_day, na.rm = TRUE),
    max_day  = max(postop_day, na.rm = TRUE),
    n = n()
  )

survey_timing