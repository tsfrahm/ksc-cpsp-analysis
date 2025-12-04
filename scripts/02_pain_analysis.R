## 02_pain_analysis.R
## Pain (CPSP) domain analysis using core setup

library(dplyr)
library(ggplot2)
library(scales)
library(here)

# Load core data + functions
source(here::here("scripts", "01_core_setup.R"))

# ---------------------- 1. Summed pain scores ---------------------- #
df_pain <- df %>%
  mutate(
    sum_pain = sum_rescaled(cur_data_all(), pain_vars, min_prop = min_prop)
  )

n_pain_items <- length(pain_vars)
pain_max     <- n_pain_items * 5
pain_min     <- n_pain_items * 1

cat("Pain domain: ", n_pain_items, " items; theoretical range ", pain_min, "–", pain_max, "\n", sep = "")

# ---------------------- 2. CPSP threshold & binary outcome ---------------------- #
thr_pain <- 16  # CCS definition

cat("CPSP threshold (sum_pain ≥ ", thr_pain, ") = ",
    round(100 * thr_pain / pain_max, 1), "% of max pain score\n\n", sep = "")

df_pain <- df_pain %>%
  mutate(
    cpsp = case_when(
      is.na(sum_pain)      ~ NA_character_,
      sum_pain >= thr_pain ~ "yes",
      TRUE                 ~ "no"
    )
  )

# ---------------------- 3. Prevalence of CPSP ---------------------- #
cpsp_prev <- df_pain %>%
  filter(!is.na(cpsp)) %>%
  summarise(
    N       = n(),
    n_yes   = sum(cpsp == "yes"),
    n_no    = sum(cpsp == "no"),
    pct_yes = n_yes / N,
    pct_no  = n_no  / N
  )

cat("CPSP prevalence (sum_pain ≥ ", thr_pain, "):\n", sep = "")
print(
  cpsp_prev %>%
    mutate(
      pct_yes = round(pct_yes * 100, 1),
      pct_no  = round(pct_no  * 100, 1)
    )
)
cat("\n")

if (nrow(cpsp_prev) == 1) {
  cat(
    "Sentence for results: CPSP (summed pain ≥ ", thr_pain, ") was observed in ",
    cpsp_prev$n_yes, " of ", cpsp_prev$N, " patients (",
    round(cpsp_prev$pct_yes * 100, 1), "%).\n\n", sep = ""
  )
}

# ---------------------- 4. Distribution of summed pain scores ---------------------- #
pain_dist <- df_pain %>%
  filter(!is.na(sum_pain)) %>%
  count(sum_pain, name = "n") %>%
  mutate(
    prop     = n / sum(n),
    prop_pct = round(prop * 100, 1)
  ) %>%
  arrange(sum_pain)

cat("Distribution of rescaled summed pain scores:\n")
print(pain_dist, n = nrow(pain_dist))
cat("\n")

# Plot: Summed pain score distribution
pain_dist_plot <- pain_dist %>%
  mutate(
    label = paste0("N=", n, "\n", prop_pct, "%")
  )

p_pain <- ggplot(pain_dist_plot, aes(x = sum_pain, y = prop)) +
  geom_col(color = "black", fill = "skyblue") +
  geom_text(aes(label = label),
            vjust = -0.3, size = 3.5, lineheight = 0.95) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_continuous(breaks = pain_dist_plot$sum_pain) +
  labs(
    title    = "Few Patients Experience CPSP After Ambulatory Inguinal Hernia Repair",
    subtitle = paste0(
      "Summed pain score distribution (rescaled; CPSP threshold ≥ ",
      thr_pain, ")"
    ),
    x = paste0("Summed pain score (", pain_min, "–", pain_max, ")"),
    y = "Proportion of patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  ) +
  coord_cartesian(clip = "off")

print(p_pain)

# ---------------------- 5. Simple associations (CPSP vs selected predictors) ---------------------- #
pain_crosstab <- function(pred_var, label = NULL) {
  vnm <- rlang::as_name(rlang::ensym(pred_var))
  lab <- ifelse(is.null(label), vnm, label)
  
  tmp <- df_pain %>%
    select(cpsp, !!rlang::sym(vnm)) %>%
    rename(predictor = !!rlang::sym(vnm)) %>%
    filter(!is.na(cpsp), !is.na(predictor))
  
  # Summarize by predictor level
  tab <- tmp %>%
    group_by(predictor) %>%
    summarise(
      n_total = n(),
      n_yes   = sum(cpsp == "yes"),
      pct_yes = ifelse(n_total > 0, round(100 * n_yes / n_total, 1), NA_real_),
      .groups = "drop"
    )
  
  # 2×2 test if predictor is binary
  xt <- table(tmp$predictor, tmp$cpsp)
  pval <- NA_real_
  if (nrow(xt) >= 2 && ncol(xt) == 2 && sum(xt) > 0 &&
      all(rowSums(xt) > 0) && all(colSums(xt) > 0)) {
    pval <- if (all(xt >= 5)) {
      suppressWarnings(chisq.test(xt)$p.value)
    } else {
      suppressWarnings(fisher.test(xt)$p.value)
    }
  }
  
  tab %>%
    mutate(
      Variable = lab,
      Level    = as.character(predictor),
      p_value  = pval
    ) %>%
    select(Variable, Level, n_yes, n_total, pct_yes, p_value)
}
# ---------------------- 6. Save optional outputs ---------------------- #
# write.csv(pain_dist,  here::here("outputs", "pain_score_distribution.csv"), row.names = FALSE)
# write.csv(cpsp_prev,  here::here("outputs", "cpsp_prevalence.csv"),         row.names = FALSE)
# write.csv(pain_assoc, here::here("outputs", "cpsp_associations.csv"),       row.names = FALSE)
# ggsave(here::here("figures", "pain_sum_distribution.png"), p_pain, width = 7, height = 5, dpi = 300)

cat("Pain domain analysis complete.\n")