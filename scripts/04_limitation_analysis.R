## 04_limit_analysis.R
## Movement limitation domain analysis (using 7 items, threshold = 14)

library(dplyr)
library(ggplot2)
library(scales)
library(here)

# Load core data + helpers
source(here::here("scripts", "01_core_setup.R"))

# ---------------------- 1. Summed limitation scores ---------------------- #
df_limit <- df %>%
  mutate(
    sum_limit = sum_rescaled(cur_data_all(), limit_vars, min_prop = min_prop)
  )

n_limit_items <- length(limit_vars)          # should be 7 given current Excel
limit_max     <- n_limit_items * 5           # 7 * 5 = 35
limit_min     <- n_limit_items * 1

cat("Limitation domain: ", n_limit_items, " items; theoretical range ",
    limit_min, "–", limit_max, "\n", sep = "")

# ---------------------- 2. Threshold: 40% of max (14/35) ---------------------- #
thr_limit <- 14   # proportional to CPSP 16/40

cat("Functional limitation threshold (sum_limit ≥ ", thr_limit, ") = ",
    round(100 * thr_limit / limit_max, 1), "% of max limitation score\n\n", sep = "")

df_limit <- df_limit %>%
  mutate(
    limit_positive = case_when(
      is.na(sum_limit)          ~ NA_character_,
      sum_limit >= thr_limit    ~ "yes",
      TRUE                      ~ "no"
    )
  )

# ---------------------- 3. Prevalence of limitations ≥ threshold ---------------------- #
limit_prev <- df_limit %>%
  filter(!is.na(limit_positive)) %>%
  summarise(
    N        = n(),
    n_yes    = sum(limit_positive == "yes"),
    n_no     = sum(limit_positive == "no"),
    pct_yes  = n_yes / N,
    pct_no   = n_no  / N
  )

cat("Functional limitation prevalence (sum_limit ≥ ", thr_limit, "):\n", sep = "")
print(
  limit_prev %>%
    mutate(
      pct_yes = round(pct_yes * 100, 1),
      pct_no  = round(pct_no  * 100, 1)
    )
)
cat("\n")

if (nrow(limit_prev) == 1) {
  cat(
    "Sentence for results: Functional limitation (summed CCS limitation score ≥ ",
    thr_limit, ") was observed in ",
    limit_prev$n_yes, " of ", limit_prev$N, " patients (",
    round(limit_prev$pct_yes * 100, 1), "%).\n\n", sep = ""
  )
}

# ---------------------- 4. Distribution of summed limitation scores ---------------------- #
limit_dist <- df_limit %>%
  filter(!is.na(sum_limit)) %>%
  count(sum_limit, name = "n") %>%
  mutate(
    prop     = n / sum(n),
    prop_pct = round(prop * 100, 1)
  ) %>%
  arrange(sum_limit)

cat("Distribution of rescaled summed limitation scores:\n")
print(limit_dist, n = nrow(limit_dist))
cat("\n")

# Plot: summed limitation score distribution
limit_dist_plot <- limit_dist %>%
  mutate(
    label = paste0("N=", n, "\n", prop_pct, "%")
  )

p_limit <- ggplot(limit_dist_plot, aes(x = sum_limit, y = prop)) +
  geom_col(color = "black", fill = "purple") +
  geom_text(aes(label = label),
            vjust = -0.3, size = 3.5, lineheight = 0.95) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_continuous(breaks = limit_dist_plot$sum_limit) +
  labs(
    title    = "Few Patients Report Clinically Significant Functional Limitations",
    subtitle = paste0(
      "Summed limitation score distribution (rescaled; threshold ≥ ",
      thr_limit, ")"
    ),
    x = paste0("Summed limitation score (", limit_min, "–", limit_max, ")"),
    y = "Proportion of patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  ) +
  coord_cartesian(clip = "off")

print(p_limit)

# ---------------------- 5. Associations: limitation ≥14 vs selected predictors ---------------------- #
limit_crosstab <- function(pred_var, label = NULL) {
  vnm <- rlang::as_name(rlang::ensym(pred_var))
  lab <- ifelse(is.null(label), vnm, label)
  
  tmp <- df_limit %>%
    select(limit_positive, !!rlang::sym(vnm)) %>%
    rename(predictor = !!rlang::sym(vnm)) %>%
    filter(!is.na(limit_positive), !is.na(predictor))
  
  tab <- tmp %>%
    group_by(predictor) %>%
    summarise(
      n_total = n(),
      n_yes   = sum(limit_positive == "yes"),
      pct_yes = ifelse(n_total > 0, round(100 * n_yes / n_total, 1), NA_real_),
      .groups = "drop"
    )
  
  xt <- table(tmp$predictor, tmp$limit_positive)
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

limit_assoc <- bind_rows(
  limit_crosstab(sex,          "Sex"),
  limit_crosstab(manual_labor, "Manual labor"),
  limit_crosstab(employed,     "Employed"),
  limit_crosstab(education,    "Education level")
)

cat("Functional limitation (sum_limit ≥ ", thr_limit, ") by selected predictors:\n", sep = "")
print(limit_assoc, n = nrow(limit_assoc))
cat("\n")

# ---------------------- 6. Optional: save outputs ---------------------- #
# write.csv(limit_dist,  here::here("outputs", "limit_score_distribution.csv"),   row.names = FALSE)
# write.csv(limit_prev,  here::here("outputs", "limit_prevalence.csv"),          row.names = FALSE)
# write.csv(limit_assoc, here::here("outputs", "limit_associations.csv"),        row.names = FALSE)
# ggsave(here::here("figures", "limit_sum_distribution.png"), p_limit, width = 7, height = 5, dpi = 300)

cat("Limitation domain analysis complete.\n")