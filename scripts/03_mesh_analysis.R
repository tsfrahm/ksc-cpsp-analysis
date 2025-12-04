## 03_mesh_analysis.R
## Mesh sensation domain analysis (8 items, threshold = 16)

library(dplyr)
library(ggplot2)
library(scales)
library(here)

# Load core data + helpers
source(here::here("scripts", "01_core_setup.R"))

# ---------------------- 1. Summed mesh scores ---------------------- #
df_mesh <- df %>%
  mutate(
    sum_mesh = sum_rescaled(cur_data_all(), mesh_vars, min_prop = min_prop)
  )

n_mesh_items <- length(mesh_vars)    # should be 8
mesh_max     <- n_mesh_items * 5     # 8 * 5 = 40
mesh_min     <- n_mesh_items * 1

cat("Mesh domain: ", n_mesh_items, " items; theoretical range ",
    mesh_min, "–", mesh_max, "\n", sep = "")

# ---------------------- 2. Threshold: sum_mesh ≥ 16 ---------------------- #
thr_mesh <- 16   # same absolute CCS cutoff as pain domain

cat("Mesh sensation threshold (sum_mesh ≥ ", thr_mesh, ") = ",
    round(100 * thr_mesh / mesh_max, 1), "% of max mesh score\n\n", sep = "")

df_mesh <- df_mesh %>%
  mutate(
    mesh_positive = case_when(
      is.na(sum_mesh)        ~ NA_character_,
      sum_mesh >= thr_mesh   ~ "yes",
      TRUE                   ~ "no"
    )
  )

# ---------------------- 3. Prevalence of mesh ≥16 ---------------------- #
mesh_prev <- df_mesh %>%
  filter(!is.na(mesh_positive)) %>%
  summarise(
    N        = n(),
    n_yes    = sum(mesh_positive == "yes"),
    n_no     = sum(mesh_positive == "no"),
    pct_yes  = n_yes / N,
    pct_no   = n_no  / N
  )

cat("Mesh sensation prevalence (sum_mesh ≥ ", thr_mesh, "):\n", sep = "")
print(
  mesh_prev %>%
    mutate(
      pct_yes = round(pct_yes * 100, 1),
      pct_no  = round(pct_no  * 100, 1)
    )
)
cat("\n")

if (nrow(mesh_prev) == 1) {
  cat(
    "Sentence for results: Mesh sensation (summed CCS mesh score ≥ ",
    thr_mesh, ") was observed in ",
    mesh_prev$n_yes, " of ", mesh_prev$N, " patients (",
    round(mesh_prev$pct_yes * 100, 1), "%).\n\n", sep = ""
  )
}

# ---------------------- 4. Distribution of summed mesh scores ---------------------- #
mesh_dist <- df_mesh %>%
  filter(!is.na(sum_mesh)) %>%
  count(sum_mesh, name = "n") %>%
  mutate(
    prop     = n / sum(n),
    prop_pct = round(prop * 100, 1)
  ) %>%
  arrange(sum_mesh)

cat("Distribution of rescaled summed mesh scores:\n")
print(mesh_dist, n = nrow(mesh_dist))
cat("\n")

# Plot: summed mesh score distribution
mesh_dist_plot <- mesh_dist %>%
  mutate(
    label = paste0("N=", n, "\n", prop_pct, "%")
  )

p_mesh <- ggplot(mesh_dist_plot, aes(x = sum_mesh, y = prop)) +
  geom_col(color = "black", fill = "orange") +
  geom_text(aes(label = label),
            vjust = -0.3, size = 3.5, lineheight = 0.95) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_continuous(breaks = mesh_dist_plot$sum_mesh) +
  labs(
    title    = "Few Patients Report Clinically Significant Mesh Sensation",
    subtitle = paste0(
      "Summed mesh sensation score distribution (rescaled; threshold ≥ ",
      thr_mesh, ")"
    ),
    x = paste0("Summed mesh sensation score (", mesh_min, "–", mesh_max, ")"),
    y = "Proportion of patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  ) +
  coord_cartesian(clip = "off")

print(p_mesh)

# ---------------------- 5. Associations: mesh ≥16 vs selected predictors ---------------------- #
mesh_crosstab <- function(pred_var, label = NULL) {
  vnm <- rlang::as_name(rlang::ensym(pred_var))
  lab <- ifelse(is.null(label), vnm, label)
  
  tmp <- df_mesh %>%
    select(mesh_positive, !!rlang::sym(vnm)) %>%
    rename(predictor = !!rlang::sym(vnm)) %>%
    filter(!is.na(mesh_positive), !is.na(predictor))
  
  tab <- tmp %>%
    group_by(predictor) %>%
    summarise(
      n_total = n(),
      n_yes   = sum(mesh_positive == "yes"),
      pct_yes = ifelse(n_total > 0, round(100 * n_yes / n_total, 1), NA_real_),
      .groups = "drop"
    )
  
  xt <- table(tmp$predictor, tmp$mesh_positive)
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

mesh_assoc <- bind_rows(
  mesh_crosstab(sex,          "Sex"),
  mesh_crosstab(manual_labor, "Manual labor"),
  mesh_crosstab(employed,     "Employed"),
  mesh_crosstab(education,    "Education level")
)

cat("Mesh sensation (sum_mesh ≥ ", thr_mesh, ") by selected predictors:\n", sep = "")
print(mesh_assoc, n = nrow(mesh_assoc))
cat("\n")

# ---------------------- 6. Optional: save outputs ---------------------- #
# write.csv(mesh_dist,  here::here("outputs", "mesh_score_distribution.csv"),   row.names = FALSE)
# write.csv(mesh_prev,  here::here("outputs", "mesh_prevalence.csv"),          row.names = FALSE)
# write.csv(mesh_assoc, here::here("outputs", "mesh_associations.csv"),        row.names = FALSE)
# ggsave(here::here("figures", "mesh_sum_distribution.png"), p_mesh, width = 7, height = 5, dpi = 300)

cat("Mesh domain analysis complete.\n")