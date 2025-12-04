## 08_question_by_question_analysis.R
## Question-by-question descriptive analyses + faceted plots
## for pain, mesh sensation, and functional limitation domains.

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(here)

# Load core setup (df, variable sets, labels, sum_rescaled, etc.)
source(here::here("scripts", "01_core_setup.R"))

# ---------------------------
# Helper: item-level summaries
# ---------------------------
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

# ---------------------------
# A. PAIN (8 items)
# ---------------------------
pain_item_summary <- item_summary(df, pain_vars, pain_labels,
                                  threshold = 2, domain = "Pain")

cat("Question-by-question Pain domain summary:\n")
print(pain_item_summary, n = Inf)
cat("\nTop findings:\n")
print(head(pain_item_summary, 3))
cat("\n")

p_pain_items <- plot_likert_hist(df, pain_vars, pain_labels,
                                 "Pain domain: Likert score distributions")

# ---------------------------
# B. MESH SENSATION (8 items)
# ---------------------------
mesh_item_summary <- item_summary(df, mesh_vars, mesh_labels,
                                  threshold = 2, domain = "Mesh")

cat("Question-by-question Mesh sensation summary:\n")
print(mesh_item_summary, n = Inf)
cat("\nTop findings:\n")
print(head(mesh_item_summary, 3))
cat("\n")

p_mesh_items <- plot_likert_hist(df, mesh_vars, mesh_labels,
                                 "Mesh sensation domain: Likert score distributions")

# ---------------------------
# C. FUNCTIONAL LIMITATION (7 items)
# ---------------------------
limit_item_summary <- item_summary(df, limit_vars, limit_labels,
                                   threshold = 2, domain = "Limitation")

cat("Question-by-question Functional limitation summary:\n")
print(limit_item_summary, n = Inf)
cat("\nTop findings:\n")
print(head(limit_item_summary, 3))
cat("\n")

p_limit_items <- plot_likert_hist(df, limit_vars, limit_labels,
                                  "Functional limitation domain: Likert score distributions")

# ---------------------------
# Save outputs (optional)
# ---------------------------
if (!dir.exists(here::here("outputs"))) {
  dir.create(here::here("outputs"))
}
if (!dir.exists(here::here("figures"))) {
  dir.create(here::here("figures"))
}

readr::write_csv(pain_item_summary,
                 here::here("outputs", "pain_item_summary.csv"))
readr::write_csv(mesh_item_summary,
                 here::here("outputs", "mesh_item_summary.csv"))
readr::write_csv(limit_item_summary,
                 here::here("outputs", "limit_item_summary.csv"))

ggsave(here::here("figures", "pain_items_plot.png"),  p_pain_items,  width = 10, height = 8, dpi = 300)
ggsave(here::here("figures", "mesh_items_plot.png"),  p_mesh_items,  width = 10, height = 8, dpi = 300)
ggsave(here::here("figures", "limit_items_plot.png"), p_limit_items, width = 10, height = 8, dpi = 300)

cat("Question-by-question analysis complete. CSV summaries + plots saved.\n")