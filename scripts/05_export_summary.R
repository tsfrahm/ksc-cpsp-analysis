## 05_export_summary.R
## Combined exports + single summary table for pain, mesh, and limitation domains

library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(here)

# -------------------------------------------------------------------
# 1. Load core data + helpers
# -------------------------------------------------------------------
source(here::here("scripts", "01_core_setup.R"))

# We assume:
# - df
# - pain_vars, mesh_vars, limit_vars
# - sum_rescaled()
# - min_prop
# are all available from 01_core_setup.R

# -------------------------------------------------------------------
# 2. Compute summed scores for all three domains
# -------------------------------------------------------------------

df_all <- df %>%
  mutate(
    sum_pain  = sum_rescaled(cur_data_all(), pain_vars,  min_prop = min_prop),
    sum_mesh  = sum_rescaled(cur_data_all(), mesh_vars,  min_prop = min_prop),
    sum_limit = sum_rescaled(cur_data_all(), limit_vars, min_prop = min_prop)
  )

# Thresholds:
thr_pain  <- 16        # CPSP definition
thr_mesh  <- 16        # same absolute cutoff as pain
thr_limit <- 14        # 40% of max (35) given 7 limitation items

# -------------------------------------------------------------------
# 3. Make binary flags for each domain
# -------------------------------------------------------------------

df_all <- df_all %>%
  mutate(
    cpsp           = case_when(
      is.na(sum_pain)      ~ NA_character_,
      sum_pain  >= thr_pain  ~ "yes",
      TRUE                 ~ "no"
    ),
    mesh_positive  = case_when(
      is.na(sum_mesh)      ~ NA_character_,
      sum_mesh  >= thr_mesh  ~ "yes",
      TRUE                 ~ "no"
    ),
    limit_positive = case_when(
      is.na(sum_limit)     ~ NA_character_,
      sum_limit >= thr_limit ~ "yes",
      TRUE                 ~ "no"
    )
  )

# -------------------------------------------------------------------
# 4. Per-domain prevalence tables
# -------------------------------------------------------------------

pain_prev <- df_all %>%
  filter(!is.na(cpsp)) %>%
  summarise(
    domain    = "Pain (CPSP)",
    threshold = paste0("sum ≥ ", thr_pain),
    N         = n(),
    n_yes     = sum(cpsp == "yes"),
    n_no      = sum(cpsp == "no"),
    pct_yes   = round(100 * n_yes / N, 1),
    pct_no    = round(100 * n_no  / N, 1)
  )

mesh_prev <- df_all %>%
  filter(!is.na(mesh_positive)) %>%
  summarise(
    domain    = "Mesh sensation",
    threshold = paste0("sum ≥ ", thr_mesh),
    N         = n(),
    n_yes     = sum(mesh_positive == "yes"),
    n_no      = sum(mesh_positive == "no"),
    pct_yes   = round(100 * n_yes / N, 1),
    pct_no    = round(100 * n_no  / N, 1)
  )

limit_prev <- df_all %>%
  filter(!is.na(limit_positive)) %>%
  summarise(
    domain    = "Functional limitation",
    threshold = paste0("sum ≥ ", thr_limit),
    N         = n(),
    n_yes     = sum(limit_positive == "yes"),
    n_no      = sum(limit_positive == "no"),
    pct_yes   = round(100 * n_yes / N, 1),
    pct_no    = round(100 * n_no  / N, 1)
  )

# -------------------------------------------------------------------
# 5. Combined prevalence table
# -------------------------------------------------------------------

domain_prevalence <- bind_rows(pain_prev, mesh_prev, limit_prev) %>%
  select(domain, threshold, N, n_yes, pct_yes)

cat("Combined prevalence table (pain, mesh, limitation):\n")
print(domain_prevalence)

# Save combined table
if (!dir.exists(here::here("outputs"))) {
  dir.create(here::here("outputs"), recursive = TRUE)
}

write_csv(domain_prevalence,
          here::here("outputs", "domain_prevalence_summary.csv"))

# -------------------------------------------------------------------
# 6. Distribution tables for each domain (score histograms)
# -------------------------------------------------------------------

pain_dist <- df_all %>%
  filter(!is.na(sum_pain)) %>%
  count(sum_pain, name = "n") %>%
  mutate(
    domain   = "Pain (CPSP)",
    prop     = n / sum(n),
    pct      = round(100 * prop, 1)
  )

mesh_dist <- df_all %>%
  filter(!is.na(sum_mesh)) %>%
  count(sum_mesh, name = "n") %>%
  mutate(
    domain   = "Mesh sensation",
    prop     = n / sum(n),
    pct      = round(100 * prop, 1)
  )

limit_dist <- df_all %>%
  filter(!is.na(sum_limit)) %>%
  count(sum_limit, name = "n") %>%
  mutate(
    domain   = "Functional limitation",
    prop     = n / sum(n),
    pct      = round(100 * prop, 1)
  )

write_csv(pain_dist,
          here::here("outputs", "pain_score_distribution.csv"))
write_csv(mesh_dist,
          here::here("outputs", "mesh_score_distribution.csv"))
write_csv(limit_dist,
          here::here("outputs", "limit_score_distribution.csv"))

# -------------------------------------------------------------------
# 7. (Optional) Simple combined plot for all three domains
# -------------------------------------------------------------------

all_dist_long <- bind_rows(
  pain_dist  %>% rename(score = sum_pain),
  mesh_dist  %>% rename(score = sum_mesh),
  limit_dist %>% rename(score = sum_limit)
)

p_all <- ggplot(all_dist_long, aes(x = score, y = prop)) +
  geom_col(color = "black", fill = "grey70") +
  facet_wrap(~ domain, scales = "free_x") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Summed CCS Scores Across Domains",
    x     = "Summed domain score",
    y     = "Proportion of patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

if (!dir.exists(here::here("figures"))) {
  dir.create(here::here("figures"), recursive = TRUE)
}

ggsave(here::here("figures", "all_domains_score_distribution.png"),
       p_all, width = 9, height = 5, dpi = 300)

cat("Combined export complete. Files written to 'outputs/' and 'figures/'.\n")