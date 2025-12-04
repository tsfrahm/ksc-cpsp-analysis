library(dplyr)
library(gtsummary)

# helper to convert logical TRUE/FALSE to "yes"/"no"
to_yesno <- function(x) ifelse(is.na(x), NA, ifelse(x, "yes", "no"))

df_t1 <- df %>%
  mutate(
    # demographics
    sex = factor(sex, levels = c("female","male")),
    # education left as-is (character/factor with multiple levels)
    
    # socioeconomic / occupation
    employed     = factor(employed,     levels = c("no","yes")),
    manual_labor = factor(manual_labor, levels = c("no","yes")),
    
    # preoperative
    other_disease           = factor(other_disease,           levels = c("no","yes")),  # any comorbidity
    malignancy              = factor(malignancy,              levels = c("no","yes")),
    preop_chronic_pain_any  = factor(preop_chronic_pain_any,  levels = c("no","yes")),
    preop_opioids_self      = factor(preop_opioids_self,      levels = c("no","yes")),
    smoke                   = factor(smoke,                   levels = c("no","yes")),
    alcohol                 = factor(alcohol,                 levels = c("no","yes")),
    
    # perioperative / institutional (cut intraop_complication)
    preventive_preop        = factor(preventive_preop,        levels = c("no","yes")),
    preventive_postop       = factor(preventive_postop,       levels = c("no","yes")),
    discharge_pain_education= factor(discharge_pain_education,levels = c("no","yes")),
    
    # follow-up (TRUE/FALSE → "yes"/"no")
    followed_2w = factor(to_yesno(followed_2w), levels = c("no","yes")),
    followed_1m = factor(to_yesno(followed_1m), levels = c("no","yes"))
  )

t1 <- tbl_summary(
  data = df_t1,
  include = c(
    age, bmi,                    # continuous
    sex, education,              # demographics
    employed, manual_labor,      # socioeconomic
    other_disease, malignancy, preop_chronic_pain_any, preop_opioids_self,
    smoke, alcohol,              # preoperative
    preventive_preop, preventive_postop, discharge_pain_education,  # institutional
    followed_2w, followed_1m     # follow-up
  ),
  type = list(
    age ~ "continuous",
    bmi ~ "continuous"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",
    all_continuous()  ~ "{mean} ± {sd}"
  ),
  digits = all_continuous() ~ 1,
  missing = "no",
  label = list(
    age ~ "Age, years",
    bmi ~ "BMI, kg/m²",
    sex ~ "Sex",
    education ~ "Education level",
    employed ~ "Employed",
    manual_labor ~ "Manual labor",
    other_disease ~ "Any comorbidity",
    malignancy ~ "Malignancy",
    preop_chronic_pain_any ~ "Preoperative chronic pain",
    preop_opioids_self ~ "Preoperative opioid use",
    smoke ~ "Smoker",
    alcohol ~ "Alcohol use",
    preventive_preop ~ "Preventive analgesia (preop)",
    preventive_postop ~ "Preventive analgesia (postop)",
    discharge_pain_education ~ "Discharge pain education provided",
    followed_2w ~ "Followed at 2 weeks",
    followed_1m ~ "Followed at 1 month"
  )
) %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_caption("**Table 1. Baseline characteristics of patients undergoing ambulatory inguinal hernia repair at KSC**") %>%
  bold_labels()

t1

t1

# convert your gtsummary object to a tibble
t1_export <- as_tibble(t1)

# export to Excel
write_xlsx(t1_export, "Library/CloudStorage/GoogleDrive-tanner.frahm@icahn.mssm.edu/My Drive/Research/Zhang/Chronic Pain/Table1_Baseline_Characteristics.xlsx")