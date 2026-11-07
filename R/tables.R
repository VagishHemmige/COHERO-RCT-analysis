# Purpose: build Table 1 and the count-outcome summary.
# Needs: setup.R (count_outcomes); 03_medication.R (clean_asthma_data_itt,
#   rescue_medication_merged).
# Produces: table_1, count_outcome_summary. Nothing is written to disk yet;
#   exporting tables is deferred (see #8).
# Ported from the table1 and CountAndBinaryOutcomes chunks of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd.

## ---- table-1
# Baseline characteristics by arm, ITT population
table_1 <- clean_asthma_data_itt %>%
  filter(time_factor == "0") %>%
  select(
    studyarm, participant_sex, child_adult, prefer_language, age_screening,
    act_score, aqlq_score, pacqlq_score, afs_score, self_reported_adherence
  ) %>%
  mutate(studyarm = factor(
    studyarm,
    levels = c(0, 1),
    labels = c("Control", "Intervention")
  )) %>%
  tbl_summary(
    by = studyarm,
    label = list(
      participant_sex ~ "Sex",
      child_adult ~ "Child or adult",
      prefer_language ~ "Preferred language",
      age_screening ~ "Age at screening (years)",
      act_score ~ "Baseline ACT",
      aqlq_score ~ "Baseline AQLQ score (adults)",
      pacqlq_score ~ "Baseline PACQLQ score (children)",
      afs_score ~ "Baseline AFS score",
      self_reported_adherence ~ "Baseline self-reported adherence score"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Patient Characteristics** (N = {N})") %>%
  as_flex_table()

## ---- count-outcome-summary
# Count outcomes by arm
count_outcome_summary <- rescue_medication_merged %>%
  select(all_of(count_outcomes), studyarm) %>%
  tbl_summary(by = studyarm)
