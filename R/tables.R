# Purpose: build Table 1 and the count-outcome summary, and save every table
#   to tables/ as a Word file.
# Needs: setup.R (count_outcomes); 03_derive_medication_outcomes.R
#   (clean_asthma_data_itt, rescue_medication_merged);
#   04_fit_count_models.R and 05_fit_mixed_models.R (their tables).
# Produces: table_1, count_outcome_summary; .docx files in tables/, which is
#   not committed (made from study data, not yet cleared; AGENTS.md).
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

## ---- export-tables
# Every table as a Word file in tables/, one file per table, named for the
# list it comes from (issue #8)
save_table <- function(table, name) {
  if (inherits(table, "gtsummary")) {
    table <- as_flex_table(table)
  }
  save_as_docx(table, path = file.path("tables", paste0(name, ".docx")))
}

single_tables <- list(
  table_1 = table_1,
  count_outcome_summary = count_outcome_summary,
  count_model_table = count_model_table,
  rescue_puffs_model_alt_table = rescue_puffs_model_alt_table,
  rescue_days_model_alt_table = rescue_days_model_alt_table,
  controller_pdc_comparison = controller_pdc_comparison
)
for (name in names(single_tables)) {
  save_table(single_tables[[name]], name)
}

for (outcome in names(count_interaction_tables)) {
  for (subgroup in names(count_interaction_tables[[outcome]])) {
    save_table(
      count_interaction_tables[[outcome]][[subgroup]],
      paste("count_interaction", outcome, subgroup, sep = "_")
    )
  }
}

for (population in names(mixed_model_tables)) {
  for (outcome in names(mixed_model_tables[[population]])) {
    for (analysis in names(mixed_model_tables[[population]][[outcome]])) {
      save_table(
        mixed_model_tables[[population]][[outcome]][[analysis]],
        paste("mixed_model_terms", population, outcome, analysis, sep = "_")
      )
      save_table(
        mixed_model_effect_tables[[population]][[outcome]][[analysis]],
        paste("mixed_model_effects", population, outcome, analysis, sep = "_")
      )
    }
  }
}
