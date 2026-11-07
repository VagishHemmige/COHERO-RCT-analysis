# Purpose: fit the negative binomial models for the count outcomes (12-month
#   health care use and rescue inhaler use), overall and with subgroup
#   interactions, and compare controller PDC between arms.
# Needs: setup.R (count_outcomes, count_subgroup_variables,
#   act_subgroup_count_outcomes); 03_derive_medication_outcomes.R
#   (rescue_medication_merged, controller_medication_merged).
# Produces: count_model_table (one stacked gtsummary table),
#   count_interaction_tables (named list: outcome, then subgroup variable),
#   rescue_puffs_model_alt_table, rescue_days_model_alt_table (see issue #1),
#   controller_pdc_comparison.
# Ported from the CountAndBinaryOutcomes, CountAndBinaryOutcomes2 and
#   medication chunks of MR_SJ_asthma_pilot_2023-9-15.Rmd. In the RMD the
#   interaction tables were printed; here they are kept in a named list. The
#   outcome summary and plots go to tables.R and figures.R; the
#   hospitalization-by-ACT model that was left out is in
#   R/exploratory/check_count_models.R.

## ---- count-models
# Negative binomial regression of each count on arm, with log follow-up time
# as an offset; incidence rate ratios, one row per outcome
count_model_table <- NULL

for (outcome in count_outcomes) {
  outcome_formula <- paste0(
    outcome, " ~ ", "studyarm + offset(log(followup_time_itt))"
  ) %>%
    as.formula()

  outcome_table <- glm.nb(outcome_formula, data = rescue_medication_merged) %>%
    tbl_regression(
      exp = TRUE,
      show_single_row = "studyarm",
      label = list(studyarm ~ outcome)
    ) %>%
    modify_header(update = list(label ~ "**Outcome**"))

  if (is.null(count_model_table)) {
    count_model_table <- outcome_table
  } else {
    count_model_table <- tbl_stack(list(count_model_table, outcome_table))
  }
}

## ---- count-interaction-models
# Arm-by-subgroup interactions for each count outcome
count_interaction_tables <- list()

for (outcome in count_outcomes) {
  count_interaction_tables[[outcome]] <- list()

  for (subgroup in count_subgroup_variables) {
    outcome_formula <- paste0(
      outcome, " ~ ", "studyarm*", subgroup,
      " + offset(log(followup_time_itt))"
    ) %>%
      as.formula()

    count_interaction_tables[[outcome]][[subgroup]] <-
      glm.nb(outcome_formula, data = rescue_medication_merged) %>%
      tbl_regression(exp = TRUE)
  }
}

# Baseline asthma control, not fitted for hospitalizations
for (outcome in act_subgroup_count_outcomes) {
  for (subgroup in c("act_binary")) {
    outcome_formula <- paste0(
      outcome, " ~ ", "studyarm*", subgroup,
      " + offset(log(followup_time_itt))"
    ) %>%
      as.formula()

    count_interaction_tables[[outcome]][[subgroup]] <-
      glm.nb(outcome_formula, data = rescue_medication_merged) %>%
      tbl_regression(exp = TRUE) %>%
      modify_caption(paste0("**", outcome, "**"))
  }
}

## ---- rescue-models-alternative
# A second fit of the rescue models from the RMD's medication chunk. As
# written, log(offset(...)) enters follow-up time as a covariate, not an
# offset. See issue #1.
rescue_puffs_model_alt_table <- glm.nb(
  rescue_puffs_itt ~ studyarm + log(offset(followup_time_itt)),
  data = rescue_medication_merged
) %>%
  tidy(., conf.int = TRUE, exp = TRUE) %>%
  flextable()

rescue_days_model_alt_table <- glm.nb(
  rescue_days_itt ~ studyarm + log(offset(followup_time_itt)),
  data = rescue_medication_merged
) %>%
  tidy(., conf.int = TRUE, exp = TRUE) %>%
  flextable()

## ---- controller-pdc-comparison
# Controller PDC for the year by arm, with gtsummary's default test
controller_pdc_comparison <- controller_medication_merged %>%
  select(controller_pdc_pp, controller_pdc_itt, studyarm) %>%
  tbl_summary(by = studyarm) %>%
  add_p()
