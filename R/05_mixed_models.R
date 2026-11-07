# Purpose: fit the linear mixed models for the repeated outcomes, overall and
#   with subgroup interactions, in the ITT and per-protocol populations.
# Needs: setup.R (repeated_outcomes, mixed_subgroup_variables,
#   child_adult_mixed_outcomes); functions.R (create_me_table());
#   03_medication.R (clean_asthma_data_itt, clean_asthma_data_pp, with
#   interval controller PDC added).
# Produces: mixed_model_tables, a named list of flextables indexed by
#   population ("itt", "pp"), then outcome, then "overall" or the subgroup
#   variable.
# Ported from the table loops after the outcome_functions chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd. In the RMD each table was printed into
#   the document; here they are kept in a named list.

## ---- mixed-model-data
mixed_model_data <- list(
  itt = clean_asthma_data_itt,
  pp = clean_asthma_data_pp
)

mixed_model_tables <- list()

## ---- mixed-models-overall
# Visit by arm, for each outcome
for (population in names(mixed_model_data)) {
  mixed_model_tables[[population]] <- list()

  for (outcome in repeated_outcomes) {
    mixed_model_tables[[population]][[outcome]] <- list()

    mixed_model_tables[[population]][[outcome]][["overall"]] <-
      create_me_table(
        mixed_model_data[[population]],
        outcome_of_interest = outcome
      )
  }
}

## ---- mixed-models-subgroups
# Visit by arm by subgroup, for each outcome
for (population in names(mixed_model_data)) {
  for (outcome in repeated_outcomes) {
    for (subgroup in mixed_subgroup_variables) {
      mixed_model_tables[[population]][[outcome]][[subgroup]] <-
        create_me_table(
          mixed_model_data[[population]],
          outcome_of_interest = outcome,
          interaction_variable = subgroup
        )
    }
  }
}

# Child or adult, only for outcomes measured in both groups
for (population in names(mixed_model_data)) {
  for (outcome in child_adult_mixed_outcomes) {
    for (subgroup in c("child_adult")) {
      mixed_model_tables[[population]][[outcome]][[subgroup]] <-
        create_me_table(
          mixed_model_data[[population]],
          outcome_of_interest = outcome,
          interaction_variable = subgroup
        )
    }
  }
}
