# Purpose: fit the linear mixed models for the repeated outcomes, overall and
#   with subgroup interactions, in the ITT and per-protocol populations, and
#   take the tables and the effect estimates from each fit.
# Needs: setup.R (repeated_outcomes, mixed_subgroup_variables,
#   child_adult_mixed_outcomes, figure_labels, population_labels);
#   functions.R (fit_mixed_model(), tabulate_model_terms(),
#   estimate_effects(), tabulate_effects()); plot_style.R (figure_label());
#   03_derive_medication_outcomes.R (clean_asthma_data_itt,
#   clean_asthma_data_pp, with interval controller PDC added).
# Produces: mixed_model_fits (fitted models), mixed_model_tables
#   (gtsummary tables of their terms), mixed_model_estimates (cell means and
#   effects from estimate_effects()) and mixed_model_effect_tables
#   (gtsummary tables of those effects), each a named list indexed by
#   population ("itt", "pp"), then outcome, then "overall" or the subgroup
#   variable. Each model is fitted once (issue #10).
# Ported from the table loops after the outcome_functions chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd. In the RMD each table was printed into
#   the document; here they are kept in a named list.

## ---- mixed-model-data
mixed_model_data <- list(
  itt = clean_asthma_data_itt,
  pp = clean_asthma_data_pp
)

mixed_model_fits <- list()
mixed_model_tables <- list()
mixed_model_estimates <- list()
mixed_model_effect_tables <- list()

## ---- mixed-models-overall
# Visit by arm, for each outcome
for (population in names(mixed_model_data)) {
  mixed_model_fits[[population]] <- list()
  mixed_model_tables[[population]] <- list()
  mixed_model_estimates[[population]] <- list()
  mixed_model_effect_tables[[population]] <- list()

  for (outcome in repeated_outcomes) {
    fit <- fit_mixed_model(mixed_model_data[[population]], outcome)
    estimates <- estimate_effects(fit)
    caption <- paste0(
      figure_label(outcome), ", ", population_labels[[population]]
    )

    mixed_model_fits[[population]][[outcome]] <- list(overall = fit)
    mixed_model_tables[[population]][[outcome]] <- list(
      overall = tabulate_model_terms(
        fit, paste0(caption, ": model terms"), labels = figure_labels
      )
    )
    mixed_model_estimates[[population]][[outcome]] <- list(
      overall = estimates
    )
    mixed_model_effect_tables[[population]][[outcome]] <- list(
      overall = tabulate_effects(
        estimates, paste0(caption, ": intervention vs control")
      )
    )
  }
}

## ---- mixed-models-subgroups
# Visit by arm by subgroup, for each outcome; child or adult only for
# outcomes measured in both groups
subgroup_analyses <- c(
  lapply(repeated_outcomes, function(outcome) {
    list(outcome = outcome, subgroups = mixed_subgroup_variables)
  }),
  lapply(child_adult_mixed_outcomes, function(outcome) {
    list(outcome = outcome, subgroups = "child_adult")
  })
)

for (population in names(mixed_model_data)) {
  for (analysis in subgroup_analyses) {
    outcome <- analysis$outcome

    for (subgroup in analysis$subgroups) {
      fit <- fit_mixed_model(
        mixed_model_data[[population]], outcome,
        modifier = subgroup
      )

      estimates <- estimate_effects(fit, modifier = subgroup)
      caption <- paste0(
        figure_label(outcome), " by ", tolower(figure_label(subgroup)), ", ",
        population_labels[[population]]
      )

      mixed_model_fits[[population]][[outcome]][[subgroup]] <- fit
      mixed_model_tables[[population]][[outcome]][[subgroup]] <-
        tabulate_model_terms(
          fit, paste0(caption, ": model terms"), labels = figure_labels
        )
      mixed_model_estimates[[population]][[outcome]][[subgroup]] <- estimates
      mixed_model_effect_tables[[population]][[outcome]][[subgroup]] <-
        tabulate_effects(
          estimates, paste0(caption, ": intervention vs control"),
          modifier = subgroup
        )
    }
  }
}
