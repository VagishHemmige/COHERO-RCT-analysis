# Purpose: build the figures: ACT by visit, count outcomes, rescue and
#   controller use, and the mixed-model plots for every outcome and subgroup.
# Needs: setup.R (count_outcomes, repeated_outcomes, mixed_subgroup_variables,
#   child_adult_mixed_outcomes, population_labels); functions.R
#   (create_me_plot()); 03_medication.R (clean_asthma_data_itt,
#   clean_asthma_data_pp, rescue_medication_merged,
#   controller_medication_merged); 05_mixed_models.R (mixed_model_data).
# Produces: act_plots, count_outcome_plots, medication_plots (named lists of
#   ggplot objects); mixed_model_plots (named list indexed by population,
#   outcome, then "overall" or the subgroup variable). Nothing is written to
#   disk yet.
# Ported from the initialgraphs, CountAndBinaryOutcomes, medication and
#   mixed-model plot chunks of MR_SJ_asthma_pilot_2023-9-15.Rmd. In the RMD
#   each plot was printed into the document; here they are kept in named
#   lists. The animations are in R/exploratory/animations.R.

## ---- act-plots
act_plots <- list()

# ACT at each visit by arm
act_plots[["act_by_visit_itt"]] <- clean_asthma_data_itt %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = act_score, color = studyarm
  )) +
  facet_wrap(~time_factor, nrow = 1) +
  theme_bw()

act_plots[["act_by_visit_pp"]] <- clean_asthma_data_pp %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = act_score, color = studyarm
  )) +
  facet_wrap(~time_factor, nrow = 1) +
  theme_bw()

# Spaghetti plots of ACT
act_plots[["act_spaghetti_itt"]] <- clean_asthma_data_itt %>%
  ggplot() +
  geom_point(mapping = aes(
    x = time_linear_shifted, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  geom_line(mapping = aes(
    x = time_linear_shifted, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  theme_bw()

act_plots[["act_spaghetti_pp"]] <- clean_asthma_data_pp %>%
  ggplot() +
  geom_point(mapping = aes(
    x = time_linear_shifted, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  geom_line(mapping = aes(
    x = time_linear_shifted, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  theme_bw()

# Self-reported adherence vs interval controller PDC
act_plots[["adherence_vs_pdc_itt"]] <- clean_asthma_data_itt %>%
  ggplot() +
  geom_point(mapping = aes(
    x = self_reported_adherence, y = controller_pdc_quarter_itt,
    color = studyarm
  ))

act_plots[["adherence_vs_pdc_pp"]] <- clean_asthma_data_pp %>%
  ggplot() +
  geom_point(mapping = aes(
    x = self_reported_adherence, y = controller_pdc_quarter_pp,
    color = studyarm
  ))

## ---- count-outcome-plots
# Each count outcome by arm, to check that outliers have been dealt with
count_outcome_plots <- list()

for (outcome in count_outcomes) {
  count_outcome_plots[[outcome]] <- rescue_medication_merged %>%
    ggplot() +
    geom_quasirandom(mapping = aes_string(
      x = "studyarm", y = outcome, color = "studyarm"
    )) +
    theme_bw()
}

## ---- medication-plots
medication_plots <- list()

# Rescue puffs, linear and log scale
medication_plots[["rescue_puffs"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_puffs_itt, color = studyarm
  )) +
  theme_bw()

medication_plots[["rescue_puffs_log"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_puffs_itt, color = studyarm
  )) +
  theme_bw() +
  scale_y_log10()

# Rescue days, linear and log scale
medication_plots[["rescue_days"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_days_itt, color = studyarm
  )) +
  theme_bw()

medication_plots[["rescue_days_log"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_days_itt, color = studyarm
  )) +
  theme_bw() +
  scale_y_log10()

# Controller PDC for the year
medication_plots[["controller_pdc_itt"]] <- controller_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = controller_pdc_itt, color = studyarm
  )) +
  theme_bw()

medication_plots[["controller_pdc_pp"]] <- controller_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = controller_pdc_pp, color = studyarm
  )) +
  theme_bw()

## ---- mixed-model-plots-overall
# Outcome over time by arm, for each outcome
mixed_model_plots <- list()

for (population in names(mixed_model_data)) {
  mixed_model_plots[[population]] <- list()

  for (outcome in repeated_outcomes) {
    plot_title <- paste(
      outcome, "vs. time in", population_labels[[population]]
    )

    mixed_model_plots[[population]][[outcome]] <- list()
    mixed_model_plots[[population]][[outcome]][["overall"]] <-
      mixed_model_data[[population]] %>%
      create_me_plot(outcome_of_interest = outcome, plot_title = plot_title)
  }
}

## ---- mixed-model-plots-subgroups
# Outcome over time by arm, stratified by each subgroup
for (population in names(mixed_model_data)) {
  for (outcome in repeated_outcomes) {
    for (subgroup in mixed_subgroup_variables) {
      plot_title <- paste(
        outcome, "vs. time, stratified by", subgroup, "in",
        population_labels[[population]]
      )

      mixed_model_plots[[population]][[outcome]][[subgroup]] <-
        mixed_model_data[[population]] %>%
        create_me_plot(
          outcome_of_interest = outcome,
          interaction_variable = subgroup,
          plot_title = plot_title
        )
    }
  }
}

# Child or adult, only for outcomes measured in both groups
for (population in names(mixed_model_data)) {
  for (outcome in child_adult_mixed_outcomes) {
    for (subgroup in c("child_adult")) {
      plot_title <- paste(
        outcome, "vs. time, stratified by", subgroup, "in",
        population_labels[[population]]
      )

      mixed_model_plots[[population]][[outcome]][[subgroup]] <-
        mixed_model_data[[population]] %>%
        create_me_plot(
          outcome_of_interest = outcome,
          interaction_variable = subgroup,
          plot_title = plot_title
        )
    }
  }
}
