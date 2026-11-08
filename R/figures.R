# Purpose: build the figures: ACT by visit, count outcomes, rescue and
#   controller use, and the mixed-model plots for every outcome and subgroup.
# Needs: setup.R (count_outcomes, repeated_outcomes, population_labels);
#   functions.R (plot_effect_trajectories());
#   03_derive_medication_outcomes.R (clean_asthma_data_itt,
#   clean_asthma_data_pp, rescue_medication_merged,
#   controller_medication_merged); 05_fit_mixed_models.R (mixed_model_data,
#   mixed_model_estimates, subgroup_analyses).
# Produces: act_plots, count_outcome_plots, medication_plots (named lists of
#   ggplot objects); mixed_model_plots (named list indexed by population,
#   outcome, then "overall" or the subgroup variable). Nothing is written to
#   disk yet.
# Ported from the initialgraphs, CountAndBinaryOutcomes, medication and
#   mixed-model plot chunks of MR_SJ_asthma_pilot_2023-9-15.Rmd. In the RMD
#   each plot was printed into the document; here they are kept in named
#   lists. The animations are in R/exploratory/animate_trajectories.R.

## ---- act-plots
act_plots <- list()

# ACT at each visit by arm
act_plots[["act_by_visit_itt"]] <- clean_asthma_data_itt %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = act_score, color = studyarm
  )) +
  facet_wrap(
    ~time_factor, nrow = 1,
    labeller = labeller(time_factor = function(month) paste("Month", month))
  ) +
  theme_publication() +
  label_figure() +
  # The legend names the arms; six narrow panels leave no room on the axis
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

act_plots[["act_by_visit_pp"]] <- clean_asthma_data_pp %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = act_score, color = studyarm
  )) +
  facet_wrap(
    ~time_factor, nrow = 1,
    labeller = labeller(time_factor = function(month) paste("Month", month))
  ) +
  theme_publication() +
  label_figure() +
  # The legend names the arms; six narrow panels leave no room on the axis
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

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
  theme_publication() +
  label_figure()

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
  theme_publication() +
  label_figure()

# Self-reported adherence vs interval controller PDC
act_plots[["adherence_vs_pdc_itt"]] <- clean_asthma_data_itt %>%
  ggplot() +
  geom_point(mapping = aes(
    x = self_reported_adherence, y = controller_pdc_quarter_itt,
    color = studyarm
  )) +
  theme_publication() +
  label_figure()

act_plots[["adherence_vs_pdc_pp"]] <- clean_asthma_data_pp %>%
  ggplot() +
  geom_point(mapping = aes(
    x = self_reported_adherence, y = controller_pdc_quarter_pp,
    color = studyarm
  )) +
  theme_publication() +
  label_figure()

## ---- count-outcome-plots
# Each count outcome by arm, to check that outliers have been dealt with
count_outcome_plots <- list()

for (outcome in count_outcomes) {
  count_outcome_plots[[outcome]] <- rescue_medication_merged %>%
    ggplot() +
    geom_quasirandom(mapping = aes(
      x = studyarm, y = .data[[outcome]], color = studyarm
    )) +
    theme_publication() +
    label_figure(arm_axis = TRUE)
}

## ---- medication-plots
medication_plots <- list()

# Rescue puffs, linear and log scale
medication_plots[["rescue_puffs"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_puffs_itt, color = studyarm
  )) +
  theme_publication() +
  label_figure(arm_axis = TRUE)

medication_plots[["rescue_puffs_log"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_puffs_itt, color = studyarm
  )) +
  theme_publication() +
  label_figure(arm_axis = TRUE) +
  scale_y_log10()

# Rescue days, linear and log scale
medication_plots[["rescue_days"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_days_itt, color = studyarm
  )) +
  theme_publication() +
  label_figure(arm_axis = TRUE)

medication_plots[["rescue_days_log"]] <- rescue_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = rescue_days_itt, color = studyarm
  )) +
  theme_publication() +
  label_figure(arm_axis = TRUE) +
  scale_y_log10()

# Controller PDC for the year
medication_plots[["controller_pdc_itt"]] <- controller_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = controller_pdc_itt, color = studyarm
  )) +
  theme_publication() +
  label_figure(arm_axis = TRUE)

medication_plots[["controller_pdc_pp"]] <- controller_medication_merged %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = controller_pdc_pp, color = studyarm
  )) +
  theme_publication() +
  label_figure(arm_axis = TRUE)

## ---- mixed-model-plots-overall
# Outcome over time by arm, for each outcome, drawn from the estimates of the
# models fitted in 05_fit_mixed_models.R
mixed_model_plots <- list()

for (population in names(mixed_model_data)) {
  mixed_model_plots[[population]] <- list()

  for (outcome in repeated_outcomes) {
    plot_title <- paste0(
      figure_label(outcome), " over time, ", population_labels[[population]]
    )

    mixed_model_plots[[population]][[outcome]] <- list()
    mixed_model_plots[[population]][[outcome]][["overall"]] <-
      plot_effect_trajectories(
        mixed_model_data[[population]],
        mixed_model_estimates[[population]][[outcome]][["overall"]],
        outcome_of_interest = outcome,
        plot_title = plot_title
      )
  }
}

## ---- mixed-model-plots-subgroups
# Outcome over time by arm, stratified by each subgroup; child or adult only
# for outcomes measured in both groups (subgroup_analyses, from
# 05_fit_mixed_models.R)
for (population in names(mixed_model_data)) {
  for (analysis in subgroup_analyses) {
    outcome <- analysis$outcome

    for (subgroup in analysis$subgroups) {
      plot_title <- paste0(
        figure_label(outcome), " over time, by ",
        tolower(figure_label(subgroup)), ", ",
        population_labels[[population]]
      )

      mixed_model_plots[[population]][[outcome]][[subgroup]] <-
        plot_effect_trajectories(
          mixed_model_data[[population]],
          mixed_model_estimates[[population]][[outcome]][[subgroup]],
          outcome_of_interest = outcome,
          interaction_variable = subgroup,
          plot_title = plot_title
        )
    }
  }
}
