# Purpose: reusable helper functions. Each takes its inputs as arguments.
# Needs: setup.R (packages). Produces: functions only.
# Ported from the outcome_functions chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd. create_me_plot() follows with
#   figures.R.

## ---- create-me-table
# Mixed model of an outcome on visit (factor) by arm, with a random intercept
# for participant, returned as a flextable of the tidied coefficients. With
# an interaction variable, the model is visit by arm by that variable.
create_me_table <- function(trial_data, outcome_of_interest,
                            interaction_variable = NULL) {
  # Primary analysis without interactions if interaction_variable is NULL
  if (is.null(interaction_variable)) {
    lmer(
      get(outcome_of_interest) ~ time_factor * studyarm +
        (1 | study_id_screening),
      data = trial_data
    ) %>%
      tidy(., conf.int = TRUE) %>%
      flextable() %>%
      set_caption(paste0(outcome_of_interest, " vs. time")) %>%
      return()
  } else {
    lmer_formula <- paste0(
      outcome_of_interest, "~time_factor*studyarm*", interaction_variable,
      "+(1|study_id_screening)"
    ) %>%
      as.formula()

    lmer(lmer_formula, data = trial_data) %>%
      tidy(., conf.int = TRUE) %>%
      flextable() %>%
      set_caption(paste0(
        outcome_of_interest, " vs. time * ", interaction_variable
      )) %>%
      return()
  }
}

## ---- create-me-plot
# Spaghetti plot of an outcome over time by arm: individual trajectories,
# model-estimated means with 95% confidence intervals, numbers observed, and
# p values for the arm difference at each visit. With an interaction
# variable (a two-level factor), one panel per level plus a row of p values
# for the difference between levels, combined with patchwork. The function
# refits its own models; see the efficiency issue.
create_me_plot <- function(trial_data, outcome_of_interest,
                           interaction_variable = NULL,
                           plot_title = "Plot") {
  if (is.null(interaction_variable)) {
    # Top of the plot
    y_max <- trial_data %>%
      select(all_of(outcome_of_interest)) %>%
      max(., na.rm = TRUE) + 3

    lmer_formula_1 <- paste0(
      outcome_of_interest, "~time_factor*studyarm+(1|study_id_screening)"
    ) %>%
      as.formula()
    lmer_formula_2 <- paste0(
      outcome_of_interest, "~ me_factor-1 + (1|study_id_screening)"
    ) %>%
      as.formula()

    # Mixed model with time as a factor: p values for the arm difference
    mixed_model_a <- lmer(lmer_formula_1, data = trial_data)

    # Same model, parameterized by visit-by-arm cell: no p values for the
    # arm difference, but a mean and confidence interval for each cell
    mixed_model_b <- lmer(lmer_formula_2, data = trial_data)

    mixed_model_a_p_values <- mixed_model_a %>%
      tidy(., conf.int = TRUE) %>%
      filter(str_detect(term, "studyarm")) %>%
      mutate(time_linear = case_when(
        str_detect(term, "time_factor2") == TRUE ~ 2,
        str_detect(term, "time_factor4") == TRUE ~ 4,
        str_detect(term, "time_factor6") == TRUE ~ 6,
        str_detect(term, "time_factor9") == TRUE ~ 9,
        str_detect(term, "time_factor12") == TRUE ~ 12,
        TRUE ~ 0
      )) %>%
      mutate(p.value = round(p.value, 3)) %>%
      mutate(p_value_label = paste0("p=", p.value)) %>%
      mutate(p_value_label = ifelse(
        time_linear == 0,
        p_value_label,
        paste0(p_value_label, "†")
      )) %>%
      select(time_linear, p_value_label)

    # Cell means from model B. Terms read "me_factortime_<t>_studyarm_<a>":
    # the extra NA allows for the underscore in "me_factor".
    mixed_model_predictions <- mixed_model_b %>%
      tidy(., conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high) %>%
      filter(str_detect(term, "time")) %>%
      separate(
        term,
        sep = "_",
        into = c(NA, NA, "time_linear", NA, "studyarm")
      ) %>%
      mutate(time_factor = (as.factor(time_linear))) %>%
      mutate(studyarm = as_factor(studyarm)) %>%
      mutate(time_linear = as.numeric(time_linear)) %>%
      mutate(time_linear_shifted = ifelse(
        studyarm == "1",
        time_linear + 0.25,
        time_linear - 0.25
      ))

    # Number observed in each arm at each visit
    mixed_model_predictions <- trial_data %>%
      filter(!is.na(get(outcome_of_interest))) %>%
      group_by(time_linear, studyarm) %>%
      summarize(n_obs = n()) %>%
      left_join(
        .,
        mixed_model_predictions,
        by = c("time_linear", "studyarm")
      )

    # Spaghetti plot with model means over individual trajectories
    ggplot() +
      geom_point(
        trial_data,
        mapping = aes(
          x = time_linear_shifted, y = get(outcome_of_interest),
          color = studyarm, group = study_id_screening
        ),
        alpha = 0.2
      ) +
      geom_line(
        trial_data,
        mapping = aes(
          x = time_linear_shifted, y = get(outcome_of_interest),
          color = studyarm, group = study_id_screening
        ),
        alpha = 0.2
      ) +
      geom_point(
        mixed_model_predictions,
        mapping = aes(x = time_linear_shifted, y = estimate, color = studyarm),
        size = 2
      ) +
      geom_line(
        mixed_model_predictions,
        mapping = aes(
          x = time_linear_shifted, y = estimate, color = studyarm,
          group = studyarm
        ),
        size = 2
      ) +
      geom_errorbar(
        mixed_model_predictions,
        mapping = aes(
          x = time_linear_shifted, ymin = conf.low, ymax = conf.high,
          color = studyarm
        ),
        size = 1.5
      ) +
      theme_bw() +
      geom_text(
        filter(mixed_model_predictions, studyarm == 1),
        mapping = aes(x = time_linear, y = 0, label = n_obs, color = studyarm)
      ) +
      geom_text(
        filter(mixed_model_predictions, studyarm == 0),
        mapping = aes(x = time_linear, y = 1, label = n_obs, color = studyarm)
      ) +
      geom_text(mapping = aes(x = -0.5, y = 1, label = "N")) +
      geom_text(
        mixed_model_a_p_values,
        mapping = aes(x = time_linear, y = y_max, label = p_value_label)
      ) +
      scale_x_continuous(
        breaks = c(0, 2, 4, 6, 9, 12),
        limits = c(-0.5, 12.5),
        minor_breaks = 0:12
      ) +
      ylab(outcome_of_interest) +
      xlab("Time (months)") +
      scale_color_discrete(
        name = "Study Arm",
        labels = c("Control", "Treatment")
      ) +
      labs(caption = "† adjusted for baseline differences") +
      theme(plot.caption = element_text(hjust = 0.0555)) +
      ggtitle(plot_title)
  } else {
    # The interaction variable must be a two-level factor
    plot_labels <- trial_data %>%
      pull(interaction_variable) %>%
      levels()
    y_max <- trial_data %>%
      select(all_of(outcome_of_interest)) %>%
      max(., na.rm = TRUE) + 3

    data_for_function <- trial_data %>%
      mutate(interaction_variable_numeric = factor(
        as.numeric(get(interaction_variable))
      )) %>%
      mutate(me_factor_interaction_variable = paste(
        "time", time_linear, "studyarm", studyarm, interaction_variable,
        interaction_variable_numeric,
        sep = "__"
      ))

    # Formulas 1 and 2 are defined but not fitted
    lmer_formula_1 <- paste0(
      outcome_of_interest,
      "~time_linear*studyarm*interaction_variable_numeric",
      "+(1|study_id_screening)"
    ) %>%
      as.formula()
    lmer_formula_2 <- paste0(
      outcome_of_interest,
      "~time_linear*studyarm*interaction_variable_numeric",
      "+(time_linear|study_id_screening)"
    ) %>%
      as.formula()
    lmer_formula_3 <- paste0(
      outcome_of_interest,
      "~time_factor*studyarm*interaction_variable_numeric",
      "+(1|study_id_screening)"
    ) %>%
      as.formula()
    lmer_formula_4 <- paste0(
      outcome_of_interest,
      " ~ me_factor_interaction_variable-1 + (1|study_id_screening)"
    ) %>%
      as.formula()
    lmer_formula_5 <- paste0(
      outcome_of_interest,
      "~interaction_variable_numeric-1 + ",
      "interaction_variable_numeric:time_factor + ",
      "interaction_variable_numeric:studyarm + ",
      "time_factor:studyarm:interaction_variable_numeric +",
      "(1|study_id_screening)"
    ) %>%
      as.formula()

    # Model A, time as a factor: p values for the difference in arm effect
    # between levels of the interaction variable
    mixed_model_a_interaction <- lmer(
      lmer_formula_3,
      data = data_for_function
    )

    mixed_model_a_heterogeneity <- mixed_model_a_interaction %>%
      tidy(., conf.int = TRUE) %>%
      filter(str_detect(term, "studyarm")) %>%
      filter(str_detect(term, "interaction_variable_numeric")) %>%
      mutate(time_linear = case_when(
        str_detect(term, "time_factor2") == TRUE ~ 2,
        str_detect(term, "time_factor4") == TRUE ~ 4,
        str_detect(term, "time_factor6") == TRUE ~ 6,
        str_detect(term, "time_factor9") == TRUE ~ 9,
        str_detect(term, "time_factor12") == TRUE ~ 12,
        TRUE ~ 0
      )) %>%
      mutate(p.value = round(p.value, 3)) %>%
      mutate(p_value_label = paste0("p=", p.value)) %>%
      select(time_linear, p_value_label)

    # Model B, parameterized by cell: a mean and confidence interval for each
    # visit, arm and level
    mixed_model_b_interaction <- lmer(
      lmer_formula_4,
      data = data_for_function
    )

    # (In the original, a table of model B, printed inside the function and
    # not returned)
    mixed_model_b_interaction %>%
      tidy(., conf.int = TRUE) %>%
      flextable()

    # Summary statistics confirm model B is the same model as A
    glance(mixed_model_b_interaction)

    # Model C, parameterized for the arm difference within each level
    mixed_model_c_interaction <- lmer(
      lmer_formula_5,
      data = data_for_function
    )

    # (In the original, a table of model C, printed inside the function and
    # not returned)
    mixed_model_c_interaction %>%
      tidy(., conf.int = TRUE) %>%
      flextable()

    # Summary statistics confirm model C is the same model as A
    glance(mixed_model_c_interaction)

    # P values from model C, nested by level
    mixed_model_c_treatment_effect <- mixed_model_c_interaction %>%
      tidy(., conf.int = TRUE) %>%
      filter(str_detect(term, "studyarm")) %>%
      mutate(interaction_variable = case_when(
        str_detect(term, "interaction_variable_numeric1") == TRUE ~ 1,
        str_detect(term, "interaction_variable_numeric2") == TRUE ~ 2
      )) %>%
      mutate(time_linear = case_when(
        str_detect(term, "time_factor2") == TRUE ~ 2,
        str_detect(term, "time_factor4") == TRUE ~ 4,
        str_detect(term, "time_factor6") == TRUE ~ 6,
        str_detect(term, "time_factor9") == TRUE ~ 9,
        str_detect(term, "time_factor12") == TRUE ~ 12,
        TRUE ~ 0
      )) %>%
      mutate(p.value = round(p.value, 3)) %>%
      mutate(p_value_label = paste0("p=", p.value)) %>%
      mutate(p_value_label = ifelse(
        time_linear == 0,
        p_value_label,
        paste0(p_value_label, "†")
      )) %>%
      select(interaction_variable, time_linear, p_value_label) %>%
      group_by(interaction_variable) %>%
      nest()

    # Cell means from model B
    predictions_interaction <- mixed_model_b_interaction %>%
      tidy(., conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high) %>%
      filter(str_detect(term, "time")) %>%
      separate(
        term,
        sep = "__",
        into = c(NA, "time_linear", NA, "studyarm", NA, "interaction_term")
      ) %>%
      mutate(time_factor = (as.factor(time_linear))) %>%
      mutate(studyarm = as_factor(studyarm)) %>%
      mutate(time_linear = as.numeric(time_linear)) %>%
      mutate(time_linear_shifted_1 = case_when(
        studyarm == "0" & interaction_term == "1" ~ time_linear - 0.5,
        studyarm == "1" & interaction_term == "1" ~ time_linear - 0.25,
        studyarm == "0" & interaction_term == "2" ~ time_linear + 0.25,
        studyarm == "1" & interaction_term == "2" ~ time_linear + 0.5
      )) %>%
      mutate(time_linear_shifted_2 = case_when(
        studyarm == "0" & interaction_term == "1" ~ time_linear - 0.25,
        studyarm == "1" & interaction_term == "1" ~ time_linear + 0.25,
        studyarm == "0" & interaction_term == "2" ~ time_linear - 0.25,
        studyarm == "1" & interaction_term == "2" ~ time_linear + 0.25
      )) %>%
      mutate(interaction_variable = as.factor(interaction_term)) %>%
      select(-interaction_term)

    # Number observed in each arm and level at each visit
    predictions_interaction <- data_for_function %>%
      filter(!is.na(get(outcome_of_interest))) %>%
      group_by(time_linear, studyarm, interaction_variable_numeric) %>%
      summarize(n_obs = n()) %>%
      rename(interaction_variable = interaction_variable_numeric) %>%
      left_join(
        .,
        predictions_interaction,
        by = c("time_linear", "studyarm", "interaction_variable")
      ) %>%
      group_by(studyarm, interaction_variable) %>%
      mutate(gid = cur_group_id())

    # Data and predictions nested by level
    data_nested_interaction <- data_for_function %>%
      group_by(interaction_variable_numeric) %>%
      nest() %>%
      rename(interaction_variable = interaction_variable_numeric)

    predictions_nested_interaction <- predictions_interaction %>%
      group_by(interaction_variable) %>%
      nest()

    # (In the original, the predictions table, printed inside the function
    # and not returned)
    predictions_interaction %>% flextable()

    # Panel for the first level
    plot_level_1 <- ggplot() +
      geom_point(
        data_nested_interaction$data[[1]],
        mapping = aes(
          x = time_linear_shifted, y = get(outcome_of_interest),
          color = studyarm, group = study_id_screening
        ),
        alpha = 0.2
      ) +
      geom_line(
        data_nested_interaction$data[[1]],
        mapping = aes(
          x = time_linear_shifted, y = get(outcome_of_interest),
          color = studyarm, group = study_id_screening
        ),
        alpha = 0.2
      ) +
      geom_point(
        predictions_nested_interaction$data[[1]],
        mapping = aes(
          x = time_linear_shifted_2, y = estimate, color = studyarm
        ),
        size = 2
      ) +
      geom_line(
        predictions_nested_interaction$data[[1]],
        mapping = aes(
          x = time_linear_shifted_2, y = estimate, color = studyarm,
          group = gid
        ),
        size = 2
      ) +
      theme_bw() +
      geom_text(
        filter(
          predictions_interaction,
          studyarm == 0,
          interaction_variable == 1
        ),
        mapping = aes(x = time_linear, y = 1, label = n_obs, color = studyarm)
      ) +
      geom_text(
        filter(
          predictions_interaction,
          studyarm == 1,
          interaction_variable == 1
        ),
        mapping = aes(x = time_linear, y = -1, label = n_obs, color = studyarm)
      ) +
      geom_text(mapping = aes(x = -1, y = 1, label = "N")) +
      geom_text(mapping = aes(x = -1, y = -1, label = plot_labels[1])) +
      geom_text(
        mixed_model_c_treatment_effect$data[[1]],
        mapping = aes(x = time_linear, y = y_max, label = p_value_label)
      ) +
      geom_errorbar(
        predictions_nested_interaction$data[[1]],
        mapping = aes(
          x = time_linear_shifted_2, ymin = conf.low, ymax = conf.high,
          color = studyarm
        ),
        size = 1.5
      ) +
      scale_x_continuous(
        breaks = NULL,
        limits = c(-1, 12.5),
        minor_breaks = 0:12
      ) +
      scale_y_continuous(limits = c(-3, y_max)) +
      theme(axis.title.x = element_blank()) +
      ylab(outcome_of_interest) +
      scale_color_discrete(
        name = "Study Arm",
        labels = c("Control", "Treatment")
      )

    # Panel for the second level
    plot_level_2 <- ggplot() +
      geom_point(
        data_nested_interaction$data[[2]],
        mapping = aes(
          x = time_linear_shifted, y = get(outcome_of_interest),
          color = studyarm, group = study_id_screening
        ),
        alpha = 0.2
      ) +
      geom_line(
        data_nested_interaction$data[[2]],
        mapping = aes(
          x = time_linear_shifted, y = get(outcome_of_interest),
          color = studyarm, group = study_id_screening
        ),
        alpha = 0.2
      ) +
      geom_point(
        predictions_nested_interaction$data[[2]],
        mapping = aes(
          x = time_linear_shifted_2, y = estimate, color = studyarm
        ),
        size = 2
      ) +
      geom_line(
        predictions_nested_interaction$data[[2]],
        mapping = aes(
          x = time_linear_shifted_2, y = estimate, color = studyarm,
          group = gid
        ),
        size = 2
      ) +
      theme_bw() +
      geom_text(
        filter(
          predictions_interaction,
          studyarm == 0,
          interaction_variable == 2
        ),
        mapping = aes(x = time_linear, y = 1, label = n_obs, color = studyarm)
      ) +
      geom_text(
        filter(
          predictions_interaction,
          studyarm == 1,
          interaction_variable == 2
        ),
        mapping = aes(x = time_linear, y = -1, label = n_obs, color = studyarm)
      ) +
      geom_text(mapping = aes(x = -1, y = 1, label = "N")) +
      geom_text(mapping = aes(x = -1, y = -1, label = plot_labels[2])) +
      geom_text(
        mixed_model_c_treatment_effect$data[[2]],
        mapping = aes(x = time_linear, y = y_max, label = p_value_label)
      ) +
      geom_errorbar(
        predictions_nested_interaction$data[[2]],
        mapping = aes(
          x = time_linear_shifted_2, ymin = conf.low, ymax = conf.high,
          color = studyarm
        ),
        size = 1.5
      ) +
      scale_x_continuous(
        breaks = NULL,
        limits = c(-1, 12.5),
        minor_breaks = 0:12
      ) +
      scale_y_continuous(limits = c(-3, y_max)) +
      theme(axis.title.x = element_blank()) +
      ylab(outcome_of_interest) +
      scale_color_discrete(
        name = "Study Arm",
        labels = c("Control", "Treatment")
      )

    # Row of p values for the difference between levels
    plot_p_values <- ggplot() +
      geom_text(
        data = mixed_model_a_heterogeneity,
        mapping = aes(x = time_linear, y = 0, label = p_value_label)
      ) +
      theme_void() +
      scale_x_continuous(limits = c(-1, 12.5))

    # Combine the panels with patchwork
    plot_level_1 / plot_level_2 / plot_p_values +
      plot_layout(guides = "collect", heights = c(0.48, 0.48, 0.04)) +
      plot_annotation(
        title = plot_title,
        caption = "† adjusted for baseline differences",
        theme = theme(plot.caption = element_text(hjust = 0.0555))
      )
  }
}
