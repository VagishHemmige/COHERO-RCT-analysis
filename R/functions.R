# Purpose: reusable helper functions for the repeated-outcome analyses, in
#   three steps: fit a model, estimate effects from it, report them. Each
#   takes its inputs as arguments.
# Needs: setup.R (packages). Produces: functions only.
# Ported from the outcome_functions chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd. The original fitted each model up to
#   four times in different codings; here it is fitted once and the
#   estimates come from emmeans (issue #10).
#
# Names are general: an exposure (here the study arm), a time variable and
#   an optional modifier (a subgroup), so the same functions serve any
#   exposure-by-time analysis with or without effect modification.

## ---- fit-mixed-model
# Linear mixed model of an outcome on time (as a factor) by exposure, with a
# random intercept for each participant. With a modifier, the model is time
# by exposure by modifier.
fit_mixed_model <- function(data, outcome, modifier = NULL,
                            exposure = "studyarm", time = "time_factor",
                            id = "study_id_screening") {
  fixed_effects <- paste(c(time, exposure, modifier), collapse = " * ")
  model_formula <- paste0(
    outcome, " ~ ", fixed_effects, " + (1 | ", id, ")"
  ) %>%
    as.formula()

  lmer(model_formula, data = data)
}

## ---- tabulate-model-terms
# The fitted model's coefficients as a flextable: the baseline exposure
# difference, the time-by-exposure interaction terms and, with a modifier,
# the three-way terms
tabulate_model_terms <- function(fit, caption) {
  fit %>%
    tidy(conf.int = TRUE) %>%
    flextable() %>%
    set_caption(caption)
}

## ---- estimate-effects
# Estimates from a fitted model, with emmeans: the mean in each time x
# exposure (x modifier level) cell, and for each modifier level the exposure
# effect at each time. At the first (baseline) time the effect is the
# exposure difference; at later times it is the time-by-exposure interaction,
# the change from baseline in the exposed group minus that in the reference
# group. With a modifier, the difference in that effect between each level
# and the first level. Degrees of freedom are Satterthwaite's, as in the
# model table, and p values are not adjusted for multiplicity.
estimate_effects <- function(fit, exposure = "studyarm",
                             time = "time_factor", modifier = NULL) {
  emm <- function(specs, by = NULL) {
    emmeans(fit, specs = specs, by = by,
            lmer.df = "satterthwaite", lmerTest.limit = Inf)
  }
  tidy_emm <- function(x) {
    as_tibble(as.data.frame(summary(x, infer = TRUE, adjust = "none")))
  }
  first <- "trt.vs.ctrl1"
  baseline <- ref_grid(fit)@levels[[time]][1]
  # The later time in a "t - baseline" interaction contrast
  contrast_time <- function(x) str_remove(x, " - .*$")

  means <- emm(exposure, by = c(time, modifier)) %>%
    tidy_emm() %>%
    rename(estimate = emmean, conf.low = lower.CL, conf.high = upper.CL)

  at_baseline <- emm(exposure, by = c(time, modifier)) %>%
    contrast(first) %>%
    tidy_emm() %>%
    filter(.data[[time]] == baseline)
  over_time <- emm(c(exposure, time), by = modifier) %>%
    contrast(interaction = c(first, first)) %>%
    tidy_emm() %>%
    mutate(!!time := contrast_time(.data[[paste0(time, "_", first)]]))
  within_levels <- bind_rows(at_baseline, over_time) %>%
    select(any_of(c(modifier, time)), estimate, conf.low = lower.CL,
           conf.high = upper.CL, p.value)

  between_levels <- NULL
  if (!is.null(modifier)) {
    between_at_baseline <- emm(c(exposure, modifier), by = time) %>%
      contrast(interaction = c(first, first)) %>%
      tidy_emm() %>%
      filter(.data[[time]] == baseline)
    between_over_time <- emm(c(exposure, time, modifier)) %>%
      contrast(interaction = c(first, first, first)) %>%
      tidy_emm() %>%
      mutate(!!time := contrast_time(.data[[paste0(time, "_", first)]]))
    between_levels <- bind_rows(between_at_baseline, between_over_time) %>%
      mutate(
        comparison = .data[[paste0(modifier, "_", first)]],
        .before = 1
      ) %>%
      select(comparison, all_of(time), estimate, conf.low = lower.CL,
             conf.high = upper.CL, p.value)
  }

  list(
    means = means %>%
      select(
        all_of(c(time, exposure, modifier)), estimate, conf.low, conf.high
      ),
    within_levels = within_levels,
    between_levels = between_levels
  )
}

## ---- plot-effect-trajectories
# Spaghetti plot of an outcome over time by arm: individual trajectories,
# model-estimated means with 95% confidence intervals, numbers observed, and
# p values for the arm effect at each visit (at baseline the arm difference;
# at follow-up, marked †, the difference in change from baseline). With a
# modifier, one panel per level plus a row of p values for the difference in
# that effect between levels, combined with patchwork. Takes the estimates
# from estimate_effects(); it fits nothing.
plot_effect_trajectories <- function(trial_data, estimates, outcome_of_interest,
                                     interaction_variable = NULL,
                                     plot_title = "Plot") {
  # Top of the plot
  y_max <- trial_data %>%
    select(all_of(outcome_of_interest)) %>%
    max(., na.rm = TRUE) + 3

  # "p=0.012", with † for the follow-up contrasts
  p_value_labels <- function(effects, dagger = TRUE) {
    effects %>%
      mutate(time_linear = as.numeric(as.character(time_factor))) %>%
      mutate(p.value = round(p.value, 3)) %>%
      mutate(p_value_label = paste0("p=", p.value)) %>%
      mutate(p_value_label = ifelse(
        time_linear == 0 | !dagger,
        p_value_label,
        paste0(p_value_label, "†")
      ))
  }
  model_means <- estimates$means %>%
    mutate(time_linear = as.numeric(as.character(time_factor)))

  if (is.null(interaction_variable)) {
    p_values <- p_value_labels(estimates$within_levels) %>%
      select(time_linear, p_value_label)

    # Number observed in each arm at each visit, with the model means
    mixed_model_predictions <- trial_data %>%
      filter(!is.na(.data[[outcome_of_interest]])) %>%
      group_by(time_linear, studyarm) %>%
      summarize(n_obs = n()) %>%
      left_join(
        .,
        model_means %>%
          select(time_linear, studyarm, estimate, conf.low, conf.high),
        by = c("time_linear", "studyarm")
      ) %>%
      mutate(time_linear_shifted = ifelse(
        studyarm == "1",
        time_linear + 0.25,
        time_linear - 0.25
      ))

    # Spaghetti plot with model means over individual trajectories
    ggplot() +
      geom_point(
        trial_data,
        mapping = aes(
          x = time_linear_shifted, y = .data[[outcome_of_interest]],
          color = studyarm, group = study_id_screening
        ),
        alpha = 0.2
      ) +
      geom_line(
        trial_data,
        mapping = aes(
          x = time_linear_shifted, y = .data[[outcome_of_interest]],
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
        linewidth = 2
      ) +
      geom_errorbar(
        mixed_model_predictions,
        mapping = aes(
          x = time_linear_shifted, ymin = conf.low, ymax = conf.high,
          color = studyarm
        ),
        linewidth = 1.5
      ) +
      theme_publication() +
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
        p_values,
        mapping = aes(x = time_linear, y = y_max, label = p_value_label)
      ) +
      scale_x_continuous(
        breaks = c(0, 2, 4, 6, 9, 12),
        limits = c(-0.5, 12.5),
        minor_breaks = 0:12
      ) +
      ylab(figure_label(outcome_of_interest)) +
      xlab(figure_label("time_linear")) +
      scale_colour_discrete(
        name = figure_label("studyarm"),
        labels = arm_labels
      ) +
      labs(caption = "† adjusted for baseline differences") +
      theme(plot.caption = element_text(hjust = 0.0555)) +
      ggtitle(plot_title)
  } else {
    plot_labels <- trial_data %>%
      pull(interaction_variable) %>%
      levels()

    # Levels are numbered 1, 2, ... in factor order; rows with a missing
    # level are counted but never drawn
    data_for_function <- trial_data %>%
      mutate(interaction_variable_numeric = factor(
        as.numeric(.data[[interaction_variable]])
      ))
    level_number <- function(level) {
      factor(match(as.character(level), plot_labels))
    }

    within_p_values <- p_value_labels(estimates$within_levels) %>%
      mutate(
        interaction_variable = level_number(.data[[interaction_variable]])
      ) %>%
      select(interaction_variable, time_linear, p_value_label)
    between_p_values <- estimates$between_levels %>%
      p_value_labels(dagger = FALSE) %>%
      select(comparison, time_linear, p_value_label)

    # Number observed in each arm and level at each visit, with the model
    # means
    predictions_interaction <- data_for_function %>%
      filter(!is.na(.data[[outcome_of_interest]])) %>%
      group_by(time_linear, studyarm, interaction_variable_numeric) %>%
      summarize(n_obs = n()) %>%
      rename(interaction_variable = interaction_variable_numeric) %>%
      left_join(
        .,
        model_means %>%
          mutate(
            interaction_variable = level_number(.data[[interaction_variable]])
          ) %>%
          select(time_linear, studyarm, interaction_variable, estimate,
                 conf.low, conf.high),
        by = c("time_linear", "studyarm", "interaction_variable")
      ) %>%
      mutate(time_linear_shifted_2 = ifelse(
        studyarm == "1",
        time_linear + 0.25,
        time_linear - 0.25
      )) %>%
      group_by(studyarm, interaction_variable) %>%
      mutate(gid = cur_group_id())

    # One panel per level, each drawing that level's participants (selected
    # by level, not by position) and that level's estimates
    plot_level <- function(k) {
      level_data <- data_for_function %>%
        filter(interaction_variable_numeric == k)
      level_predictions <- predictions_interaction %>%
        filter(interaction_variable == k)

      ggplot() +
        geom_point(
          level_data,
          mapping = aes(
            x = time_linear_shifted, y = .data[[outcome_of_interest]],
            color = studyarm, group = study_id_screening
          ),
          alpha = 0.2
        ) +
        geom_line(
          level_data,
          mapping = aes(
            x = time_linear_shifted, y = .data[[outcome_of_interest]],
            color = studyarm, group = study_id_screening
          ),
          alpha = 0.2
        ) +
        geom_point(
          level_predictions,
          mapping = aes(
            x = time_linear_shifted_2, y = estimate, color = studyarm
          ),
          size = 2
        ) +
        geom_line(
          level_predictions,
          mapping = aes(
            x = time_linear_shifted_2, y = estimate, color = studyarm,
            group = gid
          ),
          linewidth = 2
        ) +
        theme_publication() +
        geom_text(
          filter(level_predictions, studyarm == 0),
          mapping = aes(x = time_linear, y = 1, label = n_obs, color = studyarm)
        ) +
        geom_text(
          filter(level_predictions, studyarm == 1),
          mapping = aes(
            x = time_linear, y = -1, label = n_obs, color = studyarm
          )
        ) +
        geom_text(mapping = aes(x = -1, y = 1, label = "N")) +
        geom_text(mapping = aes(x = -1, y = -1, label = plot_labels[k])) +
        geom_text(
          filter(within_p_values, interaction_variable == k),
          mapping = aes(x = time_linear, y = y_max, label = p_value_label)
        ) +
        geom_errorbar(
          level_predictions,
          mapping = aes(
            x = time_linear_shifted_2, ymin = conf.low, ymax = conf.high,
            color = studyarm
          ),
          linewidth = 1.5
        ) +
        scale_x_continuous(
          breaks = NULL,
          limits = c(-1, 12.5),
          minor_breaks = 0:12
        ) +
        scale_y_continuous(limits = c(-3, y_max)) +
        theme(axis.title.x = element_blank()) +
        ylab(figure_label(outcome_of_interest)) +
        scale_colour_discrete(
          name = figure_label("studyarm"),
          labels = arm_labels
        )
    }

    # One row of p values for each level's difference from the first level,
    # named when there is more than one row
    plot_p_values <- function(level_comparison) {
      p_value_row <- ggplot() +
        geom_text(
          data = filter(between_p_values, comparison == level_comparison),
          mapping = aes(x = time_linear, y = 0, label = p_value_label)
        ) +
        theme_void() +
        scale_x_continuous(limits = c(-1, 12.5))
      if (length(comparisons) > 1) {
        p_value_row <- p_value_row +
          annotate("text", x = -1, y = 0, label = level_comparison, size = 3)
      }
      p_value_row
    }

    n_levels <- length(plot_labels)
    comparisons <- unique(as.character(between_p_values$comparison))
    panels <- c(
      lapply(seq_len(n_levels), plot_level),
      lapply(comparisons, plot_p_values)
    )

    # Combine the panels with patchwork
    wrap_plots(panels, ncol = 1) +
      plot_layout(
        guides = "collect",
        heights = c(
          rep(0.96 / n_levels, n_levels),
          rep(0.04, length(comparisons))
        )
      ) +
      plot_annotation(
        title = plot_title,
        caption = "† adjusted for baseline differences",
        theme = theme(plot.caption = element_text(hjust = 0.0555))
      )
  }
}
