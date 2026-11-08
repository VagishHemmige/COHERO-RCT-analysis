# Purpose: reusable helper functions for the repeated-outcome analyses, in
#   three steps: fit a model, estimate effects from it, report them as a
#   table and a figure built from parts. Each takes its inputs as arguments.
# Needs: setup.R (packages, figure_labels, arm_labels); plot_style.R
#   (theme_publication(), publication_colours, figure_label()) when the
#   figure functions run. Produces: functions only.
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

## ---- add-plot-offset
# Shift each arm a little left or right of its visit, so the arms' points and
# bars sit side by side instead of on top of each other. One offset serves
# the individual trajectories and the model means (issue #12).
add_plot_offset <- function(data, offset = 0.18, exposure = "studyarm",
                            time = "time_linear") {
  exposure_levels <- levels(factor(data[[exposure]]))
  shifts <- seq(-offset, offset, length.out = length(exposure_levels))
  data %>%
    mutate(time_plot = .data[[time]] +
      shifts[match(as.character(.data[[exposure]]), exposure_levels)])
}

## ---- jitter-trajectories
# Display-only changes to the individual trajectories (issue #11). Model
# means, confidence intervals, counts and p values always come from the
# unchanged data.
# - jitter_x: each participant's whole trajectory moves left or right by one
#   random amount, up to this many months, so overlapping lines separate
#   without zigzagging between visits.
# - jitter_y: each plotted value moves up or down by a random amount, up to
#   this much in the outcome's units, so exact values cannot be read off.
#   Whether to use it, and how much, is a disclosure decision (#11).
# A fixed seed makes a jittered figure reproducible, without changing R's
# random numbers anywhere else.
jitter_trajectories <- function(trajectories, outcome, jitter_x = 0,
                                jitter_y = 0, seed = 20261108) {
  if (jitter_x == 0 && jitter_y == 0) {
    return(trajectories)
  }
  withr::with_seed(seed, {
    ids <- unique(trajectories$study_id_screening)
    shifts <- stats::setNames(
      stats::runif(length(ids), -jitter_x, jitter_x), ids
    )
    trajectories %>%
      mutate(
        time_plot = time_plot + shifts[as.character(study_id_screening)],
        "{outcome}" := .data[[outcome]] +
          stats::runif(n(), -jitter_y, jitter_y)
      )
  })
}

# The visits on the x-axis, with room either side for the offsets. The
# panels zoom with coord_cartesian() rather than scale limits, so text placed
# beyond the last visit (the arm names) is kept, not dropped.
trajectory_x_scale <- function(visits) {
  scale_x_continuous(breaks = visits, expand = expansion(0))
}

trajectory_coord <- function(visits, y_limits = NULL) {
  coord_cartesian(
    xlim = range(visits) + c(-0.6, 0.6), ylim = y_limits, clip = "off"
  )
}

# "0.012", with † for the follow-up contrasts (change from baseline); the
# row's heading says these are p values
format_p_labels <- function(effects, dagger = TRUE) {
  effects %>%
    mutate(time_linear = as.numeric(as.character(time_factor))) %>%
    mutate(p.value = round(p.value, 3)) %>%
    mutate(p_value_label = as.character(p.value)) %>%
    mutate(p_value_label = ifelse(
      time_linear == 0 | !dagger,
      p_value_label,
      paste0(p_value_label, "†")
    ))
}

# Rows for one level of the modifier, or every row when there is none
rows_for_level <- function(rows, modifier, level) {
  if (is.null(modifier)) {
    rows
  } else {
    rows %>% filter(as.character(.data[[modifier]]) == level)
  }
}

## ---- plot-means
# Model-estimated mean (95% CI) for each arm at each visit, optionally over
# the individual trajectories. Arms differ by
# colour, shape and line type; label_arms names them at the right-hand end of
# their lines instead of in a legend. Returns a ggplot.
plot_means <- function(data, estimates, outcome, modifier = NULL,
                       level = NULL, y_limits, show_trajectories = TRUE,
                       label_arms = TRUE, show_y_title = TRUE,
                       show_x_axis = FALSE, title = NULL, offset = 0.18,
                       jitter_x = 0, jitter_y = 0, seed = 20261108) {
  visits <- sort(unique(data$time_linear))
  means <- rows_for_level(estimates$means, modifier, level) %>%
    mutate(time_linear = as.numeric(as.character(time_factor))) %>%
    add_plot_offset(offset)

  panel <- ggplot(means, aes(
    x = time_plot, y = estimate, colour = studyarm, group = studyarm
  ))

  if (show_trajectories) {
    trajectories <- rows_for_level(data, modifier, level) %>%
      filter(!is.na(.data[[outcome]])) %>%
      add_plot_offset(offset) %>%
      jitter_trajectories(outcome, jitter_x, jitter_y, seed)
    panel <- panel +
      geom_line(
        data = trajectories,
        mapping = aes(y = .data[[outcome]], group = study_id_screening),
        alpha = 0.2, linewidth = 0.3
      ) +
      geom_point(
        data = trajectories,
        mapping = aes(y = .data[[outcome]], group = study_id_screening),
        alpha = 0.2, size = 0.8
      )
  }

  panel <- panel +
    geom_line(aes(linetype = studyarm), linewidth = 0.7) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, linewidth = 0.6) +
    geom_point(aes(shape = studyarm), fill = "white", size = 2, stroke = 0.7)

  if (label_arms) {
    last_visit <- means %>% filter(time_linear == max(time_linear))
    panel <- panel +
      geom_text(
        data = last_visit,
        mapping = aes(
          x = max(visits) + 0.75, label = arm_labels[as.character(studyarm)]
        ),
        hjust = 0, size = 2.9, fontface = "bold"
      )
  }

  panel +
    trajectory_x_scale(visits) +
    trajectory_coord(visits, y_limits) +
    labs(
      x = if (show_x_axis) figure_label("time_linear") else NULL,
      y = if (show_y_title) figure_label(outcome) else NULL,
      title = title
    ) +
    theme_publication() +
    theme(
      legend.position = "none",
      axis.text.x = if (show_x_axis) {
        element_text(colour = "grey30")
      } else {
        element_blank()
      },
      axis.text.y = if (show_y_title) {
        element_text(colour = "grey30")
      } else {
        element_blank()
      }
    )
}

## ---- plot-effects
# The effect at each follow-up visit with its 95% CI and a line at zero:
# intervention minus control in change from baseline ("within"), or how much
# that effect differs between a modifier level and the first level
# ("between"). Returns a ggplot.
plot_effects <- function(estimates, visits, modifier = NULL, level = NULL,
                         comparison = NULL, y_limits, show_y_title = TRUE,
                         title = NULL) {
  if (is.null(comparison)) {
    effects <- rows_for_level(estimates$within_levels, modifier, level)
    colour <- publication_colours[2]
  } else {
    effects <- estimates$between_levels %>%
      filter(as.character(comparison) == .env$comparison)
    colour <- publication_colours[3]
  }
  effects <- effects %>%
    mutate(time_linear = as.numeric(as.character(time_factor))) %>%
    filter(time_linear > 0)

  panel <- ggplot(effects, aes(x = time_linear, y = estimate)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, linewidth = 0.6, colour = colour) +
    geom_point(size = 2, colour = colour)

  panel +
    trajectory_x_scale(visits) +
    trajectory_coord(visits, y_limits) +
    labs(
      x = figure_label("time_linear"),
      y = if (show_y_title) {
        "Difference (95% CI)"
      } else {
        NULL
      },
      title = title
    ) +
    theme_publication() +
    theme(
      axis.text.y = if (show_y_title) {
        element_text(colour = "grey30")
      } else {
        element_blank()
      }
    )
}

## ---- plot-p-values
# A row of p values aligned to the visits, under a heading: the effect
# within a level (marked † at follow-up, where it concerns change from
# baseline) or the difference between levels. Returns a ggplot.
plot_p_values <- function(effects, visits, title = NULL, dagger = TRUE,
                          show_baseline_p = TRUE) {
  p_values <- format_p_labels(effects, dagger = dagger)
  if (!show_baseline_p) {
    p_values <- p_values %>% filter(time_linear != 0)
  }
  ggplot(p_values, aes(x = time_linear, y = 0, label = p_value_label)) +
    geom_text(size = 2.3, colour = "grey20") +
    trajectory_x_scale(visits) +
    trajectory_coord(visits) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(size = 7.5, colour = "grey20"))
}

## ---- plot-numbers-table
# Number of participants with the outcome recorded, per arm and visit: one
# row per arm, named in text, aligned to the visits above. Returns a ggplot.
plot_numbers_table <- function(data, outcome, modifier = NULL, level = NULL,
                               show_row_labels = TRUE, title = "Observed, n") {
  visits <- sort(unique(data$time_linear))
  counts <- rows_for_level(data, modifier, level) %>%
    filter(!is.na(.data[[outcome]])) %>%
    count(time_linear, studyarm, name = "n_obs") %>%
    mutate(arm = factor(
      arm_labels[as.character(studyarm)], levels = rev(arm_labels)
    ))

  ggplot(counts, aes(
    x = time_linear, y = arm, label = n_obs, colour = studyarm
  )) +
    geom_text(size = 2.7) +
    trajectory_x_scale(visits) +
    trajectory_coord(visits) +
    labs(x = NULL, y = NULL, title = title) +
    theme_publication() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = if (show_row_labels) {
        element_text(colour = "grey20", face = "bold")
      } else {
        element_blank()
      },
      plot.title = element_text(face = "plain", size = rel(0.9))
    )
}

## ---- compose-trajectory-figure
# The whole figure: one column per modifier level (a single column without a
# modifier), rows for the means, the effects and the numbers table, built as
# one grid so the visits line up from top to bottom. The four options are
# chosen per figure (#35); the defaults reproduce the earlier figure's
# content. jitter_x and jitter_y change only what is displayed (#11); both
# default to no change. A data set with cells small enough to need
# suppressing should not be shown in this figure at all. Returns a patchwork object: change every
# panel with `&`, or one panel by indexing it.
compose_trajectory_figure <- function(data, estimates, outcome,
                                      modifier = NULL, title = NULL,
                                      show_trajectories = TRUE,
                                      show_p = TRUE,
                                      show_baseline_p = TRUE,
                                      show_effect_panel = FALSE,
                                      offset = 0.18, jitter_x = 0,
                                      jitter_y = 0, seed = 20261108) {
  visits <- sort(unique(data$time_linear))
  if (is.null(modifier)) {
    levels_shown <- list(NULL)
    comparisons <- character()
  } else {
    data <- data %>% filter(!is.na(.data[[modifier]]))
    levels_shown <- as.list(levels(data[[modifier]]))
    comparisons <- unique(as.character(estimates$between_levels$comparison))
  }
  n_levels <- length(levels_shown)

  # One y range for every means panel, widened by any vertical jitter so
  # every displaced value stays on the axis
  means_limits <- range(
    estimates$means$conf.low, estimates$means$conf.high,
    if (show_trajectories) data[[outcome]] + jitter_y,
    if (show_trajectories) data[[outcome]] - jitter_y,
    na.rm = TRUE
  )

  level_title <- function(level) {
    if (is.null(level)) NULL else paste0(figure_label(modifier), ": ", level)
  }
  spacers <- function(n) rep(list(plot_spacer()), n)

  # Extra columns: one per between-level comparison with the effect panel,
  # otherwise one narrow column that leaves room for the arm labels
  extra <- if (show_effect_panel && length(comparisons) > 0) {
    length(comparisons)
  } else {
    1
  }
  widths <- c(
    rep(1, n_levels),
    rep(if (show_effect_panel && length(comparisons) > 0) 1 else 0.3, extra)
  )

  means_row <- c(
    lapply(seq_len(n_levels), function(i) {
      plot_means(
        data, estimates, outcome, modifier, levels_shown[[i]],
        y_limits = means_limits, show_trajectories = show_trajectories,
        label_arms = i == n_levels, show_y_title = i == 1,
        show_x_axis = !show_effect_panel,
        title = level_title(levels_shown[[i]]), offset = offset,
        jitter_x = jitter_x, jitter_y = jitter_y, seed = seed
      )
    }),
    spacers(extra)
  )
  rows <- list(means_row)
  heights <- 3

  if (show_effect_panel) {
    effect_rows <- bind_rows(
      estimates$within_levels, estimates$between_levels
    ) %>%
      filter(as.numeric(as.character(time_factor)) > 0)
    effect_limits <- range(effect_rows$conf.low, effect_rows$conf.high, 0)
    effects_row <- lapply(seq_len(n_levels), function(i) {
      plot_effects(
        estimates, visits, modifier, levels_shown[[i]],
        y_limits = effect_limits, show_y_title = i == 1,
        title = if (i == 1) {
          "Intervention minus control, change from baseline"
        } else {
          NULL
        }
      )
    })
    if (length(comparisons) > 0) {
      effects_row <- c(effects_row, lapply(comparisons, function(comparison) {
        plot_effects(
          estimates, visits, comparison = comparison,
          y_limits = effect_limits, show_y_title = FALSE,
          title = paste("Difference,", comparison)
        )
      }))
    } else {
      effects_row <- c(effects_row, spacers(extra))
    }
    rows <- c(rows, list(effects_row))
    heights <- c(heights, 1.8)
  }

  if (show_p) {
    within_title <- "p, intervention vs control (\u2020 change from baseline)"
    between_title <- function(comparison) {
      paste0("p, difference between levels (", comparison, ")")
    }
    within_row <- lapply(seq_len(n_levels), function(i) {
      plot_p_values(
        rows_for_level(estimates$within_levels, modifier, levels_shown[[i]]),
        visits, title = if (i == 1) within_title else NULL,
        show_baseline_p = show_baseline_p
      )
    })
    between_rows <- lapply(comparisons, function(comparison) {
      plot_p_values(
        estimates$between_levels %>%
          filter(as.character(.data$comparison) == .env$comparison),
        visits,
        title = if (show_effect_panel) {
          paste("p,", comparison)
        } else {
          between_title(comparison)
        },
        dagger = FALSE, show_baseline_p = show_baseline_p
      )
    })
    if (show_effect_panel && length(comparisons) > 0) {
      # Between-level p values sit under their own effect columns
      rows <- c(rows, list(c(within_row, between_rows)))
      heights <- c(heights, 0.4)
    } else {
      rows <- c(rows, list(c(within_row, spacers(extra))))
      heights <- c(heights, 0.4)
      for (between_row in between_rows) {
        rows <- c(
          rows, list(c(list(between_row), spacers(n_levels - 1 + extra)))
        )
        heights <- c(heights, 0.4)
      }
    }
  }

  numbers_row <- c(
    lapply(seq_len(n_levels), function(i) {
      plot_numbers_table(
        data, outcome, modifier, levels_shown[[i]],
        show_row_labels = i == 1, title = if (i == 1) "Observed, n" else NULL
      )
    }),
    spacers(extra)
  )
  rows <- c(rows, list(numbers_row))
  heights <- c(heights, 0.5)

  caption <- c(
    if (show_trajectories) "Faint lines: individual participants.",
    if (show_trajectories && jitter_x > 0) {
      "Trajectories are shifted sideways for readability."
    },
    if (show_trajectories && jitter_y > 0) {
      "Individual values are randomly displaced; means use the exact values."
    },
    "Points and bars: model-estimated means (95% CI).",
    if (show_effect_panel) {
      "Effects: intervention minus control in change from baseline (95% CI)."
    },
    if (show_p) "† adjusted for baseline differences"
  )

  wrap_plots(unlist(rows, recursive = FALSE),
             ncol = length(widths), byrow = TRUE,
             widths = widths, heights = heights) +
    plot_annotation(
      title = title,
      caption = paste(strwrap(paste(caption, collapse = " "), 110),
                      collapse = "\n"),
      theme = theme_publication()
    )
}
