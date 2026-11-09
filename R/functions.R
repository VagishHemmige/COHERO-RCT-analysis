# Purpose: reusable helper functions for the repeated-outcome analyses, in
#   three steps: fit a model, estimate effects from it, report them as a
#   table and a figure built from parts. Each takes its inputs as arguments
#   and reads no global objects, so the file can be copied into another
#   project together with plot_style.R.
# Needs: the tidyverse attached (dplyr, ggplot2, stringr, tibble); installed:
#   lme4, lmerTest, emmeans, gtsummary, patchwork, withr, flextable;
#   plot_style.R (theme_publication(), figure_label()) for the figure
#   functions. Produces: functions only.
# Data the functions expect:
#   - one row per participant and visit, with an id column;
#   - an exposure with exactly two levels, the first the reference (for
#     example control, then intervention);
#   - time as a factor whose levels are numbers (for example "0", "2", "4"),
#     the first level being baseline;
#   - optionally a modifier (a subgroup factor) with any number of levels.
#   Column names, arm names, readable labels and colors are arguments.
# Ported from the outcome_functions chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd. The original fitted each model up to
#   four times in different codings; here it is fitted once and the
#   estimates come from emmeans (issue #10).

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

  # lmerTest's lmer(), named explicitly: it adds the Satterthwaite tests
  # that the model-terms table reports, whatever else is attached
  lmerTest::lmer(model_formula, data = data)
}

## ---- tabulate-model-terms
# The fitted model's fixed-effect terms as a gtsummary table: the baseline
# exposure difference, the time-by-exposure interaction terms and, with a
# modifier, the three-way terms (issue #8). Estimates, 95% confidence
# intervals and p values are the model's own, from broom.mixed::tidy(), with
# Satterthwaite degrees of freedom. labels: readable names for the model's
# variables, looked up by name. tbl_regression() shows fixed effects only, so
# the random intercept's SD and the residual SD go in a note under the table,
# where the model can be checked without them reading as effects.
tabulate_model_terms <- function(fit, caption, labels = NULL) {
  model_variables <- all.vars(formula(fit, fixed.only = TRUE))[-1]
  variable_labels <- as.list(labels[intersect(names(labels), model_variables)])
  standard_deviations <- as.data.frame(lme4::VarCorr(fit))
  participant_sd <-
    standard_deviations$sdcor[standard_deviations$grp != "Residual"][1]
  residual_sd <- standard_deviations$sdcor[standard_deviations$grp == "Residual"]
  # Intraclass correlation: the share of the variance that lies between
  # participants rather than between one participant's visits
  intraclass_correlation <- participant_sd^2 / (participant_sd^2 + residual_sd^2)
  sd_note <- sprintf(
    paste(
      "Participant SD %.2f; residual SD %.2f; intraclass correlation %.2f",
      "(random intercept for each participant)."
    ),
    participant_sd, residual_sd, intraclass_correlation
  )

  gtsummary::tbl_regression(fit, label = variable_labels) %>%
    gtsummary::modify_fmt_fun(p.value ~ format_p_value) %>%
    gtsummary::modify_source_note(sd_note) %>%
    gtsummary::modify_caption(caption)
}

## ---- tabulate-effects
# The exposure effect at each time from estimate_effects(), as a gtsummary
# table (issue #8): at the first (baseline) time the difference between
# exposure groups, and at each later time the difference between them in
# change from baseline. These are the effects the trajectory figures show.
# With a modifier, one column group per level, then one per difference
# between a level and the first level. time_label names the time unit in the
# row labels. p values are formatted as on the figures.
tabulate_effects <- function(estimates, caption, time = "time_factor",
                             modifier = NULL, time_label = "Month") {
  effect_table <- function(effects) {
    effects %>%
      transmute(
        variable = time,
        row_type = "level",
        label = paste(time_label, .data[[time]]),
        estimate, conf.low, conf.high, p.value
      ) %>%
      gtsummary::as_gtsummary() %>%
      gtsummary::modify_column_hide(c(variable, row_type)) %>%
      gtsummary::modify_header(
        label = "**Visit**", estimate = "**Difference**",
        conf.low = "**95% CI**", p.value = "**p-value**"
      ) %>%
      gtsummary::modify_fmt_fun(
        c(estimate, conf.low, conf.high) ~
          gtsummary::label_style_number(digits = 2),
        p.value ~ format_p_value
      ) %>%
      gtsummary::modify_column_merge(
        pattern = "{conf.low}, {conf.high}", rows = !is.na(conf.low)
      )
  }
  footnote <- paste(
    "First visit: the difference between groups.",
    "Later visits: the difference between groups in change from baseline.",
    "Not adjusted for multiple comparisons."
  )

  if (is.null(modifier)) {
    effects_table <- effect_table(estimates$within_levels)
  } else {
    within <- estimates$within_levels
    levels_shown <- if (is.factor(within[[modifier]])) {
      levels(droplevels(within[[modifier]]))
    } else {
      unique(as.character(within[[modifier]]))
    }
    comparisons <- unique(as.character(estimates$between_levels$comparison))

    within_tables <- lapply(levels_shown, function(level) {
      effect_table(within %>% filter(as.character(.data[[modifier]]) == level))
    })
    between_tables <- lapply(comparisons, function(level_comparison) {
      effect_table(
        estimates$between_levels %>%
          filter(as.character(comparison) == level_comparison)
      )
    })
    effects_table <- gtsummary::tbl_merge(
      c(within_tables, between_tables),
      tab_spanner = c(
        paste0("**", levels_shown, "**"),
        paste0("**Difference between levels: ", comparisons, "**")
      )
    )
  }

  effects_table %>%
    gtsummary::modify_footnote_header(
      footnote, columns = starts_with("estimate"), replace = TRUE
    ) %>%
    gtsummary::modify_caption(caption)
}

## ---- check-two-levels
# The effects, tables and figures compare two exposure groups. A third group
# would give two comparisons per visit that the outputs cannot tell apart, so
# stop with a message instead.
check_two_levels <- function(exposure_levels, exposure) {
  if (length(exposure_levels) != 2) {
    stop(
      "The exposure '", exposure, "' has ", length(exposure_levels),
      " levels; these functions compare exactly two.", call. = FALSE
    )
  }
  invisible(exposure_levels)
}

# Time levels must be numbers, in order, with baseline first: the first
# level is taken as baseline, and the figures place visits by number
check_time_levels <- function(time_levels, time) {
  visit_numbers <- suppressWarnings(as.numeric(time_levels))
  if (anyNA(visit_numbers) || is.unsorted(visit_numbers, strictly = TRUE)) {
    stop(
      "The levels of '", time, "' must be numbers in increasing order, ",
      "baseline first; they are: ", paste(time_levels, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(time_levels)
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
    emmeans::emmeans(fit, specs = specs, by = by,
            lmer.df = "satterthwaite", lmerTest.limit = Inf)
  }
  tidy_emm <- function(x) {
    as_tibble(as.data.frame(summary(x, infer = TRUE, adjust = "none")))
  }
  first <- "trt.vs.ctrl1"
  grid_levels <- emmeans::ref_grid(fit)@levels
  check_two_levels(grid_levels[[exposure]], exposure)
  check_time_levels(grid_levels[[time]], time)
  baseline <- grid_levels[[time]][1]
  # The later time in a "t - baseline" interaction contrast
  contrast_time <- function(x) str_remove(x, " - .*$")

  means <- emm(exposure, by = c(time, modifier)) %>%
    tidy_emm() %>%
    rename(estimate = emmean, conf.low = lower.CL, conf.high = upper.CL)

  at_baseline <- emm(exposure, by = c(time, modifier)) %>%
    emmeans::contrast(first) %>%
    tidy_emm() %>%
    filter(.data[[time]] == baseline)
  over_time <- emm(c(exposure, time), by = modifier) %>%
    emmeans::contrast(interaction = c(first, first)) %>%
    tidy_emm() %>%
    mutate(!!time := contrast_time(.data[[paste0(time, "_", first)]]))
  within_levels <- bind_rows(at_baseline, over_time) %>%
    select(any_of(c(modifier, time)), estimate, conf.low = lower.CL,
           conf.high = upper.CL, p.value)

  between_levels <- NULL
  if (!is.null(modifier)) {
    between_at_baseline <- emm(c(exposure, modifier), by = time) %>%
      emmeans::contrast(interaction = c(first, first)) %>%
      tidy_emm() %>%
      filter(.data[[time]] == baseline)
    between_over_time <- emm(c(exposure, time, modifier)) %>%
      emmeans::contrast(interaction = c(first, first, first)) %>%
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
                                jitter_y = 0, seed = 20261108,
                                id = "study_id_screening") {
  if (jitter_x == 0 && jitter_y == 0) {
    return(trajectories)
  }
  withr::with_seed(seed, {
    ids <- unique(trajectories[[id]])
    shifts <- stats::setNames(
      stats::runif(length(ids), -jitter_x, jitter_x), ids
    )
    trajectories %>%
      mutate(
        time_plot = time_plot + shifts[as.character(.data[[id]])],
        "{outcome}" := .data[[outcome]] +
          stats::runif(n(), -jitter_y, jitter_y)
      )
  })
}

# Visit numbers from the time factor ("0", "2", ...), as a column the figure
# parts use for the x-axis
add_visit_number <- function(data, time) {
  data %>% mutate(time_linear = as.numeric(as.character(.data[[time]])))
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

# A p value for display (issue #33): "<0.001" below 0.001, otherwise three
# decimals ("0.040", "1.000"), and blank when there is none. Only the label
# is rounded; the estimates keep full precision.
format_p_value <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  )
}

# The p values of a set of contrasts as labels, with † for the follow-up
# contrasts (change from baseline); the row's heading says they are p values
format_p_labels <- function(effects, dagger = TRUE, time = "time_factor") {
  effects <- add_visit_number(effects, time)
  baseline <- min(effects$time_linear)
  effects %>%
    mutate(p_value_label = format_p_value(p.value)) %>%
    mutate(p_value_label = ifelse(
      time_linear == baseline | !dagger | p_value_label == "",
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

# The two arms' display names, in level order. Without arm_labels the levels
# themselves are used; with them, every level must have a name.
resolve_arm_labels <- function(exposure_levels, arm_labels = NULL,
                               exposure = "exposure") {
  check_two_levels(exposure_levels, exposure)
  if (is.null(arm_labels)) {
    return(stats::setNames(exposure_levels, exposure_levels))
  }
  missing_names <- setdiff(exposure_levels, names(arm_labels))
  if (length(missing_names) > 0) {
    stop("arm_labels has no name for level(s): ",
         paste(missing_names, collapse = ", "), call. = FALSE)
  }
  arm_labels[exposure_levels]
}

## ---- plot-means
# Model-estimated mean (95% CI) for each arm at each visit, optionally over
# the individual trajectories. Arms differ by color, shape and line type;
# label_arms names them at the right-hand end of their lines instead of in a
# legend. arm_labels: the arms' display names, named by level; labels:
# readable variable names, looked up by name. Returns a ggplot.
plot_means <- function(data, estimates, outcome, modifier = NULL,
                       level = NULL, y_limits, show_trajectories = TRUE,
                       label_arms = TRUE, show_y_title = TRUE,
                       show_x_axis = FALSE, title = NULL, offset = 0.18,
                       jitter_x = 0, jitter_y = 0, seed = 20261108,
                       exposure = "studyarm", time = "time_factor",
                       id = "study_id_screening", arm_labels = NULL,
                       labels = NULL) {
  data <- add_visit_number(data, time)
  visits <- sort(unique(data$time_linear))
  means <- rows_for_level(estimates$means, modifier, level) %>%
    add_visit_number(time) %>%
    add_plot_offset(offset, exposure = exposure)
  arm_labels <- resolve_arm_labels(
    levels(factor(means[[exposure]])), arm_labels, exposure
  )

  panel <- ggplot(means, aes(
    x = time_plot, y = estimate,
    color = .data[[exposure]], group = .data[[exposure]]
  ))

  if (show_trajectories) {
    trajectories <- rows_for_level(data, modifier, level) %>%
      filter(!is.na(.data[[outcome]])) %>%
      add_plot_offset(offset, exposure = exposure) %>%
      jitter_trajectories(outcome, jitter_x, jitter_y, seed, id = id)
    panel <- panel +
      geom_line(
        data = trajectories,
        mapping = aes(y = .data[[outcome]], group = .data[[id]]),
        alpha = 0.2, linewidth = 0.3
      ) +
      geom_point(
        data = trajectories,
        mapping = aes(y = .data[[outcome]], group = .data[[id]]),
        alpha = 0.2, size = 0.8
      )
  }

  panel <- panel +
    geom_line(aes(linetype = .data[[exposure]]), linewidth = 0.7) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, linewidth = 0.6) +
    geom_point(aes(shape = .data[[exposure]]), fill = "white", size = 2,
               stroke = 0.7)

  if (label_arms) {
    last_visit <- means %>% filter(time_linear == max(time_linear))
    panel <- panel +
      geom_text(
        data = last_visit,
        mapping = aes(
          x = max(visits) + 0.75,
          label = arm_labels[as.character(.data[[exposure]])]
        ),
        hjust = 0, size = 2.9, fontface = "bold"
      )
  }

  panel +
    trajectory_x_scale(visits) +
    trajectory_coord(visits, y_limits) +
    labs(
      x = if (show_x_axis) figure_label(time, labels) else NULL,
      y = if (show_y_title) figure_label(outcome, labels) else NULL,
      title = title
    ) +
    theme_publication() +
    theme(
      legend.position = "none",
      axis.text.x = if (show_x_axis) {
        element_text(color = "gray30")
      } else {
        element_blank()
      },
      axis.text.y = if (show_y_title) {
        element_text(color = "gray30")
      } else {
        element_blank()
      }
    )
}

## ---- plot-effects
# The effect at each follow-up visit with its 95% CI and a line at zero: the
# second arm minus the first in change from baseline ("within"), or how much
# that effect differs between a modifier level and the first level
# ("between"). colors: the within and between colors. Returns a ggplot.
plot_effects <- function(estimates, visits, modifier = NULL, level = NULL,
                         comparison = NULL, y_limits, show_y_title = TRUE,
                         title = NULL, time = "time_factor", labels = NULL,
                         colors = c(within = "#0072B2", between = "#D55E00")) {
  if (is.null(comparison)) {
    effects <- rows_for_level(estimates$within_levels, modifier, level)
    color <- colors[["within"]]
  } else {
    effects <- estimates$between_levels %>%
      filter(as.character(comparison) == .env$comparison)
    color <- colors[["between"]]
  }
  effects <- effects %>%
    add_visit_number(time) %>%
    filter(time_linear > min(visits))

  panel <- ggplot(effects, aes(x = time_linear, y = estimate)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, linewidth = 0.6, color = color) +
    geom_point(size = 2, color = color)

  panel +
    trajectory_x_scale(visits) +
    trajectory_coord(visits, y_limits) +
    labs(
      x = figure_label(time, labels),
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
        element_text(color = "gray30")
      } else {
        element_blank()
      }
    )
}

## ---- plot-p-values
# A row of p values aligned to the visits, under a heading: the effect
# within a level (marked † at follow-up, where it concerns change from
# baseline) or the difference between levels. With stagger = TRUE, every
# second visit's label sits on a second line, so labels on narrow panels do
# not run into each other. Returns a ggplot.
plot_p_values <- function(effects, visits, title = NULL, dagger = TRUE,
                          show_baseline_p = TRUE, stagger = FALSE,
                          time = "time_factor") {
  p_values <- format_p_labels(effects, dagger = dagger, time = time) %>%
    mutate(line = if (stagger) {
      -((match(time_linear, visits) - 1) %% 2)
    } else {
      0
    })
  if (!show_baseline_p) {
    p_values <- p_values %>% filter(time_linear != min(visits))
  }
  ggplot(p_values, aes(x = time_linear, y = line, label = p_value_label)) +
    geom_text(size = 2.3, color = "gray20") +
    trajectory_x_scale(visits) +
    trajectory_coord(
      visits, y_limits = if (stagger) c(-1.4, 0.4) else c(-0.4, 0.4)
    ) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(size = 7.5, color = "gray20"))
}

## ---- plot-numbers-table
# Number of participants with the outcome recorded, per arm and visit: one
# row per arm, named in text, aligned to the visits above. Returns a ggplot.
plot_numbers_table <- function(data, outcome, modifier = NULL, level = NULL,
                               show_row_labels = TRUE, title = "Observed, n",
                               exposure = "studyarm", time = "time_factor",
                               arm_labels = NULL) {
  data <- add_visit_number(data, time)
  visits <- sort(unique(data$time_linear))
  arm_labels <- resolve_arm_labels(
    levels(factor(data[[exposure]])), arm_labels, exposure
  )
  counts <- rows_for_level(data, modifier, level) %>%
    filter(!is.na(.data[[outcome]])) %>%
    count(across(all_of(c("time_linear", exposure))), name = "n_obs") %>%
    mutate(arm = factor(
      arm_labels[as.character(.data[[exposure]])], levels = rev(arm_labels)
    ))

  ggplot(counts, aes(
    x = time_linear, y = arm, label = n_obs, color = .data[[exposure]]
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
        element_text(color = "gray20", face = "bold")
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
# suppressing should not be shown in this figure at all.
# exposure, time and id name the columns; arm_labels gives the two arms'
# display names, named by level (the wording "Intervention minus control" is
# built from them); labels gives readable variable names; effect_colors the
# within and between effect colors. Returns a patchwork object: change every
# panel with `&`, or one panel by indexing it.
compose_trajectory_figure <- function(data, estimates, outcome,
                                      modifier = NULL, title = NULL,
                                      show_trajectories = TRUE,
                                      show_p = TRUE,
                                      show_baseline_p = TRUE,
                                      show_effect_panel = FALSE,
                                      offset = 0.18, jitter_x = 0,
                                      jitter_y = 0, seed = 20261108,
                                      exposure = "studyarm",
                                      time = "time_factor",
                                      id = "study_id_screening",
                                      arm_labels = NULL, labels = NULL,
                                      effect_colors = c(
                                        within = "#0072B2",
                                        between = "#D55E00"
                                      )) {
  data <- add_visit_number(data, time)
  visits <- sort(unique(data$time_linear))
  arm_labels <- resolve_arm_labels(
    levels(factor(estimates$means[[exposure]])), arm_labels, exposure
  )
  # Wording for titles and the caption, built from the arm names
  reference_arm <- tolower(arm_labels[[1]])
  compared_arm <- tolower(arm_labels[[2]])

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
    if (is.null(level)) {
      NULL
    } else {
      paste0(figure_label(modifier, labels), ": ", level)
    }
  }
  spacers <- function(n) rep(list(patchwork::plot_spacer()), n)

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
        jitter_x = jitter_x, jitter_y = jitter_y, seed = seed,
        exposure = exposure, time = time, id = id, arm_labels = arm_labels,
        labels = labels
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
      add_visit_number(time) %>%
      filter(time_linear > min(visits))
    effect_limits <- range(effect_rows$conf.low, effect_rows$conf.high, 0)
    effects_row <- lapply(seq_len(n_levels), function(i) {
      plot_effects(
        estimates, visits, modifier, levels_shown[[i]],
        y_limits = effect_limits, show_y_title = i == 1,
        title = if (i == 1) {
          paste0(arm_labels[[2]], " minus ", reference_arm,
                 ", change from baseline")
        } else {
          NULL
        },
        time = time, labels = labels, colors = effect_colors
      )
    })
    if (length(comparisons) > 0) {
      effects_row <- c(effects_row, lapply(comparisons, function(comparison) {
        plot_effects(
          estimates, visits, comparison = comparison,
          y_limits = effect_limits, show_y_title = FALSE,
          title = paste("Difference,", comparison),
          time = time, labels = labels, colors = effect_colors
        )
      }))
    } else {
      effects_row <- c(effects_row, spacers(extra))
    }
    rows <- c(rows, list(effects_row))
    heights <- c(heights, 1.8)
  }

  if (show_p) {
    # Three or more levels leave too little width per visit for the labels
    # on one line (#33)
    stagger <- n_levels >= 3
    p_height <- if (stagger) 0.6 else 0.4
    within_title <- paste0(
      "p, ", compared_arm, " vs ", reference_arm,
      " († change from baseline)"
    )
    within_row <- lapply(seq_len(n_levels), function(i) {
      plot_p_values(
        rows_for_level(estimates$within_levels, modifier, levels_shown[[i]]),
        visits, title = if (i == 1) within_title else NULL,
        show_baseline_p = show_baseline_p, stagger = stagger, time = time
      )
    })
    between_rows <- lapply(comparisons, function(comparison) {
      plot_p_values(
        estimates$between_levels %>%
          filter(as.character(.data$comparison) == .env$comparison),
        visits,
        title = if (show_effect_panel) paste("p,", comparison) else NULL,
        dagger = FALSE, show_baseline_p = show_baseline_p, stagger = stagger,
        time = time
      )
    })
    if (show_effect_panel && length(comparisons) > 0) {
      # Between-level p values sit under their own effect columns
      rows <- c(rows, list(c(within_row, between_rows)))
      heights <- c(heights, p_height)
    } else {
      rows <- c(rows, list(c(within_row, spacers(extra))))
      heights <- c(heights, p_height)
      if (length(comparisons) > 0) {
        stopifnot(length(comparisons) == n_levels - 1)
        # Each level's difference from the first sits under that level; the
        # heading takes the first level's column, which has no difference
        between_heading <- ggplot() +
          labs(title = paste0(
            "p, difference between levels (each minus ",
            levels_shown[[1]], ")"
          )) +
          theme_void() +
          theme(plot.title = element_text(size = 7.5, color = "gray20"))
        rows <- c(
          rows, list(c(list(between_heading), between_rows, spacers(extra)))
        )
        heights <- c(heights, p_height)
      }
    }
  }

  numbers_row <- c(
    lapply(seq_len(n_levels), function(i) {
      plot_numbers_table(
        data, outcome, modifier, levels_shown[[i]],
        show_row_labels = i == 1, title = if (i == 1) "Observed, n" else NULL,
        exposure = exposure, time = time, arm_labels = arm_labels
      )
    }),
    spacers(extra)
  )
  rows <- c(rows, list(numbers_row))
  heights <- c(heights, 0.5)

  # What each element shows (issue #34)
  caption <- c(
    if (show_trajectories) "Faint lines: each participant's recorded values.",
    if (show_trajectories && jitter_x > 0) {
      "Trajectories are shifted sideways for readability."
    },
    if (show_trajectories && jitter_y > 0) {
      "Individual values are randomly displaced; means use the exact values."
    },
    paste(
      "Points and bars: each arm's model-estimated mean and its 95%",
      "confidence interval (the uncertainty in the mean, not the spread of",
      "individual values)."
    ),
    if (show_effect_panel) {
      paste0(
        "Effects: ", compared_arm, " minus ", reference_arm,
        " in change from baseline (95% CI)."
      )
    },
    if (show_p) {
      paste(
        "p values: at baseline, the difference between arms; at follow-up",
        "(†), the difference between arms in change from baseline."
      )
    },
    if (show_p && length(comparisons) > 0) {
      paste(
        "Between levels: each level's effect minus the first level's",
        "(the reference), not a test across all levels."
      )
    },
    if (show_p) "Not adjusted for multiple comparisons."
  )

  patchwork::wrap_plots(unlist(rows, recursive = FALSE),
                        ncol = length(widths), byrow = TRUE,
                        widths = widths, heights = heights) +
    patchwork::plot_annotation(
      title = title,
      caption = paste(strwrap(paste(caption, collapse = " "), 110),
                      collapse = "\n"),
      theme = theme_publication()
    )
}

## ---- save-table
# Save a table as a Word file: a gtsummary table is converted with
# as_flex_table(), a flextable is saved as it is. folder is created if
# needed. Returns the path written.
save_table <- function(table, name, folder = "tables") {
  if (inherits(table, "gtsummary")) {
    table <- gtsummary::as_flex_table(table)
  }
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(folder, paste0(name, ".docx"))
  flextable::save_as_docx(table, path = path)
  invisible(path)
}
