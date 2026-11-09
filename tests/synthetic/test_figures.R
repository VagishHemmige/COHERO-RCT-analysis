# Purpose: visual snapshot tests of representative figures, and a check that
#   figures save in every export format. Synthetic data only.
# Run by run_pipeline.R after main.R, so the figure objects exist. The first
#   run writes the reference images to _snaps/test_figures/; later runs fail
#   when a figure changes, and the new image is saved beside the old one for
#   review (accept it with testthat::snapshot_accept()).

testthat::local_edition(3)

## ---- figure-snapshots
# The beeswarm plots of ACT and rescue puffs are left out: their point
# placement depends on a density estimate that changed between R versions, so
# their images differed between R 4.3 and the R 4.6 that CI runs, with no
# change to the code. The count beeswarm below matched on both.
testthat::test_that("representative figures look as recorded", {
  vdiffr::expect_doppelganger(
    "Emergency department visits",
    count_outcome_plots[["ed_visit_number_times_12months"]]
  )
  vdiffr::expect_doppelganger(
    "ACT trajectories, ITT",
    mixed_model_plots[["itt"]][["act_score"]][["overall"]]
  )
  vdiffr::expect_doppelganger(
    "ACT trajectories by sex, ITT",
    mixed_model_plots[["itt"]][["act_score"]][["participant_sex"]]
  )
})

testthat::test_that("the trajectory figure's options change it as recorded", {
  # Effect panel on; p values, baseline test and trajectories off
  vdiffr::expect_doppelganger(
    "ACT effects by sex, ITT",
    compose_trajectory_figure(
      mixed_model_data[["itt"]],
      mixed_model_estimates[["itt"]][["act_score"]][["participant_sex"]],
      "act_score", modifier = "participant_sex",
      show_trajectories = FALSE, show_p = FALSE, show_baseline_p = FALSE,
      show_effect_panel = TRUE, arm_labels = arm_labels, labels = figure_labels
    )
  )
  # P values without the baseline test, no trajectories
  vdiffr::expect_doppelganger(
    "ACT means and p values without baseline test, ITT",
    compose_trajectory_figure(
      mixed_model_data[["itt"]],
      mixed_model_estimates[["itt"]][["act_score"]][["overall"]],
      "act_score", show_trajectories = FALSE, show_baseline_p = FALSE,
      arm_labels = arm_labels, labels = figure_labels
    )
  )
})

## ---- display-options
# Jitter changes only what is drawn (#11)
testthat::test_that("jitter changes only the display", {
  data <- mixed_model_data[["itt"]]
  estimates <- mixed_model_estimates[["itt"]][["act_score"]][["overall"]]
  draw <- function(...) {
    figure <- compose_trajectory_figure(
      data, estimates, "act_score", ..., arm_labels = arm_labels, labels = figure_labels
    )
    lapply(ggplot2::ggplot_build(figure[[1]])$data, as.data.frame)
  }
  plain <- draw()
  jittered <- draw(jitter_x = 0.1, jitter_y = 1)

  # Layer 1 is the individual trajectories; layers 3-5 are the model means
  testthat::expect_false(isTRUE(all.equal(plain[[1]]$y, jittered[[1]]$y)))
  testthat::expect_false(isTRUE(all.equal(plain[[1]]$x, jittered[[1]]$x)))
  testthat::expect_equal(plain[3:5], jittered[3:5])
  # The same seed gives the same figure; another seed a different one
  testthat::expect_equal(jittered, draw(jitter_x = 0.1, jitter_y = 1))
  testthat::expect_false(isTRUE(all.equal(
    jittered[[1]]$y, draw(jitter_x = 0.1, jitter_y = 1, seed = 1)[[1]]$y
  )))
  # Each participant's trajectory moves sideways as a whole
  sideways <- draw(jitter_x = 0.1)[[1]]
  shift <- sideways$x - plain[[1]]$x
  testthat::expect_true(all(tapply(shift, sideways$group, function(s) {
    diff(range(s)) < 1e-9
  })))

  # The y-axis widens so every displaced value stays on it
  limits <- ggplot2::ggplot_build(
    compose_trajectory_figure(
      data, estimates, "act_score", jitter_y = 1, arm_labels = arm_labels, labels = figure_labels
    )[[1]]
  )$layout$panel_params[[1]]$y.range
  testthat::expect_true(all(jittered[[1]]$y >= limits[1] &
                              jittered[[1]]$y <= limits[2]))
})

## ---- p-value-labels
# Display cases from issue #33: below, at and above the threshold, near one,
# and missing
testthat::test_that("p values are formatted for display", {
  testthat::expect_equal(
    format_p_value(c(0.00001, 0.0009999, 0.001, 0.0404, 0.04, 0.9996, 1, NA)),
    c("<0.001", "<0.001", "0.001", "0.040", "0.040", "1.000", "1.000", "")
  )
  labels <- format_p_labels(tibble::tibble(
    time_factor = factor(c("0", "2", "4")), p.value = c(0.5, 0.00001, NA)
  ))$p_value_label
  testthat::expect_equal(labels, c("0.500", "<0.001\u2020", ""))
})

## ---- figure-export
testthat::test_that("figures save as PDF, SVG and PNG", {
  file <- file.path(tempdir(), "export-check")
  paths <- save_figure(
    mixed_model_plots[["itt"]][["act_score"]][["overall"]],
    file, width = 120, height = 90
  )
  testthat::expect_equal(
    basename(paths), paste0("export-check.", c("pdf", "svg", "png"))
  )
  testthat::expect_true(all(file.exists(paths)))
  testthat::expect_true(all(file.size(paths) > 0))
})

# Symbols such as the dagger print as ".." outside a UTF-8 locale
testthat::test_that("R runs in a UTF-8 locale", {
  testthat::expect_true(l10n_info()[["UTF-8"]])
})
