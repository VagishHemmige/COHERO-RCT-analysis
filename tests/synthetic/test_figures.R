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
      show_effect_panel = TRUE
    )
  )
  # P values without the baseline test, no trajectories
  vdiffr::expect_doppelganger(
    "ACT means and p values without baseline test, ITT",
    compose_trajectory_figure(
      mixed_model_data[["itt"]],
      mixed_model_estimates[["itt"]][["act_score"]][["overall"]],
      "act_score", show_trajectories = FALSE, show_baseline_p = FALSE
    )
  )
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
