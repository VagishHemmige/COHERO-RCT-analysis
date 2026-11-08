# Purpose: visual snapshot tests of representative figures, and a check that
#   figures save in every export format. Synthetic data only.
# Run by run_pipeline.R after main.R, so the figure objects exist. The first
#   run writes the reference images to _snaps/test_figures/; later runs fail
#   when a figure changes, and the new image is saved beside the old one for
#   review (accept it with testthat::snapshot_accept()).

testthat::local_edition(3)

## ---- figure-snapshots
testthat::test_that("representative figures look as recorded", {
  vdiffr::expect_doppelganger(
    "ACT at each visit, ITT", act_plots[["act_by_visit_itt"]]
  )
  vdiffr::expect_doppelganger(
    "Emergency department visits",
    count_outcome_plots[["ed_visit_number_times_12months"]]
  )
  vdiffr::expect_doppelganger(
    "Rescue inhaler puffs, log scale", medication_plots[["rescue_puffs_log"]]
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
