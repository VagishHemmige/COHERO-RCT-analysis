# Purpose: run the whole analysis on synthetic data and check that it
#   finishes, produces every expected object, gives every model enough data
#   and recovers the effects built into the data. Used by
#   .github/workflows/test-pipeline.yml; can also be run locally.
# Run from the project root: Rscript tests/synthetic/run_pipeline.R
#   The synthetic data and the snapshot code are read from this script's own
#   folder, so the same harness can run another copy of R/ (the workflow runs
#   it on main's code too, from main's project root).
# Set COHERO_SNAPSHOT to a file path to save every output there, for
#   tests/synthetic/compare_snapshots.R.
# Produces: a pass or fail, counts of warnings and messages, and nothing
#   written outside a temporary folder (and the snapshot, if asked for).
#   Prints no data values, only check results and estimates on synthetic data.

## ---- harness-folder
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
harness_dir <- if (length(script_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", script_arg)))
} else {
  "tests/synthetic"
}

## ---- synthetic-data-folder
synthetic_dir <- file.path(tempdir(), "cohero-synthetic-data")
Sys.setenv(COHERO_DATA_DIR = synthetic_dir)

## ---- build-synthetic-data
source("R/setup.R")
source(file.path(harness_dir, "build_synthetic_data.R"))
cat("Synthetic files written:", length(list.files(synthetic_dir)), "\n")

## ---- run-pipeline
# Run main.R, collecting warnings and messages rather than printing them as
# they happen. Model-fitting problems (singular fits, dropped columns) are
# reported by lme4 as messages.
collected_warnings <- character()
collected_messages <- character()
withCallingHandlers(
  source("R/main.R"),
  warning = function(w) {
    collected_warnings <<- c(collected_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  },
  message = function(m) {
    collected_messages <<- c(collected_messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
cat("main.R finished.\n")

## ---- check-cells
# Every visit x arm x level cell each mixed model uses has at least two
# observations of the outcome, in both populations
mixed_analyses <- c(
  lapply(repeated_outcomes, function(o) list(o, NULL)),
  unlist(lapply(repeated_outcomes, function(o) {
    lapply(mixed_subgroup_variables, function(s) list(o, s))
  }), recursive = FALSE),
  lapply(child_adult_mixed_outcomes, function(o) list(o, "child_adult"))
)
thin_cells <- character()
for (population in names(mixed_model_data)) {
  for (analysis in mixed_analyses) {
    outcome <- analysis[[1]]
    subgroup <- analysis[[2]]
    cells <- mixed_model_data[[population]] %>%
      filter(!is.na(.data[[outcome]])) %>%
      count(time_factor, studyarm, across(all_of(as.character(subgroup))))
    expected_cells <- length(unique(mixed_model_data[[population]]$time_factor)) *
      2 * if (is.null(subgroup)) 1 else 2
    if (nrow(cells) < expected_cells || any(cells$n < 2)) {
      thin_cells <- c(thin_cells, paste(
        population, outcome, if (is.null(subgroup)) "overall" else subgroup
      ))
    }
  }
}

## ---- check-panel-levels
# Each subgroup panel draws its own level's participants, whatever the row
# order (issue #30): rows reversed, so a level-2 participant comes first
# (FALSE, not an error, when the code under test predates these functions,
# so a comparison run on older code still saves its snapshot)
panels_match_levels <- tryCatch({
  reversed_data <- clean_asthma_data_itt %>% arrange(desc(study_id_screening))
  reversed_fit <- fit_mixed_model(
    reversed_data, "act_score", modifier = "participant_sex"
  )
  reversed_plot <- compose_trajectory_figure(
    reversed_data, estimate_effects(reversed_fit, modifier = "participant_sex"),
    "act_score", modifier = "participant_sex"
  )
  baseline_levels <- reversed_data %>%
    filter(time_factor == "0") %>%
    select(study_id_screening, participant_sex)
  all(vapply(
    seq_along(levels(reversed_data$participant_sex)),
    function(k) {
      drawn <- unique(reversed_plot[[k]]$layers[[1]]$data$study_id_screening)
      drawn_levels <- baseline_levels %>%
        filter(study_id_screening %in% drawn) %>%
        pull(participant_sex)
      all(drawn_levels == levels(reversed_data$participant_sex)[k])
    },
    logical(1)
  ))
}, error = function(e) FALSE)

## ---- check-figures
# Visual snapshots and export formats (test_figures.R). FALSE, not an error,
# when the code under test predates the figure style, so a comparison run on
# older code still saves its snapshot.
# testthat skips snapshot tests unless NOT_CRAN is set; a skipped test
# counts as a failure here, so a missing comparison never passes silently
Sys.setenv(NOT_CRAN = "true")
figure_results <- tryCatch(
  as.data.frame(testthat::test_file(
    file.path(harness_dir, "test_figures.R"),
    reporter = "summary", stop_on_failure = FALSE
  )),
  error = function(e) NULL
)
figures_pass <- !is.null(figure_results) &&
  all(figure_results$failed == 0) && !any(figure_results$error) &&
  !any(figure_results$skipped)

## ---- check-recovery
# Refit two models directly and check that the 95% confidence interval
# covers the effect built into the synthetic data
act_fit <- lmer(
  act_score ~ time_factor * studyarm + (1 | study_id_screening),
  data = clean_asthma_data_itt
)
act_term <- "time_factor12:studyarm1"
act_ci <- fixef(act_fit)[[act_term]] +
  c(-1, 1) * qnorm(0.975) * sqrt(diag(vcov(act_fit)))[[act_term]]

ed_fit <- glm.nb(
  ed_visit_number_times_12months ~ studyarm + offset(log(followup_time_itt)),
  data = rescue_medication_merged
)
ed_ci <- exp(
  coef(ed_fit)[["studyarm1"]] +
    c(-1, 1) * qnorm(0.975) * sqrt(diag(vcov(ed_fit)))[["studyarm1"]]
)

covers <- function(ci, truth) ci[1] <= truth && truth <= ci[2]

## ---- check-outputs
singular_fits <- sum(grepl("boundary \\(singular\\)", collected_messages))
rank_deficient_fits <- sum(grepl("rank deficient", collected_messages))

# Every object the scripts' headers say they produce, with the expected
# sizes, and the checks that make the synthetic data fit for comparing
# estimates
expected <- list(
  "per-protocol visits exist" = nrow(clean_asthma_data_pp) > 0,
  "per-protocol drops some visits" =
    nrow(clean_asthma_data_pp) < nrow(clean_asthma_data_itt),
  "per-protocol rescue counts not all zero" =
    any(rescue_medication_merged$rescue_puffs_pp > 0),
  "per-protocol controller PDC not all missing" =
    any(!is.na(controller_medication_merged$controller_pdc_pp)),
  "count model table" = inherits(count_model_table, "gtsummary"),
  "count interaction tables: 5 outcomes" =
    length(count_interaction_tables) == 5,
  "mixed model tables: 2 populations" = length(mixed_model_tables) == 2,
  "mixed model tables: 56 in all" =
    sum(lengths(unlist(mixed_model_tables, recursive = FALSE))) == 56,
  "mixed model plots: 56 in all" =
    sum(lengths(unlist(mixed_model_plots, recursive = FALSE))) == 56,
  "table 1" = inherits(table_1, "flextable"),
  "subgroup panels draw their own level's participants" =
    panels_match_levels,
  "figures match their snapshots and save in every format" = figures_pass,
  "every mixed-model cell has 2 or more observations" =
    length(thin_cells) == 0,
  "no singular mixed-model fits" = singular_fits == 0,
  "no rank-deficient mixed-model fits" = rank_deficient_fits == 0,
  "ACT 12-month difference: 95% CI covers the true 2" =
    covers(act_ci, synthetic_truth$act_difference),
  "ED visits rate ratio: 95% CI covers the true 0.7" =
    covers(ed_ci, synthetic_truth$health_care_rate_ratio)
)

## ---- report
cat("\nChecks:\n")
for (check in names(expected)) {
  cat(if (isTRUE(expected[[check]])) "  PASS " else "  FAIL ", check, "\n")
}
if (length(thin_cells) > 0) {
  cat("\nAnalyses with an empty or thin cell:\n")
  cat(paste0("  ", thin_cells, "\n"), sep = "")
}
cat(sprintf(
  "\nRecovered on synthetic data: ACT difference %.2f (%.2f to %.2f); ED rate ratio %.2f (%.2f to %.2f)\n",
  fixef(act_fit)[[act_term]], act_ci[1], act_ci[2],
  exp(coef(ed_fit)[["studyarm1"]]), ed_ci[1], ed_ci[2]
))

tally <- function(x, label) {
  cat("\n", label, " by message (", length(x), " in all):\n", sep = "")
  counts <- sort(table(substr(trimws(x), 1, 150)), decreasing = TRUE)
  for (text in names(counts)) {
    cat("  ", counts[[text]], " x ", text, "\n", sep = "")
  }
}
tally(collected_warnings, "Warnings")
tally(collected_messages, "Messages")

## ---- snapshot
snapshot_file <- Sys.getenv("COHERO_SNAPSHOT")
if (snapshot_file != "") {
  source(file.path(harness_dir, "snapshot.R"))
  saveRDS(snapshot_outputs(), snapshot_file)
  cat("\nSnapshot saved.\n")
}

if (!all(vapply(expected, isTRUE, logical(1)))) {
  quit(status = 1)
}
