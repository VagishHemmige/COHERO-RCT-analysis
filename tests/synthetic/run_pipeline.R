# Purpose: run the whole analysis on synthetic data and check that it
#   finishes and produces every expected object. Used by
#   .github/workflows/test-pipeline.yml; can also be run locally.
# Run from the project root: Rscript tests/synthetic/run_pipeline.R
# Produces: a pass or fail, a count of warnings by message, and nothing
#   written outside a temporary folder. Prints no data values.

## ---- synthetic-data-folder
synthetic_dir <- file.path(tempdir(), "cohero-synthetic-data")
Sys.setenv(COHERO_DATA_DIR = synthetic_dir)

## ---- build-synthetic-data
source("R/setup.R")
source("tests/synthetic/build_synthetic_data.R")
cat("Synthetic files written:", length(list.files(synthetic_dir)), "\n")

## ---- run-pipeline
# Run main.R, collecting warnings by message rather than printing them as
# they happen
collected_warnings <- character()
withCallingHandlers(
  source("R/main.R"),
  warning = function(w) {
    collected_warnings <<- c(collected_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
cat("main.R finished.\n")

## ---- check-outputs
# Every object the scripts' headers say they produce, with the expected sizes
expected <- list(
  "per-protocol visits exist" = nrow(clean_asthma_data_pp) > 0,
  "per-protocol drops some visits" =
    nrow(clean_asthma_data_pp) < nrow(clean_asthma_data_itt),
  "count model table" = inherits(count_model_table, "gtsummary"),
  "count interaction tables: 5 outcomes" =
    length(count_interaction_tables) == 5,
  "mixed model tables: 2 populations" = length(mixed_model_tables) == 2,
  "mixed model tables: 56 in all" =
    sum(lengths(unlist(mixed_model_tables, recursive = FALSE))) == 56,
  "mixed model plots: 56 in all" =
    sum(lengths(unlist(mixed_model_plots, recursive = FALSE))) == 56,
  "table 1" = inherits(table_1, "flextable")
)

# Reported but not failing: suspected defects in the original code, to be
# confirmed here and fixed in their own pull requests. If PuffDate is read as
# a date-time, PuffDate <= glitch_date compares a date-time with a Date,
# which R may compare as raw numbers; whether current R does is unconfirmed.
watch <- list(
  "per-protocol rescue counts not all zero" =
    any(rescue_medication_merged$rescue_puffs_pp > 0),
  "per-protocol controller PDC not all missing" =
    any(!is.na(controller_medication_merged$controller_pdc_pp))
)

## ---- report
cat("\nChecks:\n")
for (check in names(expected)) {
  cat(if (isTRUE(expected[[check]])) "  PASS " else "  FAIL ", check, "\n")
}

cat("\nWatched (suspected defects; these do not fail the run):\n")
for (check in names(watch)) {
  cat(if (isTRUE(watch[[check]])) "  OK    " else "  WATCH ", check, "\n")
}

cat("\nWarnings by message (", length(collected_warnings), " in all):\n",
    sep = "")
warning_counts <- sort(table(collected_warnings), decreasing = TRUE)
for (message in names(warning_counts)) {
  cat("  ", warning_counts[[message]], " x ", substr(message, 1, 150), "\n",
      sep = "")
}

if (!all(vapply(expected, isTRUE, logical(1)))) {
  quit(status = 1)
}
