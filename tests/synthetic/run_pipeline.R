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
# Produces: a pass or fail, counts of warnings and messages, and the
#   snapshot, if asked for. Synthetic data and outputs go to temporary
#   folders: COHERO_DATA_DIR and COHERO_OUTPUT_DIR point there while it runs
#   and are put back when it ends, and a check confirms that the project's
#   own tables/ and figures/ are unchanged. Prints no data values, only check
#   results and estimates on synthetic data.

## ---- harness-folder
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
harness_dir <- if (length(script_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", script_arg)))
} else {
  "tests/synthetic"
}

## ---- synthetic-folders
# Synthetic data and every output go to temporary folders. The two settings
# are put back when the run ends (restore_settings(), below), so running this
# with source() in a session cannot leave the session pointed at synthetic
# data or a temporary output folder.
saved_settings <- Sys.getenv(
  c("COHERO_DATA_DIR", "COHERO_OUTPUT_DIR"), unset = NA
)
restore_settings <- function() {
  for (name in names(saved_settings)) {
    if (is.na(saved_settings[[name]])) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, as.list(saved_settings[name]))
    }
  }
}
synthetic_dir <- file.path(tempdir(), "cohero-synthetic-data")
synthetic_output_dir <- file.path(tempdir(), "cohero-synthetic-outputs")
Sys.setenv(
  COHERO_DATA_DIR = synthetic_dir, COHERO_OUTPUT_DIR = synthetic_output_dir
)

# The project's own output folders, as they are before the run: every file's
# size and modification time
project_outputs <- function() {
  files <- list.files(c("tables", "figures"), recursive = TRUE,
                      full.names = TRUE)
  info <- file.info(files)
  paste(files, info$size, format(info$mtime, "%Y-%m-%d %H:%M:%OS3"))
}
project_outputs_before <- project_outputs()

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
  tryCatch(source("R/main.R"), error = function(e) {
    restore_settings()
    stop(e)
  }),
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
    "act_score", modifier = "participant_sex",
    arm_labels = arm_labels, labels = figure_labels
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

## ---- check-row-order
# Script 02 gives the same visit data whatever the order of rows in the
# REDCap export, and stops on a participant with two baseline rows; the sex
# merge in 01 stops on a participant listed twice (issue #48). Each is run in
# its own environment, so the pipeline's objects are untouched.
build_visits <- function(export) {
  env <- new.env(parent = globalenv())
  env$clean_asthma_data <- export
  suppressWarnings(source("R/02_build_visit_data.R", local = env))
  env$clean_asthma_data_itt %>% arrange(study_id_screening, time_linear)
}
row_order_pass <- tryCatch({
  shuffled_export <- withr::with_seed(
    48, clean_asthma_data[sample(nrow(clean_asthma_data)), ]
  )
  isTRUE(all.equal(
    build_visits(clean_asthma_data), build_visits(shuffled_export)
  ))
}, error = function(e) FALSE)

duplicate_baseline_stops <- tryCatch({
  first_baseline <- clean_asthma_data %>%
    filter(redcap_event_name == "baseline_arm_1") %>%
    slice(1)
  build_visits(bind_rows(clean_asthma_data, first_baseline))
  FALSE
}, error = function(e) {
  grepl("more than one baseline", conditionMessage(e), fixed = TRUE)
})

duplicate_sex_stops <- tryCatch({
  sex_file <- tempfile(fileext = ".xlsx")
  sex_records <- readxl::read_excel(baseline_sex_file)
  writexl::write_xlsx(bind_rows(sex_records, sex_records[1, ]), sex_file)
  env <- new.env(parent = globalenv())
  env$baseline_sex_file <- sex_file
  suppressWarnings(suppressMessages(
    source("R/01_import_data.R", local = env)
  ))
  FALSE
}, error = function(e) {
  grepl("at most 1 row", conditionMessage(e), fixed = TRUE)
})

## ---- check-time-zones
# Scripts 01-03 give the same date-dependent results in time zones behind,
# at and far ahead of UTC (check_time_zone.R, each in its own R session).
# Date windows must not move with the computer's time zone.
time_zone_results <- vapply(
  c("Pacific/Honolulu", "UTC", "Pacific/Kiritimati"),
  function(time_zone) {
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      file.path(harness_dir, "check_time_zone.R"),
      stdout = TRUE, stderr = TRUE, env = paste0("TZ=", time_zone)
    ))
    status <- attr(output, "status")
    if (!is.null(status)) {
      return(paste("failed:", paste(output, collapse = " ")))
    }
    utils::tail(output, 1)
  },
  character(1)
)
time_zones_agree <- length(unique(time_zone_results)) == 1 &&
  !any(startsWith(time_zone_results, "failed"))
if (!time_zones_agree) {
  cat("Results by time zone:\n")
  print(time_zone_results)
}

## ---- check-portable
# The model, table and figure functions run in a fresh R session that has
# only the tidyverse attached and only functions.R and plot_style.R loaded,
# on invented data with other column names (test_portable.R). A function that
# reads one of this project's objects fails there. The run's output is shown
# only if it fails.
portable_output <- suppressWarnings(system2(
  file.path(R.home("bin"), "Rscript"),
  file.path(harness_dir, "test_portable.R"),
  stdout = TRUE, stderr = TRUE
))
portable_pass <- is.null(attr(portable_output, "status"))
if (!portable_pass) {
  cat("test_portable.R failed:\n", paste(portable_output, collapse = "\n"),
      "\n")
}

## ---- check-tables
# Every number in the mixed-model tables matches its source, row by row with
# its label (issues #8, #49): the model-term tables match broom.mixed::tidy()
# of each fit by model term, with the fit's two standard deviations and
# intraclass correlation in the note under the table; the effects tables
# match estimate_effects() by visit and by column (a subgroup level, or a
# difference between levels, in the order tabulate_effects() lays them out).
# A number under the wrong visit, level or term fails. FALSE, not an error,
# when the code under test predates these tables.
statistics <- c("estimate", "conf.low", "conf.high", "p.value")
same_rows <- function(table_rows, source_rows, keys) {
  compared <- full_join(
    table_rows, source_rows, by = keys, suffix = c("_table", "_source")
  )
  all(vapply(statistics, function(statistic) {
    isTRUE(all.equal(
      compared[[paste0(statistic, "_table")]],
      compared[[paste0(statistic, "_source")]],
      tolerance = 1e-8
    ))
  }, logical(1)))
}
model_terms_match <- tryCatch(all(unlist(lapply(
  names(mixed_model_fits), function(population) {
    lapply(names(mixed_model_fits[[population]]), function(outcome) {
      lapply(names(mixed_model_fits[[population]][[outcome]]), function(analysis) {
        fit <- mixed_model_fits[[population]][[outcome]][[analysis]]
        table <- mixed_model_tables[[population]][[outcome]][[analysis]]
        terms <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
          filter(term != "(Intercept)") %>%
          select(term, all_of(statistics))
        # Reference levels carry a placeholder estimate, not a model term
        table_rows <- table$table_body %>%
          filter(!is.na(term), is.na(reference_row) | !reference_row) %>%
          select(term, all_of(statistics))
        # The note under the table states the fit's two standard deviations
        standard_deviations <- broom.mixed::tidy(fit, effects = "ran_pars")$estimate
        intraclass_correlation <- standard_deviations[1]^2 /
          sum(standard_deviations^2)
        note <- table$table_styling$source_note$source_note
        same_rows(table_rows, terms, "term") &&
          all(vapply(
            sprintf("%.2f", c(standard_deviations, intraclass_correlation)),
            function(value) any(grepl(value, note, fixed = TRUE)),
            logical(1)
          ))
      })
    })
  }
))), error = function(e) FALSE)

# The effects as tabulate_effects() lays them out: one row per visit, and
# one column group per subgroup level, then per difference between levels
expected_effect_rows <- function(estimates, modifier) {
  by_visit <- function(rows, column) {
    rows %>%
      transmute(
        label = paste("Month", time_factor), column = column,
        across(all_of(statistics))
      )
  }
  if (is.null(modifier)) {
    return(by_visit(estimates$within_levels, ""))
  }
  within <- estimates$within_levels
  levels_shown <- levels(droplevels(factor(within[[modifier]])))
  comparisons <- unique(as.character(estimates$between_levels$comparison))
  bind_rows(
    lapply(seq_along(levels_shown), function(i) {
      by_visit(
        filter(within, as.character(.data[[modifier]]) == levels_shown[i]),
        as.character(i)
      )
    }),
    lapply(seq_along(comparisons), function(j) {
      by_visit(
        filter(
          estimates$between_levels, as.character(comparison) == comparisons[j]
        ),
        as.character(length(levels_shown) + j)
      )
    })
  )
}
table_effect_rows <- function(table_body) {
  table_body %>%
    select(label, matches("^(estimate|conf\\.low|conf\\.high|p\\.value)(_[0-9]+)?$")) %>%
    pivot_longer(
      -label, names_to = c(".value", "column"),
      names_pattern = "^(estimate|conf\\.low|conf\\.high|p\\.value)_?([0-9]*)$"
    )
}
effect_tables_match <- tryCatch(all(unlist(lapply(
  names(mixed_model_estimates), function(population) {
    lapply(names(mixed_model_estimates[[population]]), function(outcome) {
      lapply(names(mixed_model_estimates[[population]][[outcome]]), function(analysis) {
        modifier <- if (analysis == "overall") NULL else analysis
        same_rows(
          table_effect_rows(
            mixed_model_effect_tables[[population]][[outcome]][[analysis]]$table_body
          ),
          expected_effect_rows(
            mixed_model_estimates[[population]][[outcome]][[analysis]], modifier
          ),
          c("label", "column")
        )
      })
    })
  }
))), error = function(e) FALSE)

## ---- check-model-warnings
# The count models fit without convergence warnings (review finding F19)
count_model_warnings <- sum(grepl(
  "iteration limit reached|alternation limit reached", collected_warnings
))

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
  "mixed model effect tables: 56 in all" = tryCatch(
    sum(lengths(unlist(mixed_model_effect_tables, recursive = FALSE))) == 56,
    error = function(e) FALSE
  ),
  "model-term tables match the fitted models" = model_terms_match,
  "effect tables match the estimates" = effect_tables_match,
  "count models fit without convergence warnings" = count_model_warnings == 0,
  "outputs saved only in the temporary folder" = tryCatch(
    startsWith(normalizePath(tables_dir), normalizePath(tempdir())),
    error = function(e) FALSE
  ),
  "the project's own tables/ and figures/ are unchanged" =
    identical(project_outputs(), project_outputs_before),
  "every table saved as a Word file" = tryCatch(
    length(list.files(tables_dir, pattern = "\\.docx$")) ==
      6 + sum(lengths(count_interaction_tables)) + 2 * 56,
    error = function(e) FALSE
  ),
  "mixed model plots: 56 in all" =
    sum(lengths(unlist(mixed_model_plots, recursive = FALSE))) == 56,
  "table 1" = inherits(table_1, "flextable"),
  "subgroup panels draw their own level's participants" =
    panels_match_levels,
  "figures match their snapshots and save in every format" = figures_pass,
  "model, table and figure functions work outside this project" =
    portable_pass,
  "date windows are the same in every time zone" = time_zones_agree,
  "visit data do not depend on row order" = row_order_pass,
  "a second baseline row stops the run" = duplicate_baseline_stops,
  "a participant listed twice in the sex file stops the run" =
    duplicate_sex_stops,
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

restore_settings()

if (!all(vapply(expected, isTRUE, logical(1)))) {
  quit(status = 1)
}
