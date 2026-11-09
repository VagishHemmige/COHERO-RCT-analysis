# Purpose: collect the analysis outputs into one list, so two runs (for
#   example a pull request and main) can be compared. Synthetic data only.
# Needs: the objects main.R produces. Produces: snapshot_outputs(), a
#   function returning a named list of plain data frames.

## ---- snapshot-helpers
# Plain data from each kind of output; anything that fails is recorded as
# the error message, so one missing output never stops the snapshot
snapshot_value <- function(expr) {
  tryCatch(expr, error = function(e) paste("not available:", conditionMessage(e)))
}

built_plot_data <- function(p) {
  plots <- if (inherits(p, "patchwork")) {
    lapply(seq_along(p), function(i) p[[i]])
  } else {
    list(p)
  }
  suppressWarnings(lapply(plots, function(q) {
    lapply(ggplot2::ggplot_build(q)$data, as.data.frame)
  }))
}

table_data <- function(x) {
  if (inherits(x, "gtsummary")) as.data.frame(x$table_body)
  else if (inherits(x, "flextable")) as.data.frame(x$body$dataset)
  else x
}

flatten_named <- function(x, prefix = NULL) {
  out <- list()
  for (name in names(x)) {
    key <- paste(c(prefix, name), collapse = " / ")
    if (is.list(x[[name]]) && !inherits(x[[name]], c("gtsummary", "flextable",
                                                      "ggplot", "patchwork"))) {
      out <- c(out, flatten_named(x[[name]], key))
    } else {
      out[[key]] <- x[[name]]
    }
  }
  out
}

## ---- snapshot-outputs
snapshot_outputs <- function() {
  snapshot <- list()
  add <- function(group, items, convert) {
    for (name in names(items)) {
      snapshot[[paste(group, name, sep = ": ")]] <<-
        snapshot_value(convert(items[[name]]))
    }
  }
  data_frames <- c(
    "clean_asthma_data_itt", "clean_asthma_data_pp",
    "rescue_medication_merged", "controller_medication_merged",
    "controller_pdc_quarter", "medication_quarter_counts"
  )
  for (name in data_frames) {
    snapshot[[paste("data:", name)]] <-
      snapshot_value(as.data.frame(get(name, envir = globalenv())))
  }
  add("table", list(
    table_1 = snapshot_value(table_1),
    count_outcome_summary = snapshot_value(count_outcome_summary),
    count_model_table = snapshot_value(count_model_table),
    controller_pdc_comparison = snapshot_value(controller_pdc_comparison)
  ), table_data)
  add("count interaction table", snapshot_value(flatten_named(count_interaction_tables)), table_data)
  add("mixed model table", snapshot_value(flatten_named(mixed_model_tables)), table_data)
  if (exists("mixed_model_effect_tables")) {
    add("mixed model effect table", snapshot_value(flatten_named(mixed_model_effect_tables)), table_data)
  }
  add("mixed model plot", snapshot_value(flatten_named(mixed_model_plots)), built_plot_data)
  add("count outcome plot", snapshot_value(count_outcome_plots), built_plot_data)
  snapshot
}
