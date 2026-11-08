# Purpose: report what differs between two snapshots of the analysis outputs
#   on synthetic data, for example a pull request against main.
# Run: Rscript tests/synthetic/compare_snapshots.R base.rds proposed.rds
# Produces: one count per status and the first differences for each output
#   that is not identical. An output whose values match once the columns of
#   every data frame are sorted by name is reported as "reordered", not as
#   "differs": ggplot2 versions can order built-plot columns differently for
#   the same plot. Report only: it never fails the run, because a pull
#   request may change outputs on purpose.

## ---- read-snapshots
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: compare_snapshots.R base.rds proposed.rds")
}
if (!file.exists(args[1])) {
  cat("No snapshot from the base branch (its run failed or it predates",
      "snapshots), so there is nothing to compare.\n")
  quit(status = 0)
}
base <- readRDS(args[1])
proposed <- readRDS(args[2])

## ---- compare
# Sort the columns of every data frame by name, at any depth
sort_columns <- function(x) {
  if (is.data.frame(x)) {
    x[sort(names(x))]
  } else if (is.list(x)) {
    lapply(x, sort_columns)
  } else {
    x
  }
}

outputs <- union(names(base), names(proposed))
status <- vapply(outputs, function(name) {
  if (!name %in% names(base)) {
    "new"
  } else if (!name %in% names(proposed)) {
    "removed"
  } else if (identical(base[[name]], proposed[[name]])) {
    "identical"
  } else if (identical(sort_columns(base[[name]]),
                       sort_columns(proposed[[name]]))) {
    "reordered"
  } else {
    "differs"
  }
}, character(1))

## ---- report
cat("Outputs compared:", length(outputs), "\n")
for (s in c("identical", "reordered", "differs", "new", "removed")) {
  cat(sprintf("  %-9s %d\n", s, sum(status == s)))
}
for (name in outputs[status != "identical"]) {
  cat("\n", toupper(status[[name]]), ": ", name, "\n", sep = "")
  if (status[[name]] == "differs") {
    difference <- all.equal(
      sort_columns(base[[name]]), sort_columns(proposed[[name]])
    )
    cat(paste0("  ", head(difference, 3), "\n"), sep = "")
  }
}
