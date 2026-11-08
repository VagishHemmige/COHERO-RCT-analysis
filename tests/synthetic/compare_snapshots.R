# Purpose: report what differs between two snapshots of the analysis outputs
#   on synthetic data, for example a pull request against main.
# Run: Rscript tests/synthetic/compare_snapshots.R base.rds proposed.rds
# Produces: one line per output (identical, differs or missing) and the first
#   difference for each output that differs. Report only: it never fails the
#   run, because a pull request may change outputs on purpose.

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
outputs <- union(names(base), names(proposed))
status <- vapply(outputs, function(name) {
  if (!name %in% names(base)) {
    "new"
  } else if (!name %in% names(proposed)) {
    "removed"
  } else if (identical(base[[name]], proposed[[name]])) {
    "identical"
  } else {
    "differs"
  }
}, character(1))

## ---- report
cat("Outputs compared:", length(outputs), "\n")
for (s in c("identical", "differs", "new", "removed")) {
  cat(sprintf("  %-9s %d\n", s, sum(status == s)))
}
for (name in outputs[status != "identical"]) {
  cat("\n", toupper(status[[name]]), ": ", name, "\n", sep = "")
  if (status[[name]] == "differs") {
    difference <- all.equal(base[[name]], proposed[[name]])
    cat(paste0("  ", head(difference, 3), "\n"), sep = "")
  }
}
