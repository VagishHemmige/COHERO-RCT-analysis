# Purpose: run the import, visit and medication scripts (01-03) on the
#   synthetic data and print their date-dependent results on one line, so
#   run_pipeline.R can run this in several time zones and require the same
#   line from each. Also checks as_calendar_date() on each form a date can be
#   read in. Synthetic data only; reads the folder named by COHERO_DATA_DIR,
#   which run_pipeline.R has already filled.

suppressPackageStartupMessages(suppressWarnings({
  source("R/setup.R")
  source("R/functions.R")
  source("R/01_import_data.R")
  source("R/02_build_visit_data.R")
  source("R/03_derive_medication_outcomes.R")
}))

## ---- calendar-date-forms
# A record late in the day keeps its recorded day in every time zone
stopifnot(
  identical(
    as_calendar_date(as.POSIXct("2018-05-16 23:30:00", tz = "UTC")),
    as.Date("2018-05-16")
  ),
  identical(as_calendar_date("2018-05-16"), as.Date("2018-05-16")),
  identical(as_calendar_date(as.Date("2018-05-16")), as.Date("2018-05-16")),
  inherits(try(as_calendar_date("5/16/2018"), silent = TRUE), "try-error")
)

## ---- date-dependent-results
cat(
  "records", nrow(medication_data_clean),
  "rescue_puffs", sum(rescue_medication_merged$rescue_puffs_itt, na.rm = TRUE),
  "rescue_days", sum(rescue_medication_merged$rescue_days_itt, na.rm = TRUE),
  "rescue_puffs_pp", sum(rescue_medication_merged$rescue_puffs_pp, na.rm = TRUE),
  "pdc_itt", format(sum(controller_medication_merged$controller_pdc_itt,
                        na.rm = TRUE), digits = 12),
  "pdc_pp", format(sum(controller_medication_merged$controller_pdc_pp,
                       na.rm = TRUE), digits = 12),
  "followup_itt", format(sum(baseline_clean_data$followup_time_itt,
                             na.rm = TRUE), digits = 12),
  "followup_pp", format(sum(baseline_clean_data$followup_time_pp,
                            na.rm = TRUE), digits = 12),
  "pp_visits", nrow(clean_asthma_data_pp),
  "interval_records", sum(!is.na(medication_data_quarter$quarter)),
  "interval_days", sum(as.numeric(medication_data_quarter$quarter_number_days),
                       na.rm = TRUE),
  "\n"
)
