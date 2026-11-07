# Purpose: read the four study files and tidy their column names.
# Needs: setup.R (file paths). Produces: raw_asthma_data,
#   baseline_sex, clean_asthma_data (REDCap export joined to
#   sex), raw_notifications, raw_medications, RawFollowupdates.
# Ported from the ImportData chunk of MR_SJ_asthma_pilot_2023-9-15.Rmd
#   (reading steps only; cleaning is in 02_build_visit_data.R).

## ---- import-redcap
# Raw data export from REDCap
raw_asthma_data <- read_excel(redcap_export_file)

# Sex is in a separate file
baseline_sex <- read_excel(baseline_sex_file) %>%
  select(study_id_screening, participant_sex)

# Merge sex into the REDCap export
clean_asthma_data <- left_join(
  raw_asthma_data,
  baseline_sex,
  by = "study_id_screening"
)

## ---- import-notifications
raw_notifications <- read_excel(
  push_data_file,
  sheet = "Notifications",
  col_types = c(
    "numeric", "numeric", "text", "date", "skip",
    "numeric", "text", "date", "skip", "skip"
  )
)

# Remove spaces from names
names(raw_notifications) <- names(raw_notifications) %>%
  str_replace_all(fixed(" "), "")

## ---- import-medications
# suppressWarnings avoids one warning per missing cell
raw_medications <- read_excel(
  push_data_file,
  sheet = "Medication Usage",
  col_types = c(
    "numeric", "date", "numeric", "numeric", "numeric", "text", "date",
    "numeric", "text", "text", "text", "text", "text", "text", "text",
    "numeric", "numeric", "date", "date", "date", "date", "date"
  )
) %>%
  suppressWarnings()

# Remove spaces from names
names(raw_medications) <- names(raw_medications) %>%
  str_replace_all(fixed(" "), "")

## ---- import-followup-dates
# suppressWarnings avoids one warning per missing cell
raw_followup_dates <- read_excel(
  timepoint_review_file,
  col_types = c("numeric", "date", "date", "date", "date", "date", "date")
) %>%
  suppressWarnings()

# Remove spaces and other troublesome characters from names
names(raw_followup_dates) <- names(raw_followup_dates) %>%
  str_replace_all(fixed(" "), "") %>%
  str_replace_all(fixed("#"), "") %>%
  str_replace_all(fixed("-"), "")
