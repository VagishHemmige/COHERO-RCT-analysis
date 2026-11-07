# Purpose: read the four study files and tidy their column names.
# Needs: setup.R (file paths). Produces: RawAsthmaData,
#   EMASTHMA_DATA_Baseline_gender, CleanAsthmaData (REDCap export joined to
#   sex), RawNotifications, RawMedications, RawFollowupdates.
# Ported from the ImportData chunk of MR_SJ_asthma_pilot_2023-9-15.Rmd
#   (reading steps only; cleaning is in 02_visits.R).

## ---- import-redcap
# Raw data export from REDCap
RawAsthmaData <- read_excel(redcap_export_file)

# Sex is in a separate file
EMASTHMA_DATA_Baseline_gender <- read_excel(baseline_sex_file) %>%
  select(study_id_screening, participant_sex)

# Merge sex into the REDCap export
CleanAsthmaData <- left_join(RawAsthmaData, EMASTHMA_DATA_Baseline_gender, by = "study_id_screening")

## ---- import-notifications
RawNotifications <- read_excel(push_data_file,
                               sheet = "Notifications",
                               col_types = c("numeric", "numeric", "text", "date", "skip",
                                             "numeric", "text", "date", "skip", "skip"))

# Remove spaces from names
names(RawNotifications) <- names(RawNotifications) %>%
  str_replace_all(fixed(" "), "")

## ---- import-medications
# suppressWarnings avoids one warning per missing cell
RawMedications <- read_excel(push_data_file,
                             sheet = "Medication Usage",
                             col_types = c("numeric", "date", "numeric", "numeric", "numeric", "text", "date", "numeric", "text",
                                           "text", "text", "text", "text", "text", "text", "numeric", "numeric", "date", "date",
                                           "date", "date", "date")) %>%
  suppressWarnings()

# Remove spaces from names
names(RawMedications) <- names(RawMedications) %>%
  str_replace_all(fixed(" "), "")

## ---- import-followup-dates
# suppressWarnings avoids one warning per missing cell
RawFollowupdates <- read_excel(timepoint_review_file,
                               col_types = c("numeric", "date", "date", "date", "date", "date", "date")) %>%
  suppressWarnings()

# Remove spaces and other troublesome characters from names
names(RawFollowupdates) <- names(RawFollowupdates) %>%
  str_replace_all(fixed(" "), "") %>%
  str_replace_all(fixed("#"), "") %>%
  str_replace_all(fixed("-"), "")
