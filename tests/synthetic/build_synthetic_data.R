# Purpose: write synthetic stand-ins for the four study files, so the
#   pipeline can be run end to end without the study data.
# Needs: setup.R (item lists, the four file names). Produces: four .xlsx
#   files in the folder named by COHERO_DATA_DIR.
#
# Every value here is made up in code and labelled synthetic. None is derived
# from study data. The file structure (column names, types, sheets) is
# inferred from the analysis code, because the study files are not available
# to check against. Each inference that matters is listed under
# "Assumptions" and marked ASSUMPTION where it is used; when the files are
# available, check these first.
#
# Assumptions
#   A1. REDCap visit dates (consent_date, follow_up_*) are stored as text
#       (YYYY-MM-DD), not Excel dates. If they were Excel dates, read_excel()
#       would return date-times and the comparison with glitch_date in
#       02_build_visit_data.R would mark every visit post-glitch.
#   A2. App PuffDate sits in one of the columns read with col_types "date",
#       so it is read as a date-time. The join with the
#       day grid (a Date) in 03_derive_medication_outcomes.R works for a
#       date-time but would fail for text.
#   A3. StudyPeriod is the day number since consent, so that
#       PuffDate = consent date + StudyPeriod, as the day grid assumes.
#   A4. The ten controller-adherence items are scored 0-4. With 1-5, the
#       adherence score cannot reach the cut-off of 35 and the adherence
#       subgroup would have one level.
#   A5. "Med. Type" is the column header behind Med.Type (spaces are removed
#       on import).
#   A6. Baseline-only REDCap fields (arm, child or adult, language, dates,
#       age, 12-month health care counts) are filled on the baseline row
#       only; follow-up rows leave them empty.
#   A7. Column positions in the two app sheets and the timepoint review
#       follow the col_types in 01_import_data.R; columns the analysis never
#       uses get placeholder names.

## ---- synthetic-settings
set.seed(20261107)
n_participants <- 40
visit_events <- c(
  "baseline_arm_1", "2month_followup_arm_1", "4month_followup_arm_1",
  "6month_followup_arm_1", "9month_followup_arm_1", "12month_followup_arm_1"
)
visit_months <- c(0, 2, 4, 6, 9, 12)

## ---- synthetic-participants
# Arms, groups and consent dates chosen so every arm-by-subgroup cell is
# filled, and so some visits fall after glitch_date and some before
participants <- tibble(
  study_id_screening = seq_len(n_participants),
  studyarm = rep(c(0, 1), length.out = n_participants),
  child_adult = rep(c(1, 1, 2, 2), length.out = n_participants),
  prefer_language = rep(c(1, 2, 1, 1, 2), length.out = n_participants),
  participant_sex = rep(c(1, 2, 2, 1, 1, 2, 1), length.out = n_participants),
  age_screening = round(runif(n_participants, 6, 60)),
  # About a third answer the adherence items as adherent, so both baseline
  # adherence groups exist (see ASSUMPTION A4)
  answers_adherent = rep(c(TRUE, FALSE, FALSE), length.out = n_participants),
  # Most participants finish before the glitch date; some do not
  consent_date = as.Date("2017-01-15") +
    sample(c(0:90, 300:420), n_participants, replace = TRUE),
  ed_visit_number_times_12months = rnbinom(n_participants, mu = 1, size = 1),
  hospitalization_number_times_12months =
    rnbinom(n_participants, mu = 0.5, size = 1),
  urgent_care_number_times_12months =
    rnbinom(n_participants, mu = 1, size = 1)
)
# One preferred language of 999, one missing count and one 999 count, to
# exercise the recodes
participants$prefer_language[3] <- 999
participants$ed_visit_number_times_12months[5] <- NA
participants$urgent_care_number_times_12months[7] <- 999

follow_up_dates <- function(consent, months) {
  as.Date(consent) + round(months * 30.4)
}

## ---- synthetic-redcap-export
redcap_rows <- list()
for (i in seq_len(n_participants)) {
  p <- participants[i, ]
  for (v in seq_along(visit_events)) {
    is_baseline <- v == 1
    # A few follow-up visits missed, never the baseline
    if (!is_baseline && runif(1) < 0.08) next
    is_adult <- p$child_adult == 2

    row <- tibble(
      study_id_screening = p$study_id_screening,
      redcap_event_name = visit_events[v],
      # ASSUMPTION A6: baseline-only fields on the baseline row
      studyarm = if (is_baseline) p$studyarm else NA,
      child_adult = if (is_baseline) p$child_adult else NA,
      prefer_language = if (is_baseline) p$prefer_language else NA,
      age_screening = if (is_baseline) p$age_screening else NA,
      # ASSUMPTION A1: dates stored as text
      consent_date = if (is_baseline) format(p$consent_date) else NA,
      follow_up_2_months =
        if (is_baseline) format(follow_up_dates(p$consent_date, 2)) else NA,
      follow_up_4_months =
        if (is_baseline) format(follow_up_dates(p$consent_date, 4)) else NA,
      follow_up_6_months =
        if (is_baseline) format(follow_up_dates(p$consent_date, 6)) else NA,
      follow_up_9_months =
        if (is_baseline) format(follow_up_dates(p$consent_date, 9)) else NA,
      follow_up_12_month =
        if (is_baseline) format(follow_up_dates(p$consent_date, 12)) else NA,
      ed_visit_number_times_12months =
        if (is_baseline) p$ed_visit_number_times_12months else NA,
      hospitalization_number_times_12months =
        if (is_baseline) p$hospitalization_number_times_12months else NA,
      urgent_care_number_times_12months =
        if (is_baseline) p$urgent_care_number_times_12months else NA,
      act_score = sample(5:25, 1)
    )

    # Quality-of-life items: AQLQ for adults, PACQLQ for children (1-7)
    for (item in aqlq_items) {
      row[[item]] <- if (is_adult) sample(1:7, 1) else NA
    }
    for (item in pacqlq_items) {
      row[[item]] <- if (!is_adult) sample(1:7, 1) else NA
    }
    for (item in afs_items) {
      row[[item]] <- sample(1:5, 1)
    }
    # ASSUMPTION A4: adherence items scored 0-4. Two items add to the score
    # (regularly, as label); the other eight subtract from it.
    adherence_items <- setdiff(missing_999_variables, "act_score")
    positive_items <- c(
      "give_controller_medication_regularly",
      "give_controller_medication_as_label"
    )
    for (item in adherence_items) {
      is_positive <- item %in% positive_items
      row[[item]] <- if (p$answers_adherent) {
        if (is_positive) sample(c(3, 4, 4), 1) else sample(c(0, 0, 0, 1), 1)
      } else {
        sample(0:4, 1)
      }
    }
    redcap_rows[[length(redcap_rows) + 1]] <- row
  }
}
redcap_export <- bind_rows(redcap_rows)
# A few 999 codes, to exercise the recode to missing
redcap_export$act_score[c(4, 11)] <- 999
first_aqlq_rows <- which(!is.na(redcap_export[[aqlq_items[1]]]))
redcap_export[[aqlq_items[1]]][first_aqlq_rows[2]] <- 999

## ---- synthetic-baseline-sex
baseline_sex_table <- participants %>%
  select(study_id_screening, participant_sex)

## ---- synthetic-app-medication-usage
# One row per recorded puff. ASSUMPTION A7: column order follows col_types
# in 01_import_data.R; ASSUMPTION A3: StudyPeriod is days since consent.
medication_names <- c(
  "Flovent", "Symbicort", "Advair", "QVAR", "ProAir", "Ventolin",
  "Proventil", "Combivent", "Inhaler A"
)
puff_rows <- list()
for (i in seq_len(n_participants)) {
  p <- participants[i, ]
  n_puffs <- rpois(1, 150)
  study_period <- sort(sample(0:365, n_puffs, replace = TRUE))
  medication <- sample(medication_names, n_puffs, replace = TRUE)
  puff_rows[[i]] <- tibble(
    UserFk = 1000 + p$study_id_screening,
    `Date Created` = p$consent_date + study_period,
    SUBJECTID = p$study_id_screening,
    StudyPeriod = study_period,
    Dose = 1,
    Medication = medication,
    # ASSUMPTION A2: an Excel date
    PuffDate = p$consent_date + study_period,
    Puffs = 1,
    # ASSUMPTION A5: header "Med. Type"; mostly blank, so relabelling by
    # name is exercised; "Inhaler A" stays unlabelled
    `Med. Type` = ifelse(runif(n_puffs) < 0.2, "Control", NA_character_),
    `Text 1` = NA_character_, `Text 2` = NA_character_,
    `Text 3` = NA_character_, `Text 4` = NA_character_,
    `Text 5` = NA_character_, `Text 6` = NA_character_,
    `Number 1` = 0, `Number 2` = 0,
    `Puff (EST)` = as.POSIXct(p$consent_date + study_period) +
      round(runif(n_puffs, 0, 86399)),
    `Date 1` = as.Date(NA), `Date 2` = as.Date(NA), `Date 3` = as.Date(NA),
    `Date 4` = as.Date(NA)
  )
}
# App test users: records with no study ID, excluded by the analysis
test_users <- tibble(
  UserFk = c(1, 1, 2), SUBJECTID = NA_real_, StudyPeriod = 0,
  Medication = "ProAir", PuffDate = as.Date("2017-03-01")
)
medication_usage <- bind_rows(puff_rows, test_users)

## ---- synthetic-app-notifications
# Not used by the analysis; ten columns to match col_types
notifications <- tibble(
  `Notification Id` = 1:5, SUBJECTID = 1:5, Type = "Reminder",
  Sent = as.Date("2017-03-01") + 0:4, Skipped1 = NA,
  `Number 1` = 0, Message = "Synthetic", Opened = as.Date("2017-03-02") + 0:4,
  Skipped2 = NA, Skipped3 = NA
)

## ---- synthetic-timepoint-review
# Not used by the analysis; seven columns to match col_types
timepoint_review <- tibble(
  `Study ID #` = participants$study_id_screening,
  `Time-point 0` = participants$consent_date,
  `Time-point 2` = follow_up_dates(participants$consent_date, 2),
  `Time-point 4` = follow_up_dates(participants$consent_date, 4),
  `Time-point 6` = follow_up_dates(participants$consent_date, 6),
  `Time-point 9` = follow_up_dates(participants$consent_date, 9),
  `Time-point 12` = follow_up_dates(participants$consent_date, 12)
)

## ---- write-synthetic-files
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
writexl::write_xlsx(redcap_export, redcap_export_file)
writexl::write_xlsx(baseline_sex_table, baseline_sex_file)
writexl::write_xlsx(
  list(
    Notifications = notifications,
    `Medication Usage` = medication_usage
  ),
  push_data_file
)
writexl::write_xlsx(timepoint_review, timepoint_review_file)
