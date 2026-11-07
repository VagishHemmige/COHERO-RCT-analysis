# Purpose: build the visit-level analysis data for the ITT and per-protocol
#   populations, with recodes and scale scores.
# Needs: setup.R (item lists, missing_999_variables, glitch_date,
#   adherence_cutoff, act_cutoff); 01_import_data.R (clean_asthma_data).
# Produces: clean_asthma_data_itt (all visits), clean_asthma_data_pp (visits
#   on or before glitch_date), study_dates (one row per participant with
#   consent and follow-up visit dates).
# Ported from the cleaning steps of the ImportData chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd.

## ---- build-visit-data
clean_asthma_data_itt <- clean_asthma_data %>%
  # Months since baseline for each REDCap event
  mutate(time_linear = case_when(
    redcap_event_name == "baseline_arm_1" ~ 0,
    redcap_event_name == "2month_followup_arm_1" ~ 2,
    redcap_event_name == "4month_followup_arm_1" ~ 4,
    redcap_event_name == "6month_followup_arm_1" ~ 6,
    redcap_event_name == "9month_followup_arm_1" ~ 9,
    redcap_event_name == "12month_followup_arm_1" ~ 12
  )) %>%
  # A preferred language of 999 is set to English. See issue #16.
  mutate(prefer_language = ifelse(
    prefer_language == 999,
    1,
    prefer_language
  )) %>%
  # Visit time as a factor, for models that treat time as categorical
  mutate(time_factor = as_factor(time_linear)) %>%
  mutate(studyarm = as.factor(studyarm)) %>%
  mutate(participant_sex = factor(
    participant_sex,
    levels = c(1, 2),
    labels = c("Male", "Female")
  )) %>%
  mutate(child_adult = factor(
    child_adult,
    levels = c(1, 2),
    labels = c("Child", "Adult")
  )) %>%
  mutate(prefer_language = factor(
    prefer_language,
    levels = c(1, 2),
    labels = c("English", "Spanish")
  )) %>%
  # These are recorded only at baseline; carry them to every visit
  group_by(study_id_screening) %>%
  fill(
    studyarm, child_adult, prefer_language, consent_date,
    follow_up_2_months, follow_up_4_months, follow_up_6_months,
    follow_up_9_months, follow_up_12_month
  ) %>%
  ungroup() %>%
  mutate(across(
    all_of(c(aqlq_items, pacqlq_items, afs_items, missing_999_variables)),
    ~ ifelse(.x == 999, NA, .x)
  )) %>%
  # Date of each visit, from the consent and follow-up dates
  mutate(date_of_observation = case_when(
    redcap_event_name == "baseline_arm_1" ~ consent_date,
    redcap_event_name == "2month_followup_arm_1" ~ follow_up_2_months,
    redcap_event_name == "4month_followup_arm_1" ~ follow_up_4_months,
    redcap_event_name == "6month_followup_arm_1" ~ follow_up_6_months,
    redcap_event_name == "9month_followup_arm_1" ~ follow_up_9_months,
    redcap_event_name == "12month_followup_arm_1" ~ follow_up_12_month
  )) %>%
  # Visits after the app update glitch
  mutate(post_glitch_date = (date_of_observation > glitch_date)) %>%
  # Visit-by-arm cell label, and a time offset so arms do not overlap in plots
  mutate(me_factor = paste(
    "time", time_linear, "studyarm", studyarm,
    sep = "_"
  )) %>%
  mutate(time_linear_shifted = ifelse(
    studyarm == 1,
    time_linear + 0.1,
    time_linear - 0.1
  )) %>%
  # Self-reported controller adherence score. See analysis-plan.md.
  mutate(self_reported_adherence = 32 -
    only_give_controller_medication_needed -
    only_give_controller_medication_breathless +
    give_controller_medication_regularly -
    try_to_avoid_controller_medication -
    forget_give_controller_medication -
    alter_change_dosage_controller_medication -
    stop_giving_controller_medication -
    reserve_controller_medication_if_treatment_not_work -
    give_controller_medication_child_does_breathless +
    give_controller_medication_as_label) %>%
  # Baseline adherence and asthma control groups, carried to every visit
  mutate(self_reported_adherence_binary = case_when(
    self_reported_adherence >= adherence_cutoff &
      time_factor == "0" ~ "Adherent",
    self_reported_adherence < adherence_cutoff &
      time_factor == "0" ~ "Non-adherent"
  )) %>%
  mutate(
    self_reported_adherence_binary = as.factor(self_reported_adherence_binary)
  ) %>%
  mutate(act_binary = case_when(
    act_score >= act_cutoff & time_factor == "0" ~ "Controlled",
    act_score < act_cutoff & time_factor == "0" ~ "Uncontrolled"
  )) %>%
  mutate(act_binary = as.factor(act_binary)) %>%
  group_by(study_id_screening) %>%
  fill(self_reported_adherence_binary, act_binary) %>%
  ungroup() %>%
  # Each score is the mean of the items answered at that visit
  mutate(aqlq_score = rowMeans(across(all_of(aqlq_items)), na.rm = TRUE)) %>%
  mutate(
    pacqlq_score = rowMeans(across(all_of(pacqlq_items)), na.rm = TRUE)
  ) %>%
  mutate(afs_score = rowMeans(across(all_of(afs_items)), na.rm = TRUE))

## ---- per-protocol-population
clean_asthma_data_pp <- clean_asthma_data_itt %>%
  filter(post_glitch_date == FALSE)

## ---- study-dates
# One row per participant with consent and follow-up visit dates
study_dates <- clean_asthma_data_itt %>%
  filter(time_linear == 0) %>%
  select(
    study_id_screening, consent_date, follow_up_2_months,
    follow_up_4_months, follow_up_6_months, follow_up_9_months,
    follow_up_12_month
  )
