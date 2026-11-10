# Purpose: build the visit-level analysis data for the ITT and per-protocol
#   populations, with recodes and scale scores.
# Needs: setup.R (item lists, missing_999_variables, glitch_date,
#   adherence_cutoff, act_cutoff, baseline_variables); 01_import_data.R
#   (clean_asthma_data).
# Produces: clean_asthma_data_itt (all visits), clean_asthma_data_pp (visits
#   on or before glitch_date), study_dates (one row per participant with
#   consent and follow-up visit dates).
# Ported from the cleaning steps of the ImportData chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd.

## ---- visit-times
# Months since baseline for each REDCap event, and the preferred-language
# recode, before baseline values are copied to other visits
visits <- clean_asthma_data %>%
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
  ))

## ---- baseline-values
# The values recorded only at baseline, one row per participant, to copy to
# their other visits (issue #48). Taken from the baseline row itself, so the
# order of rows in the export does not matter. A participant with two
# baseline rows stops the run; one with visits but no baseline row keeps
# missing values, with a warning.
baseline_values <- visits %>%
  filter(time_linear == 0) %>%
  select(study_id_screening, all_of(baseline_variables))
repeated_baselines <- baseline_values %>%
  count(study_id_screening) %>%
  filter(n > 1)
if (nrow(repeated_baselines) > 0) {
  stop(
    nrow(repeated_baselines), " participant(s) have more than one baseline ",
    "row; each needs exactly one.", call. = FALSE
  )
}
no_baseline <- setdiff(
  visits$study_id_screening, baseline_values$study_id_screening
)
if (length(no_baseline) > 0) {
  warning(
    length(no_baseline), " participant(s) have visits but no baseline row; ",
    "their baseline values stay missing.", call. = FALSE
  )
}

## ---- build-visit-data
clean_asthma_data_itt <- visits %>%
  # Visit time as a factor, for models that treat time as categorical. The
  # levels are in time order, baseline first, whatever the row order.
  mutate(time_factor = factor(
    time_linear, levels = sort(unique(time_linear))
  )) %>%
  # These are recorded only at baseline: a visit keeps its own value if it
  # has one, and otherwise takes the participant's baseline value
  left_join(
    baseline_values, join_by(study_id_screening),
    suffix = c("", "_at_baseline"), relationship = "many-to-one"
  ) %>%
  mutate(across(
    all_of(baseline_variables),
    ~ coalesce(.x, pick(everything())[[paste0(cur_column(), "_at_baseline")]])
  )) %>%
  select(-ends_with("_at_baseline")) %>%
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
  # Each participant's baseline group, on every visit
  group_by(study_id_screening) %>%
  mutate(across(
    c(self_reported_adherence_binary, act_binary),
    ~ .x[which(time_linear == 0)][1]
  )) %>%
  ungroup() %>%
  # Each score is the mean of the items answered at that visit
  mutate(aqlq_score = rowMeans(across(all_of(aqlq_items)), na.rm = TRUE)) %>%
  mutate(
    pacqlq_score = rowMeans(across(all_of(pacqlq_items)), na.rm = TRUE)
  ) %>%
  mutate(afs_score = rowMeans(across(all_of(afs_items)), na.rm = TRUE)) %>%
  # A score with no answered items is missing (rowMeans() gives NaN)
  mutate(across(c(aqlq_score, pacqlq_score, afs_score), ~ replace(.x, is.nan(.x), NA)))

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
