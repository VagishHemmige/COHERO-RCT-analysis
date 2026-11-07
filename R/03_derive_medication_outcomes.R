# Purpose: clean the app-recorded inhaler data, count rescue use, compute
#   controller proportion of days covered (PDC), and build the
#   participant-level data for the count models.
# Needs: setup.R (controller_medication_names, rescue_medication_names,
#   glitch_date, n_subject_ids, hospital_count_outcomes); 01_import_data.R
#   (raw_medications); 02_build_visit_data.R (clean_asthma_data_itt,
#   clean_asthma_data_pp, study_dates).
# Produces: medication_data_clean, total_rescue_medication_uses,
#   control_medication, controller_pdc, controller_pdc_quarter,
#   baseline_clean_data, rescue_medication_merged,
#   controller_medication_merged, medication_quarter_counts; adds
#   interval-level controller PDC to clean_asthma_data_itt and
#   clean_asthma_data_pp.
# Ported from the medication_data_processing and medicationquarteranalysis
#   chunks of MR_SJ_asthma_pilot_2023-9-15.Rmd. The chunk's data checks are
#   in R/exploratory/check_medication.R.

## ---- clean-medication-records
# Records with no study ID are not participants and are excluded from all
# analyses (study team decision). Records are relabelled controller or rescue
# by medication name where the app's own label is not Control or Rescue.
medication_data_clean <- raw_medications %>%
  filter(!is.na(SUBJECTID)) %>%
  mutate(med_type_clean = case_when(
    Med.Type == "Control" ~ "Control",
    Med.Type == "Rescue" ~ "Rescue",
    str_detect(Medication, paste(controller_medication_names, collapse = "|")) ~
      "Control",
    str_detect(Medication, paste(rescue_medication_names, collapse = "|")) ~
      "Rescue"
  )) %>%
  left_join(study_dates, by = c("SUBJECTID" = "study_id_screening")) %>%
  # Drop records after the 12-month visit. See issue #3 for the lower bound.
  filter(PuffDate <= follow_up_12_month)

## ---- count-rescue-use
rescue_medication <- medication_data_clean %>%
  filter(med_type_clean == "Rescue") %>%
  select(SUBJECTID, StudyPeriod, Medication, PuffDate, `Puff(EST)`)

# One row per participant: rescue puffs and rescue days, ITT
total_rescue_medication_uses <- rescue_medication %>%
  group_by(SUBJECTID) %>%
  summarize(
    rescue_puffs_itt = n(),
    rescue_days_itt = n_distinct(PuffDate)
  ) %>%
  ungroup() %>%
  select(SUBJECTID, rescue_puffs_itt, rescue_days_itt)

# Add the per-protocol counts (records on or before the glitch date)
total_rescue_medication_uses <- rescue_medication %>%
  filter(PuffDate <= glitch_date) %>%
  group_by(SUBJECTID) %>%
  summarize(
    rescue_puffs_pp = n(),
    rescue_days_pp = n_distinct(PuffDate)
  ) %>%
  ungroup() %>%
  select(SUBJECTID, rescue_puffs_pp, rescue_days_pp) %>%
  right_join(total_rescue_medication_uses)

## ---- count-controller-use
# Controller puffs and uses per participant per day
control_medication <- medication_data_clean %>%
  filter(med_type_clean == "Control") %>%
  select(SUBJECTID, StudyPeriod, Medication, PuffDate, `Puff(EST)`) %>%
  group_by(SUBJECTID, StudyPeriod, PuffDate) %>%
  mutate(control_puffs = n()) %>%
  mutate(control_uses = n_distinct(`Puff(EST)`)) %>%
  select(SUBJECTID, StudyPeriod, control_puffs, control_uses, PuffDate) %>%
  distinct()

## ---- controller-day-grid
# Every participant-day from consent to the 12-month visit, with the visit
# interval it falls in. See issues #4 and #15.
full_date_grid <- expand.grid(
  seq(1, n_subject_ids, 1),
  unique(medication_data_clean$StudyPeriod)
) %>%
  rename(SUBJECTID = Var1, StudyPeriod = Var2) %>%
  left_join(study_dates, by = c("SUBJECTID" = "study_id_screening")) %>%
  arrange(SUBJECTID, StudyPeriod) %>%
  mutate(consent_date = as.Date(consent_date)) %>%
  mutate(PuffDate = consent_date + StudyPeriod) %>%
  filter(PuffDate <= follow_up_12_month) %>%
  mutate(quarter = case_when(
    (PuffDate >= consent_date) & (PuffDate < follow_up_2_months) ~
      "0-2 months",
    (PuffDate >= follow_up_2_months) & (PuffDate < follow_up_4_months) ~
      "2-4 months",
    (PuffDate >= follow_up_4_months) & (PuffDate < follow_up_6_months) ~
      "4-6 months",
    (PuffDate >= follow_up_6_months) & (PuffDate < follow_up_9_months) ~
      "6-9 months",
    (PuffDate >= follow_up_9_months) & (PuffDate <= follow_up_12_month) ~
      "9-12 months"
  )) %>%
  # Interval controller PDC is attached to the visit that closes the interval
  mutate(time_factor = case_when(
    (PuffDate >= consent_date) & (PuffDate < follow_up_2_months) ~ "2",
    (PuffDate >= follow_up_2_months) & (PuffDate < follow_up_4_months) ~ "4",
    (PuffDate >= follow_up_4_months) & (PuffDate < follow_up_6_months) ~ "6",
    (PuffDate >= follow_up_6_months) & (PuffDate < follow_up_9_months) ~ "9",
    (PuffDate >= follow_up_9_months) & (PuffDate <= follow_up_12_month) ~ "12"
  )) %>%
  mutate(time_factor = as.factor(time_factor)) %>%
  select(SUBJECTID, StudyPeriod, PuffDate, quarter, time_factor)

# Days with no controller record get zero puffs and uses
control_medication <- right_join(control_medication, full_date_grid) %>%
  mutate(control_puffs = ifelse(is.na(control_puffs), 0, control_puffs)) %>%
  mutate(control_uses = ifelse(is.na(control_uses), 0, control_uses)) %>%
  arrange(SUBJECTID, StudyPeriod)

## ---- controller-pdc
# Proportion of days with at least one controller puff, for the year
controller_pdc <- control_medication %>%
  mutate(day_is_covered = ifelse(control_puffs > 0, 1, 0)) %>%
  group_by(SUBJECTID) %>%
  summarize(controller_pdc_itt = mean(day_is_covered)) %>%
  ungroup()

controller_pdc <- control_medication %>%
  mutate(day_is_covered = ifelse(control_puffs > 0, 1, 0)) %>%
  filter(PuffDate <= glitch_date) %>%
  group_by(SUBJECTID) %>%
  summarize(controller_pdc_pp = mean(day_is_covered)) %>%
  ungroup() %>%
  right_join(controller_pdc)

# The same, within each visit interval
controller_pdc_quarter <- control_medication %>%
  mutate(day_is_covered = ifelse(control_puffs > 0, 1, 0)) %>%
  group_by(SUBJECTID, time_factor) %>%
  summarize(controller_pdc_quarter_itt = mean(day_is_covered)) %>%
  rename(study_id_screening = SUBJECTID) %>%
  ungroup()

controller_pdc_quarter <- control_medication %>%
  mutate(day_is_covered = ifelse(control_puffs > 0, 1, 0)) %>%
  filter(PuffDate <= glitch_date) %>%
  group_by(SUBJECTID, time_factor) %>%
  summarize(controller_pdc_quarter_pp = mean(day_is_covered)) %>%
  rename(study_id_screening = SUBJECTID) %>%
  ungroup() %>%
  right_join(controller_pdc_quarter)

## ---- baseline-count-data
# One row per participant: baseline values and 12-month health care use
baseline_clean_data <- clean_asthma_data_itt %>%
  filter(time_linear == 0) %>%
  # A missing count is set to 0
  mutate(across(
    .cols = all_of(hospital_count_outcomes),
    .fns = ~ ifelse(is.na(.x), 0, .x)
  )) %>%
  # A count of 999 is set to missing
  mutate(across(
    .cols = all_of(hospital_count_outcomes),
    .fns = ~ ifelse(.x == 999, NA, .x)
  )) %>%
  mutate(followup_time_itt = as.numeric(
    difftime(follow_up_12_month, consent_date, units = "days")
  )) %>%
  # See issue #2: min() returns one value for everyone
  mutate(followup_time_pp = min(
    as.numeric(difftime(follow_up_12_month, consent_date, units = "days")),
    as.numeric(difftime(glitch_date, consent_date, units = "days"))
  ))

## ---- merge-medication-summaries
# Participants with no rescue records have zero rescue use
rescue_medication_merged <- left_join(
  baseline_clean_data,
  total_rescue_medication_uses,
  by = c("study_id_screening" = "SUBJECTID")
) %>%
  mutate(rescue_puffs_itt = ifelse(
    is.na(rescue_puffs_itt), 0, rescue_puffs_itt
  )) %>%
  mutate(rescue_days_itt = ifelse(
    is.na(rescue_days_itt), 0, rescue_days_itt
  )) %>%
  mutate(rescue_puffs_pp = ifelse(
    is.na(rescue_puffs_pp), 0, rescue_puffs_pp
  )) %>%
  mutate(rescue_days_pp = ifelse(
    is.na(rescue_days_pp), 0, rescue_days_pp
  ))

controller_medication_merged <- left_join(
  baseline_clean_data,
  controller_pdc,
  by = c("study_id_screening" = "SUBJECTID")
)

# Interval controller PDC joined to the visit data
clean_asthma_data_itt <- clean_asthma_data_itt %>%
  left_join(controller_pdc_quarter) %>%
  select(-controller_pdc_quarter_pp)

clean_asthma_data_pp <- clean_asthma_data_pp %>%
  left_join(controller_pdc_quarter) %>%
  select(-controller_pdc_quarter_itt)

## ---- medication-by-interval
# Puff and day counts by visit interval and inhaler type. See issue #4.
medication_data_quarter <- medication_data_clean %>%
  mutate(quarter = case_when(
    (PuffDate >= consent_date) & (PuffDate < follow_up_2_months) ~
      "0-2 months",
    (PuffDate >= follow_up_2_months) & (PuffDate < follow_up_4_months) ~
      "2-4 months",
    (PuffDate >= follow_up_4_months) & (PuffDate < follow_up_6_months) ~
      "4-6 months",
    (PuffDate >= follow_up_6_months) & (PuffDate < follow_up_9_months) ~
      "6-9 months",
    (PuffDate >= follow_up_9_months) & (PuffDate < follow_up_12_month) ~
      "9-12 months"
  )) %>%
  mutate(quarter_number_days = case_when(
    (PuffDate >= consent_date) & (PuffDate < follow_up_2_months) ~
      difftime(follow_up_2_months, consent_date, units = "days"),
    (PuffDate >= follow_up_2_months) & (PuffDate < follow_up_4_months) ~
      difftime(follow_up_4_months, follow_up_2_months, units = "days"),
    (PuffDate >= follow_up_4_months) & (PuffDate < follow_up_6_months) ~
      difftime(follow_up_6_months, follow_up_4_months, units = "days"),
    (PuffDate >= follow_up_6_months) & (PuffDate < follow_up_9_months) ~
      difftime(follow_up_9_months, follow_up_6_months, units = "days"),
    (PuffDate >= follow_up_9_months) & (PuffDate < follow_up_12_month) ~
      difftime(follow_up_12_month, follow_up_9_months, units = "days"),
  ))

medication_quarter_counts <- medication_data_quarter %>%
  filter(!is.na(quarter)) %>%
  filter(!is.na(med_type_clean)) %>%
  group_by(SUBJECTID, quarter, med_type_clean) %>%
  mutate(number_puffs = n()) %>%
  mutate(number_days = n_distinct(PuffDate)) %>%
  ungroup() %>%
  select(
    SUBJECTID, quarter, quarter_number_days, med_type_clean,
    number_puffs, number_days
  ) %>%
  distinct() %>%
  complete(SUBJECTID, quarter, med_type_clean) %>%
  mutate(number_puffs = ifelse(is.na(number_puffs), 0, number_puffs)) %>%
  mutate(number_days = ifelse(is.na(number_days), 0, number_days))
