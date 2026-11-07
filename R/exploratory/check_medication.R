# Purpose: data checks on the app medication records. Exploratory: not sourced
#   by main.R. Run by an authorized person only; the printed tables come from
#   restricted data and are not cleared for release.
# Needs: setup.R; 01_import_data.R (raw_medications);
#   03_derive_medication_outcomes.R (medication_data_clean). Produces: printed
#   check tables only.
# Ported from the medication_data_processing chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd. A check on one named participant in the
#   original is left out, because it ties a study ID to a data fact.

## ---- check-missing-ids
# Records with no study ID, by app user
raw_medications %>%
  filter(is.na(SUBJECTID)) %>%
  select(SUBJECTID, UserFk) %>%
  group_by(UserFk) %>%
  count() %>%
  flextable()

## ---- check-medication-labels
# Are controller and rescue inhalers labelled correctly in the app export?
raw_medications %>%
  group_by(Medication, Med.Type) %>%
  filter(!is.na(SUBJECTID)) %>%
  count() %>%
  arrange(Med.Type) %>%
  flextable()

## ---- check-relabelled-medications
# After relabelling by medication name
medication_data_clean %>%
  group_by(Medication, med_type_clean) %>%
  count() %>%
  arrange(med_type_clean) %>%
  flextable()
