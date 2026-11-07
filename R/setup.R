# Purpose: attach packages and define constants shared by every script.
# Needs: COHERO_DATA_DIR set in your .Renviron (the folder holding the four
#   Excel files; it lives outside this repository). Produces: attached
#   packages, file paths and named constants.
# Ported from MR_SJ_asthma_pilot_2023-9-15.Rmd (library chunk and constants
#   from the ImportData, CountAndBinaryOutcomes and mixed-model chunks).

## ---- load-packages
library(MASS)
library(tidyverse)
library(readr)
library(readxl)
library(ggbeeswarm)
library(plotly)
library(broom)
library(broom.mixed)
library(lme4)
library(lmerTest)
library(gganimate)
library(flextable)
library(gtsummary)
library(patchwork)

## ---- data-paths
# Restricted data are reached only through this folder, set outside the repo.
data_dir <- Sys.getenv("COHERO_DATA_DIR")
if (data_dir == "") {
  stop(
    "Set COHERO_DATA_DIR in your .Renviron to the folder holding the study data."
  )
}

redcap_export_file <- file.path(
  data_dir,
  "EMASTHMA_DATA_All Data Export_deidentified.xlsx"
)
baseline_sex_file <- file.path(
  data_dir,
  "EMASTHMA_DATA_Baseline gender.xlsx"
)
push_data_file <- file.path(
  data_dir,
  "EM-ASTHMA Aggregate COHERO Push Data 06.24.2020 GS Edits.xlsx"
)
timepoint_review_file <- file.path(
  data_dir,
  "COHERO Timepoint Review - Pre-Update Glitch Consideration 07.22.2020 GS.xlsx"
)

## ---- define-constants
# Observations after this date are excluded from the per-protocol analysis
# (app update glitch). See analysis-plan.md.
glitch_date <- as.Date("2018-5-16")

# Items averaged into each scale score
aqlq_items <- c(
  "short_of_breath_2weeks",
  "avoid_dust_2weeks",
  "frustrated_2weeks",
  "bothered_by_cough_2weeks",
  "fear_of_not_having_meds_2weeks",
  "chest_tightness_2weeks",
  "avoid_cigarette_smoke_2weeks",
  "difficulty_sleep_2weeks",
  "concerned_2weeks",
  "wheeze_in_chest_2weeks",
  "avoid_pollution_2weeks",
  "limited_strenuous_activity_2weeks",
  "limited_moderate_activity_2weeks",
  "limited_social_activity_2weeks",
  "limited_work_tasks_2weeks"
)

pacqlq_items <- c(
  "caregiver_helplessness_1week",
  "family_plans_change_1week",
  "caregiver_frustration_1week",
  "caregiver_work_interference_1week",
  "caregiver_upset_1week",
  "caregiver_sleepless_1week",
  "caregiver_family_relations_1week",
  "caregivier_nightime_awakening_1week", # spelled as in the REDCap export
  "caregiver_angry_1week",
  "caregiver_worry_child_daily_activities_1week",
  "caregiver_worry_medications_1week",
  "caregiver_worry_overprotective_1week",
  "caregiver_worry_child_normalcy_1week"
)

afs_items <- c(
  "limitation_structured_physical_activities",
  "limitation_sports",
  "limitation_strenuous_activities",
  "limitation_moderate_activities",
  "limitation_mild_activities"
)

# Variables that code missing as 999
missing_999_variables <- c(
  "act_score",
  "only_give_controller_medication_needed",
  "only_give_controller_medication_breathless",
  "give_controller_medication_regularly",
  "try_to_avoid_controller_medication",
  "forget_give_controller_medication",
  "alter_change_dosage_controller_medication",
  "stop_giving_controller_medication",
  "reserve_controller_medication_if_treatment_not_work",
  "give_controller_medication_child_does_breathless",
  "give_controller_medication_as_label"
)

# Baseline cut-offs for the binary subgroup variables. See analysis-plan.md.
adherence_cutoff <- 35 # self-reported adherence score >= 35 is "Adherent"
act_cutoff <- 12 # ACT score >= 12 is "Controlled"

# Inhaler names used to relabel controller vs rescue medication
controller_medication_names <- c(
  "Advair", "Breo", "Dulera", "Flovent", "QVAR", "Symbicort"
)
rescue_medication_names <- c("Combivent", "ProAir", "Ventolin", "Proventil")

# Upper bound of the study ID range used to build the day grid; see #15
n_subject_ids <- 81

# 12-month health care use outcomes
hospital_count_outcomes <- c(
  "ed_visit_number_times_12months",
  "hospitalization_number_times_12months",
  "urgent_care_number_times_12months"
)
hospital_binary_outcomes <- c(
  "ed_visit_12months",
  "hospitalization_asthma_12months",
  "urgent_care_12months"
)

# Count outcomes modelled with negative binomial regression
count_outcomes <- c(
  "ed_visit_number_times_12months",
  "hospitalization_number_times_12months",
  "urgent_care_number_times_12months",
  "rescue_puffs_itt",
  "rescue_days_itt"
)

# Repeated-measures outcomes and subgroup variables for the mixed models
repeated_outcomes <- c(
  "act_score",
  "aqlq_score",
  "pacqlq_score",
  "afs_score",
  "self_reported_adherence"
)
subgroup_variables <- c(
  "prefer_language",
  "child_adult",
  "participant_sex",
  "self_reported_adherence_binary",
  "act_binary"
)

# Subgroup variables for the count-model interactions, and the count outcomes
# fitted with the baseline asthma control subgroup (hospitalizations are left
# out, as in the original analysis)
count_subgroup_variables <- c(
  "prefer_language",
  "participant_sex",
  "child_adult",
  "self_reported_adherence_binary"
)
act_subgroup_count_outcomes <- c(
  "ed_visit_number_times_12months",
  "urgent_care_number_times_12months",
  "rescue_puffs_itt",
  "rescue_days_itt"
)

# Subgroup variables for the mixed-model interactions, and the repeated
# outcomes also fitted by child or adult (AQLQ and PACQLQ each apply to one
# group only)
mixed_subgroup_variables <- c(
  "prefer_language",
  "participant_sex",
  "self_reported_adherence_binary",
  "act_binary"
)
child_adult_mixed_outcomes <- c(
  "act_score",
  "afs_score",
  "self_reported_adherence"
)

# Population names used in plot titles
population_labels <- c(
  itt = "ITT analysis",
  pp = "per-protocol analysis"
)

## ---- output-folders
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
