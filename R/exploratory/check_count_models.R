# Purpose: reproduce the hospitalization model with the baseline asthma
#   control interaction, which the original analysis did not report.
#   Exploratory: not sourced by main.R.
#   Run by an authorized person only; output comes from restricted data.
# Needs: setup.R; 03_derive_medication_outcomes.R (rescue_medication_merged).
# Produces: printed model output only.
# Ported from the CountAndBinaryOutcomes chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd.

## ---- check-hospitalization-act-model
# Hospitalizations with the arm-by-ACT interaction, as attempted in the
# original analysis.
glm.nb(
  hospitalization_number_times_12months ~ studyarm * act_binary +
    offset(log(followup_time_itt)),
  data = rescue_medication_merged
) %>%
  tidy(exponentiate = TRUE)
