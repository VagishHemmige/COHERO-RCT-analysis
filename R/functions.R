# Purpose: reusable helper functions. Each takes its inputs as arguments.
# Needs: setup.R (packages). Produces: functions only.
# Ported from the outcome_functions chunk of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd. create_me_plot() follows with
#   figures.R.

## ---- create-me-table
# Mixed model of an outcome on visit (factor) by arm, with a random intercept
# for participant, returned as a flextable of the tidied coefficients. With
# an interaction variable, the model is visit by arm by that variable.
create_me_table <- function(trial_data, outcome_of_interest,
                            interaction_variable = NULL) {
  # Primary analysis without interactions if interaction_variable is NULL
  if (is.null(interaction_variable)) {
    lmer(
      get(outcome_of_interest) ~ time_factor * studyarm +
        (1 | study_id_screening),
      data = trial_data
    ) %>%
      tidy(., conf.int = TRUE) %>%
      flextable() %>%
      set_caption(paste0(outcome_of_interest, " vs. time")) %>%
      return()
  } else {
    lmer_formula <- paste0(
      outcome_of_interest, "~time_factor*studyarm*", interaction_variable,
      "+(1|study_id_screening)"
    ) %>%
      as.formula()

    lmer(lmer_formula, data = trial_data) %>%
      tidy(., conf.int = TRUE) %>%
      flextable() %>%
      set_caption(paste0(
        outcome_of_interest, " vs. time * ", interaction_variable
      )) %>%
      return()
  }
}
