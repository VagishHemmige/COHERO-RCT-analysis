# Purpose: animated views of ACT and controller use. Exploratory: not sourced
#   by main.R. Run by an authorized person only: several animations step
#   through participants one at a time and show each study ID, so their
#   output is participant-level and not for release.
# Needs: setup.R; 02_visits.R (clean_asthma_data_itt, clean_asthma_data_pp);
#   03_medication.R (control_medication).
# Produces: animation objects; animate() renders them when run.
# Ported from the initialgraphs and medication chunks of
#   MR_SJ_asthma_pilot_2023-9-15.Rmd.

## ---- animate-act-by-visit
# ACT by arm, stepping through visits
act_by_visit_animation_itt <- clean_asthma_data_itt %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = act_score, color = studyarm, group = study_id_screening
  )) +
  transition_states(time_factor) +
  theme_bw() +
  ggtitle("Time= {closest_state} months")

act_by_visit_animation_pp <- clean_asthma_data_pp %>%
  ggplot() +
  geom_quasirandom(mapping = aes(
    x = studyarm, y = act_score, color = studyarm, group = study_id_screening
  )) +
  transition_states(time_factor) +
  theme_bw() +
  ggtitle("Time= {closest_state} months")

## ---- animate-act-spaghetti
# ACT trajectories, stepping through participants
animated_spaghetti_itt <- clean_asthma_data_itt %>%
  ggplot() +
  geom_point(mapping = aes(
    x = time_linear, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  geom_line(mapping = aes(
    x = time_linear, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  theme_bw() +
  transition_states(study_id_screening) +
  ggtitle("StudyID= {closest_state}")

animate(animated_spaghetti_itt, nframes = 200)

animated_spaghetti_pp <- clean_asthma_data_pp %>%
  ggplot() +
  geom_point(mapping = aes(
    x = time_linear, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  geom_line(mapping = aes(
    x = time_linear, y = act_score, color = studyarm,
    group = study_id_screening
  )) +
  theme_bw() +
  transition_states(study_id_screening) +
  ggtitle("StudyID= {closest_state}")

animate(animated_spaghetti_pp, nframes = 200)

## ---- animate-controller-use
# Controller uses and puffs per day, stepping through participants
control_medication_use_animation <- control_medication %>%
  ggplot() +
  geom_point(
    mapping = aes(x = StudyPeriod, y = control_uses, group = SUBJECTID),
    alpha = 0.2
  ) +
  theme_bw() +
  transition_states(SUBJECTID) +
  ggtitle(
    "Control Medication Uses per Day",
    subtitle = "StudyID= {closest_state}"
  )

animate(control_medication_use_animation, nframes = 200, fps = 1)

control_medication_puff_animation <- control_medication %>%
  ggplot() +
  geom_point(
    mapping = aes(x = StudyPeriod, y = control_puffs, group = SUBJECTID),
    alpha = 0.2
  ) +
  theme_bw() +
  transition_states(SUBJECTID) +
  ggtitle(
    "Control Medication Puffs per Day",
    subtitle = "StudyID= {closest_state}"
  )

animate(control_medication_puff_animation, nframes = 200, fps = 1)
