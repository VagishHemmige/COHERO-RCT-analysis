# Purpose: draw the example figures for the "How to read the trajectory
#   figure" page, from synthetic data only, so the page never shows study
#   data. The three-level group is invented for the example.
# Run from the project root: Rscript tests/synthetic/make_site_figures.R
# Produces: quarto/images/trajectory_example_effects.png and
#   quarto/images/trajectory_example_three_levels.png.

## ---- synthetic-data
Sys.setenv(COHERO_DATA_DIR = file.path(tempdir(), "cohero-synthetic-data"))
source("R/setup.R")
source("tests/synthetic/build_synthetic_data.R")
source("R/main.R")

## ---- example-figures
data <- mixed_model_data[["itt"]] %>%
  mutate(example_group = factor(
    study_id_screening %% 3,
    labels = c("Group A", "Group B", "Group C")
  ))
figure_labels[["example_group"]] <- "Example group"

overall <- compose_trajectory_figure(
  data, mixed_model_estimates[["itt"]][["act_score"]][["overall"]],
  "act_score", title = "Synthetic example: one outcome, two arms",
  show_trajectories = FALSE, show_baseline_p = FALSE,
  show_effect_panel = TRUE
)
save_figure(
  overall, "quarto/images/trajectory_example_effects",
  width = 170, height = 140, formats = "png", dpi = 200
)

three_levels <- compose_trajectory_figure(
  data,
  estimate_effects(
    fit_mixed_model(data, "act_score", modifier = "example_group"),
    modifier = "example_group"
  ),
  "act_score", modifier = "example_group",
  title = "Synthetic example: three subgroup levels"
)
save_figure(
  three_levels, "quarto/images/trajectory_example_three_levels",
  width = 190, height = 150, formats = "png", dpi = 200
)
