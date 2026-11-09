# Purpose: check that R/functions.R and R/plot_style.R work outside this
#   project. Run in its own R session by run_pipeline.R: only the tidyverse is
#   attached and only those two files are sourced, so any function that
#   reads one of this project's objects (setup.R, figure_labels, arm_labels)
#   or relies on a package being attached fails here. The invented data use
#   other column names, arm names and visit numbers than this project's.
#   Synthetic data only. Exits with status 1 if any check fails.

suppressPackageStartupMessages(library(tidyverse))
source("R/functions.R")
source("R/plot_style.R")


## ---- portable-data
# 60 people, two groups, visits at weeks 0, 1 and 3, and a two-level site.
# The drug group improves by 2 more points than placebo by week 3; true
# baseline means are equal.
withr::with_seed(1, {
  people <- tibble(
    person = sprintf("P%02d", 1:60),
    group = factor(rep(c("placebo", "drug"), each = 30),
                   levels = c("placebo", "drug")),
    site = factor(rep(c("North", "South"), times = 30)),
    person_effect = rnorm(60, 0, 2)
  )
  trial <- people %>%
    tidyr::crossing(week = c(0, 1, 3)) %>%
    mutate(
      visit = factor(week, levels = c(0, 1, 3)),
      score = 50 + person_effect + 0.5 * week +
        (group == "drug") * (2 / 3) * week + rnorm(n(), 0, 1)
    )
})
labels <- c(score = "Symptom score", visit = "Weeks since baseline",
            site = "Site")
group_names <- c(placebo = "Placebo", drug = "Drug")

## ---- portable-checks
testthat::test_that("the model and estimates work with other names", {
  fit <- fit_mixed_model(trial, "score", exposure = "group", time = "visit",
                         id = "person")
  testthat::expect_s4_class(fit, "lmerModLmerTest")
  estimates <- estimate_effects(fit, exposure = "group", time = "visit")
  week_3 <- estimates$within_levels %>% filter(visit == "3")
  # The known effect lies within the 95% CI
  testthat::expect_true(week_3$conf.low < 2 && 2 < week_3$conf.high)

  by_site <- estimate_effects(
    fit_mixed_model(trial, "score", modifier = "site", exposure = "group",
                    time = "visit", id = "person"),
    exposure = "group", time = "visit", modifier = "site"
  )
  testthat::expect_equal(nrow(by_site$within_levels), 6)
  testthat::expect_equal(unique(as.character(by_site$between_levels$comparison)),
                         "South - North")
})

testthat::test_that("the figure works with other names and labels", {
  estimates <- estimate_effects(
    fit_mixed_model(trial, "score", exposure = "group", time = "visit",
                    id = "person"),
    exposure = "group", time = "visit"
  )
  figure <- compose_trajectory_figure(
    trial, estimates, "score", show_effect_panel = TRUE,
    exposure = "group", time = "visit", id = "person",
    arm_labels = group_names, labels = labels
  )
  built <- lapply(seq_along(figure), function(i) ggplot2::ggplot_build(figure[[i]]))
  testthat::expect_equal(figure[[1]]$labels$y, "Symptom score")
  arm_text <- built[[1]]$data[[length(built[[1]]$data)]]$label
  testthat::expect_setequal(arm_text, c("Placebo", "Drug"))
  testthat::expect_equal(
    figure[[3]]$labels$title, "Drug minus placebo, change from baseline"
  )

  by_site <- compose_trajectory_figure(
    trial,
    estimate_effects(
      fit_mixed_model(trial, "score", modifier = "site", exposure = "group",
                      time = "visit", id = "person"),
      exposure = "group", time = "visit", modifier = "site"
    ),
    "score", modifier = "site", exposure = "group", time = "visit",
    id = "person", arm_labels = group_names, labels = labels
  )
  testthat::expect_equal(by_site[[1]]$labels$title, "Site: North")
  invisible(lapply(seq_along(by_site), function(i) ggplot2::ggplot_build(by_site[[i]])))
})

testthat::test_that("the tables work with other names", {
  testthat::skip_if_not_installed("gtsummary")
  fit <- fit_mixed_model(trial, "score", exposure = "group", time = "visit",
                         id = "person")
  terms <- tabulate_model_terms(fit, "Model terms", labels = labels)
  testthat::expect_s3_class(terms, "gtsummary")
  effects <- tabulate_effects(
    estimate_effects(fit, exposure = "group", time = "visit"),
    "Effects", time = "visit", time_label = "Week"
  )
  testthat::expect_equal(effects$table_body$label,
                         c("Week 0", "Week 1", "Week 3"))
})

testthat::test_that("unsupported data stop with a message", {
  three_groups <- trial %>%
    mutate(group = factor(rep(c("a", "b", "c"), length.out = n())))
  testthat::expect_error(
    estimate_effects(
      fit_mixed_model(three_groups, "score", exposure = "group",
                      time = "visit", id = "person"),
      exposure = "group", time = "visit"
    ),
    "compare exactly two"
  )
  unordered <- trial %>%
    mutate(visit = factor(week, levels = c(1, 0, 3)))
  testthat::expect_error(
    estimate_effects(
      fit_mixed_model(unordered, "score", exposure = "group", time = "visit",
                      id = "person"),
      exposure = "group", time = "visit"
    ),
    "increasing order"
  )
})
