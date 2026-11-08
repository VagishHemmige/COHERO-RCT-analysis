<!-- badges: start -->
[![Status](https://img.shields.io/badge/status-preliminary_analysis-yellow)](#)
[![Methods](https://img.shields.io/badge/methods-under_development-blue)](#)
[![Not for Citation](https://img.shields.io/badge/use-not_for_citation-lightgrey)](#)
<!-- badges: end -->

# COHERO RCT analysis

The goal of this analysis is to compare the COHERO mobile app with control in the EM-ASTHMA pilot randomized trial, on asthma control, quality of life, activity limitation, controller adherence, app-recorded inhaler use and 12-month health care use. It uses the trial's REDCap export and the data recorded by the COHERO app.

This analysis is currently in progress. The code is a port of an earlier R Markdown analysis into plain R scripts, and has not yet been run in this form.

The analysis website, which explains the methods and shows the code step by step, is at:
https://vagishhemmige.github.io/COHERO-RCT-analysis/

## Documentation

- [`analysis-plan.md`](analysis-plan.md): what the analysis does (a retrospective record, written after the results were seen)
- [`AGENTS.md`](AGENTS.md): how to work in this repository
- [`REPOSITORY-BLUEPRINT.md`](REPOSITORY-BLUEPRINT.md): the standard repository layout
- [Issues](https://github.com/VagishHemmige/COHERO-RCT-analysis/issues): known defects, open decisions and planned changes

`ARCHITECTURE.md` is not written yet.

## Running the analysis

Requires authorized access to the trial data, which are not in this repository. Run `R/main.R` from the project root. Needs the packages attached in `R/setup.R`, plus broom.helpers, which gtsummary's model tables require.

<!-- Results: add only when the figures and tables are cleared for public release. -->
