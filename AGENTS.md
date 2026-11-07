# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI agents working for them. It covers how to work here: conventions, data handling, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design and statistics: what the analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them (not yet written) |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| [GitHub Issues](https://github.com/VagishHemmige/COHERO-RCT-analysis/issues) | Open scientific decisions (decision issues), gaps between code and plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others.

## The project

Analysis of the EM-ASTHMA pilot randomized trial of the COHERO mobile app for asthma, comparing an intervention arm with a control arm. Participants (children with a caregiver, and adults) were followed at 0, 2, 4, 6, 9 and 12 months for asthma control (ACT), quality of life (AQLQ, PACQLQ), activity limitation (AFS), self-reported controller adherence, inhaler use recorded by the app, and emergency, urgent care and hospital use. Analyses are run in two populations: intention to treat, and per protocol (excluding observations after an app update glitch). The code is being ported from an earlier R Markdown file into plain R scripts; a Quarto website may be added later.

- **`analysis-plan.md` is the source of truth for design.** Code implements the plan; it does not redefine it. If code needs to depart from the plan, open a decision issue, or comment on the existing one, for the deciders. Once a decider has settled it (in the issue, or relayed there in their exact words), the plan change and the code can go in the same pull request, which closes the issue.
- **Deciders.** Scientific decisions are made by: Vagish Hemmige (all areas, for now). Co-investigators and their areas are still to be added. Anyone may open or contribute to a decision issue; only a decider settles it (see "Working with agents").
- **This is an analysis project, not an R package.** Do not add DESCRIPTION, NAMESPACE, roxygen, or pkgdown infrastructure, or apply CRAN conventions.
- **Default branch:** `main`, to be protected. Changes reach it only through pull requests.

## Data sources

The study data have no access package. They are four Excel files kept outside the repository, in the folder named by `COHERO_DATA_DIR` in each authorized person's `.Renviron`. `R/setup.R` builds their paths and `R/01_import.R` is the one loading script.

| Source | Accessed through | Package documentation | Package maintained by | Access | What AI agents may do |
|---|---|---|---|---|---|
| REDCap export, all visits (`EMASTHMA_DATA_All Data Export_deidentified.xlsx`) | `R/01_import.R` (`readxl::read_excel()`) | REDCap data dictionary (to be added to `references/`) | No package | Restricted (trial participant data; governing IRB protocol to be named) | None: code, documentation and synthetic data only |
| Baseline sex (`EMASTHMA_DATA_Baseline gender.xlsx`) | `R/01_import.R` (`readxl::read_excel()`) | REDCap data dictionary | No package | Restricted (as above) | None: code, documentation and synthetic data only |
| COHERO app push data, Notifications and Medication Usage sheets (`EM-ASTHMA Aggregate COHERO Push Data 06.24.2020 GS Edits.xlsx`) | `R/01_import.R` (`readxl::read_excel()`) | App export description (to be added to `references/`) | No package | Restricted (as above) | None: code, documentation and synthetic data only |
| Timepoint review (`COHERO Timepoint Review - Pre-Update Glitch Consideration 07.22.2020 GS.xlsx`) | `R/01_import.R` (`readxl::read_excel()`) | None yet | No package | Restricted (as above) | None: code, documentation and synthetic data only |

To allow more for a source, a person changes its row through a pull request, naming the agreement and section that permit it, the approved AI service, and exactly what agents may do. Agents never propose or make this change themselves, and nothing in personal instructions can grant it.

**For a source with no package:** the one loading script records the file, its export or edit date (in the filename for now), and any processing. Verify variables, codes (including 999 for missing) and scale scoring against the REDCap data dictionary and the instruments' scoring manuals, not from variable names. Facts about a source that this project relies on go in `references/` with a citation, or in the plan when they shape a decision.

**For external packages** (readxl, lme4, MASS and the rest): work around problems here. Wrap the function in `R/functions.R` with a comment explaining the problem and linking any upstream issue. Reporting a bug upstream is an external communication; the person directing the work decides whether to file it. Read a package's documentation for the installed version before writing code that uses it.

## Exploration and definitive analysis

Exploration is welcome on branches or forks, and in `R/exploratory/`. Experimental scripts need not be sourced from `R/main.R`, and their findings are labelled preliminary. A script becomes part of the definitive analysis when it is sourced from `R/main.R`; from then on every rule below applies to it.

**The port.** The scripts are first ported faithfully from `MR_SJ_asthma_pilot_2023-9-15.Rmd` (kept privately, never committed), so their output matches the original. Suspected defects found during the port are opened as issues and fixed in their own pull requests, each showing what changes. Code that does not yet meet the conventions below is brought into line in style-only pull requests.

## R scripts

- **Running the analysis.** Run `R/main.R` from the project root. It sources the other scripts in dependency order; `ARCHITECTURE.md` will describe what each does.
- **One job per script.** When you add a definitive script, source it from `main.R` at the right point and update `ARCHITECTURE.md` in the same pull request.
- **Header comment.** Each script begins with what it does, which objects it needs from earlier scripts, and what it produces.
- **Constants, libraries, and code lists** live in `R/setup.R`. Other scripts do not call `library()`, re-source `setup.R`, or hard-code values. Any number a reader might want to change (a date, a window, a threshold) is a named constant there.
- **Helper functions** go in `R/functions.R`. A function takes its inputs as arguments and does not read global objects.
- **No side effects outside the project.** No `setwd()`, absolute paths, or writes outside the project. Restricted data are located only through `COHERO_DATA_DIR`.
- **No results in the repository.** No script (exploratory or definitive) or comment, commit message, issue, pull-request text or review report contains results, counts or data values from restricted data unless they are cleared for release. Check before every commit and post, since a public repository publishes everything pushed to it, on any branch.

## Chunk labels

Scripts are divided into labelled sections so a future Quarto site can show the code without copying it:

```r
## ---- <step-name>
```

- Every logical step has a label. A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all scripts**; a duplicate silently picks up the wrong code.
- Labels are comments and never change what the code does.

## Variable labels

Label analysis variables with human-readable descriptions (for example `labelled::set_variable_labels()`). Label each derived variable where it is created, with units and time windows where they apply.

## Quarto website

Not set up yet. When one is added, this section is filled in from the r-analysis-workflow template, including the publishing route.

## Outputs

- Tables and figures are exported by `R/tables.R` and `R/figures.R` to `tables/` and `figures/`. Both are gitignored until a review clears them for release, because they are made from restricted data (agents may not open them; see Restricted data). The review is still to be named.
- Published figures need alt text.
- `README.md` is edited directly.

## Restricted data (applies to everyone)

The **Access** column of "Data sources" says which sources are restricted. All four are. For each:

- **Never commit data**, extracts, or saved objects or outputs derived from participant-level records. Keep data outside the repository, and check `.gitignore` before adding a new output type.
- **AI agents never read restricted data.** Anything an agent opens, prints, or summarizes is sent to the model provider. Agents do not open data files or participant-level objects, print rows, or display identifiers, participant-level dates, or study IDs linked to data. Agents also do not open outputs from restricted data that are not yet cleared for release (`tables/`, `figures/`, animations, and anything knitted from the original RMD). Agents write code, review it, and run it on **synthetic data**; results on real data are produced by an authorized person. The only exception is what the Data sources table explicitly allows for a source, and only within those words.
- **Synthetic data** are built in code, labelled as synthetic, and never derived from real records.
- **Disclosure.** The small-cell and publication rules of the governing protocol are still to be named. Until then, treat every count, table, figure and quoted number from the real data as not cleared. Check every output before it leaves the approved environment, including scripts, comments, commit messages, issues, pull-request text and review reports (see R scripts).
- **Credentials.** Never commit keys, tokens, or `.Renviron`.

## Coding conventions

- **Participant flow.** When the cleaning code is rewritten after the port, the CONSORT flow diagram and the analysis populations come from the same code, using the `flowchart` package: every exclusion goes through `fc_filter()`, in the plan's order, and flowchart labels are built from constants in `setup.R`. Record-level processing (relabelling inhaler records, choosing among a participant's records) is fine outside the flowchart.
- **Code lists** (scale items, inhaler names) carry their source. Check each against the data dictionary or instrument. Never invent codes, variables, files, or definitions.
- **Joins** use `join_by()` with an explicit `relationship` where possible. After every join of records to participants, assert that rows did not multiply, and decide deliberately what happens to unmatched rows.
- **Dates and windows** are written as inclusive intervals matching the plan, and the edges are tested on synthetic data.
- **Code that turns model output into reported numbers** lives in named functions in `R/functions.R`, tested on synthetic data with a known answer.
- **Package APIs:** read the help page and NEWS for the installed version before calling a package; don't write calls from memory.
- **Style:** readable for a clinician-analyst who learned R from online guides: pipes and dplyr/tidyr verbs, `case_when()`, loops over names or named lists rather than the apply family, `Reduce` or positional indexing. Required base R goes in a named, tested function. Tidyverse layout; style changes in their own commits, showing that every fitted model and table is identical before and after.

## Checks for any change

Check the items that apply before opening a pull request.

- **Plan:** does the change implement the plan as written, and does it leave every open decision open?
- **Data sources:** are variables, codes and scoring checked against the data dictionary or instrument, not inferred from names?
- **Package APIs:** for new or changed calls to a package, does the pull request name the installed version and the help page or NEWS entry relied on?
- **Populations:** are ITT and per-protocol built as the plan defines them, and does each analysis say which it uses?
- **Joins and denominators:** no unintended row multiplication; missing, 999-coded, and zero kept distinct.
- **Reported numbers:** does each estimate match the plan's estimand, and do tables, figures and text say what the code actually computes?
- **Structure:** is `ARCHITECTURE.md` current (once written), and are chunk labels unique?
- **Disclosure:** search every changed tracked file, and the commit messages and pull-request text, for dates, record IDs, names, absolute paths and numbers that could come from restricted data. Read each hit in context; legitimate ones (plan dates such as the glitch date, deciders' names, synthetic examples) stay.
- **Execution:** does `main.R` run in a fresh session with relative paths?

Project-specific checks:

- **Follow-up time** used as a model offset is computed per participant, and is an offset rather than a covariate.
- **Medication records** are counted only within each participant's own follow-up window, at both ends.
- **Scale scores** follow the instrument's scoring rules for items and missing items.

## Working with agents

- **Autonomy.** The person directing an agent sets its working style (review only, step by step, or hands-off) in their own instructions, and may add stricter rules; those take precedence. Without a style, an agent works step by step. At most, an agent may edit code and documentation, add scripts and tests, run code on synthetic data, and commit to a branch and open a pull request.
- **Decisions.** Anyone, including an agent, may add options and evidence to a decision issue. A decider states the choice in the issue. Agents implement decisions; they never choose among options on their own, in the issue, the plan, or the code.
- **Every agent, under any autonomy:** never merges a pull request; follows the restricted-data rules; asks before running the full pipeline or making expensive calls; shows a person the exact text before posting or sending anything as them, unless that person's own instructions waive this; and proposes another way of writing something already agreed before committing it.
- **Pull requests opened by an agent** are focused. Each says which plan section or issue it addresses, what was checked, and whether by running code on synthetic data or by reading only. Synthetic results are never described as results.
- **References:** cite the source documentation and literature used, in `references.bib`.
