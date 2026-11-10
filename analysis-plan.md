# Analysis plan: EM-ASTHMA pilot randomized trial of the COHERO app

Status: draft. **Retrospective record of the analysis as performed**, written in November 2026 from the analysis code (`MR_SJ_asthma_pilot_2023-9-15.Rmd`, kept privately), after the results had been seen. It is not a prespecified statistical analysis plan, and no part of it was locked before outcome data were analyzed. Every analysis below is post hoc unless a trial protocol is later shown to have specified it.

## Scope and reporting framework

This document is the authoritative specification of the study's scientific objectives, population, definitions, estimands and statistical methods. `ARCHITECTURE.md` documents the implementation, `AGENTS.md` the working conventions, and GitHub Issues the open decisions and implementation status. Neither file supersedes this plan.

The plan describes what the analysis code does, and nothing else. Where the code is silent (for example the intervention itself, or a primary outcome), the point is marked **Open** for the deciders rather than filled in. Where the code contradicts itself, both versions are stated and the point is marked **Open**.

Reporting will follow the CONSORT 2010 extension to randomised pilot and feasibility trials [@eldridge2016consortpilot]. The reporting map in section 10 links each checklist item to a section of this plan.

Change record: git history. Each design change is committed on its own, with the reason, and notes that results had already been seen.

## 1. Objectives

The code compares the intervention arm with the control arm on asthma control, quality of life, activity limitation, controller adherence, app-recorded inhaler use and 12-month health care use, overall and within subgroups.

**Open:** the pilot trial's own objectives (checklist 2b), including any feasibility objectives such as recruitment, retention or app engagement, are not in the code. Decision issue to be opened.

**Open:** no outcome is designated primary. Decision issue to be opened.

## 2. Trial design

Two-arm randomized trial, intervention (`studyarm` = 1) vs control (`studyarm` = 0). Assessments at baseline (consent) and at 2, 4, 6, 9 and 12 months, recorded as REDCap events `baseline_arm_1` and `2month_followup_arm_1` through `12month_followup_arm_1`.

**Open:** allocation ratio, randomization method, allocation concealment and blinding (checklist 3a, 8–11) are not in the code; they come from the protocol. Decision issue to be opened.

## 3. Participants

Children (with a caregiver completing caregiver measures) and adults (`child_adult`), with English or Spanish as preferred language (`prefer_language`).

- Records in the app medication data with no study ID are not participants and are excluded from every analysis (decision recorded in the original code, attributed to the study team).
- A preferred-language value of 999 is set to English, as in the original code (see #16).

**Open:** eligibility criteria, setting, and how participants were identified and consented (checklist 4a–4c) are not in the code.

## 4. Intervention and comparator

**Open:** the COHERO app intervention and the control condition (checklist 5) are not described in the code.

## 5. Outcomes

All definitions below are as computed by the code. Scale items coded 999 are treated as missing.

### Repeated measures (each visit)

- **Asthma control:** ACT score (`act_score`).
- **Adult quality of life:** AQLQ score, the mean of 15 items with a two-week recall, averaged over the items answered.
- **Caregiver quality of life (children):** PACQLQ score, the mean of 13 items with a one-week recall, averaged over the items answered.
- **Activity limitation:** AFS score, the mean of 5 items, averaged over the items answered.
- **Self-reported controller adherence:** 32 − (needed) − (breathless) + (regularly) − (avoid) − (forget) − (alter dose) − (stop) − (reserve) − (child breathless) + (as label), using the ten controller-medication items.

**Open:** the source instrument and scoring rules for each scale, including whether averaging over answered items matches the instruments' missing-item rules, and the source of the adherence formula. Decision issue to be opened.

### App-recorded inhaler use (consent to the 12-month visit)

- **Inhaler type:** records labeled Control or Rescue keep their label. Other records are relabeled by medication name: Advair, Breo, Dulera, Flovent, QVAR and Symbicort as controller; Combivent, ProAir, Ventolin and Proventil as rescue.
- **Rescue puffs:** number of rescue records per participant. **Rescue days:** number of distinct days with a rescue record.
- **Controller proportion of days covered (PDC):** for each participant, the share of days from consent to the 12-month visit with at least one controller record. Also computed within each visit interval (consent to 2 months, 2–4, 4–6, 6–9, 9–12 months) and attached to the visit that closes the interval.
- Records dated after the participant's 12-month visit are excluded.

**Open:** records are excluded after the 12-month visit, but nothing excludes records before consent. Decision issue to be opened.

### 12-month health care use (reported at baseline record)

- Counts of emergency department visits, hospitalizations and urgent care visits in the 12 months (`ed_visit_number_times_12months`, `hospitalization_number_times_12months`, `urgent_care_number_times_12months`). A missing count is set to 0; a count of 999 is set to missing.

**Open:** treating a missing count as zero. Decision issue to be opened.

## 6. Analysis populations

- **Intention to treat (ITT):** all randomized participants and all their visits, by assigned arm.
- **Per protocol:** visits on or before 16 May 2018, the date of an app update glitch. Visits after that date are excluded. For inhaler use, records after that date are excluded.

The health care use and rescue-inhaler models (section 7) use ITT only.

**Open:** the per-protocol follow-up time is computed as the smallest value across all participants rather than for each participant, and it is not used in any model. Decision issue to be opened.

## 7. Statistical analysis

### Baseline characteristics

Table 1 by arm, at baseline: sex, child or adult, preferred language, age at screening, ACT, AQLQ (adults), PACQLQ (children), AFS and self-reported adherence. ITT population.

### Count outcomes

Negative binomial regression of each count on arm, with the log of follow-up time (consent to 12-month visit, in days) as an offset, reported as incidence rate ratios with 95% confidence intervals. Outcomes: ED visits, hospitalizations, urgent care visits, rescue puffs, rescue days. ITT.

**Open:** a second set of rescue-puff and rescue-day models enters follow-up time as `log(offset(followuptime_ITT))`, which makes it a covariate rather than an offset. Decision issue to be opened.

### Repeated measures

Linear mixed model for each of ACT, AQLQ, PACQLQ, AFS and self-reported adherence: outcome ~ visit (as a factor) × arm, with a random intercept for participant. Arm-by-visit terms estimate the difference between arms at each visit relative to the baseline difference. P values from lmerTest. Run in ITT and per-protocol populations.

Each model is reported in two tables (decided in #8): its terms, and the arm effect at each visit. The effects table reports the difference between arms at baseline and, at each follow-up visit, the difference between arms in change from baseline (intervention change minus control change), each with its 95% confidence interval and p value; with a subgroup, within each level and as the difference between each level and the first. These are the effects the trajectory figures show.

### Controller PDC

PDC for the year compared between arms descriptively, with gtsummary's default test, in ITT and per-protocol populations.

### Subgroups (effect modification)

Each count model and each mixed model is repeated with an arm × subgroup interaction (for mixed models, visit × arm × subgroup) for:

- preferred language;
- sex;
- child or adult (count models and the mixed models for ACT, AFS and adherence only, as AQLQ and PACQLQ each apply to one group);
- baseline self-reported adherence: adherent (score ≥ 35) vs non-adherent;
- baseline asthma control: controlled (ACT ≥ 12) vs uncontrolled. Not fitted for hospitalizations, as in the original analysis.

**Open:** the source of the adherence cut-off (35) and the ACT cut-off (12). Decision issue to be opened.

### Multiplicity and missing data

No adjustment for multiple comparisons. Mixed models use all available visits; other analyses use available cases.

### Software

R with MASS (negative binomial), lme4 and lmerTest (mixed models), emmeans (effects from the fitted models), broom and broom.mixed, gtsummary and flextable (tables), ggplot2, ggbeeswarm, patchwork and gganimate (figures). Package citations to be added from the analysis machine's `citation()` output.

## 8. Disclosure control

**Open:** the governing IRB protocol, its small-cell rule and any publication restrictions. Until named, no count, table, figure or number from the study data is treated as cleared for release.

## 9. Limitations to address in reporting

- The plan was written after the results were seen; all analyses are post hoc.
- The pilot extension treats feasibility as the aim of a pilot trial and does not apply subgroup or adjusted analyses (checklist 12b). The comparative and subgroup analyses here are exploratory, and their p values are not evidence of effectiveness.
- No adjustment for multiplicity across many outcomes, populations and subgroups.
- The per-protocol population is defined by calendar date relative to the app glitch, not by adherence to the intervention.

## 10. Reporting map

Items follow the CONSORT 2010 extension to randomised pilot and feasibility trials [@eldridge2016consortpilot], Table 2.

| Checklist item | Topic | Plan section |
|---|---|---|
| 1a, 1b | Title and abstract | Reporting |
| 2a | Rationale for future definitive trial, reasons for pilot | Open (1) |
| 2b | Pilot objectives | 1 (Open) |
| 3a | Design and allocation ratio | 2 (ratio Open) |
| 3b | Changes to methods after commencement | 6 (app glitch) |
| 4a–4c | Eligibility, setting, identification and consent | 3 (Open) |
| 5 | Interventions | 4 (Open) |
| 6a | Assessments for each objective | 5 |
| 6b | Changes to assessments | 6 |
| 6c | Progression criteria | Open (1) |
| 7a, 7b | Rationale for numbers; interim analyses | Open |
| 8a–10 | Randomization, concealment, implementation | Open (2) |
| 11a, 11b | Blinding | Open (2) |
| 12a | Methods for each objective | 7 |
| 12b | Additional analyses (not applicable to pilots) | 7 subgroups, 9 |
| 13a, 13b | Participant flow | 3, 6 |
| 14a, 14b | Recruitment dates; why the trial ended | Reporting |
| 15 | Baseline table | 7 |
| 16 | Numbers analyzed per objective | 6, 7 |
| 17a | Results with uncertainty | 7 |
| 18 | Other analyses informing the definitive trial | 7 |
| 19, 19a | Harms; unintended consequences | Open |
| 20–22a | Limitations, generalisability, interpretation, progression | 9 |
| 23–26 | Registration, protocol, funding, ethics | Open (8) |

## References
