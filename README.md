# Clinical Trial Data & Dashboard System

A self-contained mock clinical trial data generator and participant management dashboard, built for learning, prototyping, and demonstrating data review workflows. All data is entirely synthetic and does not represent any real participants, sites, or institutions.

---

## Project Overview

This project simulates a multi-site, randomized clinical study with 50 fictitious participants across 5 research sites. It covers the full data lifecycle of a clinical trial: screening and consent, participant enrollment, longitudinal visit tracking, laboratory results, adverse event logging, site monitoring, and data review status tracking.

The system is composed of two layers:

- **Python data generation scripts** — produce a set of linked CSV files that form the mock study dataset
- **Dashboards** — interactive interfaces for reviewing participant status, site activity, and data cleaning priority

---

## File Reference

### Python Scripts

| File | Description |
|---|---|
| `generate_site_data.py` | Adds site codes to `mock_study_data.csv` and generates the site visit schedule CSV. Run after all other data scripts. |

> The core data generation scripts (subjects, visits, labs, adverse events, consent/screening, and master report) were developed iteratively in Jupyter and are not included as standalone files. Re-run those notebook cells to regenerate the source CSVs.

---

### Generated CSV Files

All CSVs link to one another via `subject_id`. Run the scripts in the order listed below.

| Order | File | Description |
|---|---|---|
| 1 | `mock_study_data.csv` | Core subject demographics, baseline vitals, survey scores, and behavioral data for 50 participants |
| 2 | `mock_visit_data.csv` | Longitudinal visit records across 5 scheduled visits per participant (Baseline through Week 16), with completion status |
| 3 | `mock_lab_results.csv` | Per-visit lab panel results (CBC, Metabolic, Lipid) with reference ranges and High/Low/Normal flags |
| 4 | `mock_adverse_events.csv` | Adverse event log with severity, relatedness, outcome, and resolution date |
| 5 | `mock_consent_screening.csv` | Screening eligibility checks, exclusion criteria, consent details, and referral source |
| 6 | `mock_study_data_v2.csv` | Updated subject file with `site_code` and `site_name` columns added. **Use this file in the dashboards instead of v1.** |
| 7 | `mock_site_visit_schedule.csv` | Combined participant visit and site monitoring visit schedule across all 5 sites |
| 8 | `mock_master_report.csv` | Flat summary merging all data sources into one row per subject (40+ columns) |
| 9 | `mock_master_report.json` | Nested JSON version of the master report, one object per subject |
| 10 | `mock_review_status.csv` | Exported from the dashboard. Tracks data review status, notes, reviewer, and timestamp per subject. Re-upload to resume progress. |

---

### Dashboard Files

| File | Description |
|---|---|
| `app.R` | **Current Shiny app.** Full R Shiny implementation of the dashboard. Sidebar navigation, DT tables with row-click modals, site cards, schedule tab, review tracking, and CSV export. |

---

## Quickstart

### 1. Generate the mock data (Python)

Run each data generation script in order in Jupyter or a Python environment. Scripts use only the standard library (`random`, `csv`, `datetime`) — no additional packages required.

```
mock_study_data.csv         ← generate_subjects.py
mock_visit_data.csv         ← generate_visits.py
mock_lab_results.csv        ← generate_lab_results.py
mock_adverse_events.csv     ← generate_adverse_events.py
mock_consent_screening.csv  ← generate_consent_screening.py
mock_study_data_v2.csv  \
                         ←  generate_site_data.py  (run last)
mock_site_visit_schedule.csv /
```

### 2. Run the Shiny app

Install dependencies (one time):

```r
install.packages(c("shiny", "shinydashboard", "DT", "dplyr",
                   "readr", "lubridate", "shinyWidgets", "htmltools"))
```

Launch the app:

```r
shiny::runApp("app.R")
```

Then navigate to the **Upload Files** tab and load your CSVs. Use the sidebar to switch between Participants, Site Visit Schedule, and Export views.

---

## Study Design

| Parameter | Value |
|---|---|
| Study ID | MOCK-STUDY-001 |
| Total participants | 50 |
| Study conditions | Control, Treatment A, Treatment B |
| Number of visits | 5 (Baseline, Week 4, Week 8, Week 12, Week 16) |
| Number of sites | 5 |
| Lab panels | Complete Blood Count, Metabolic Panel, Lipid Panel |

### Sites

| Code | Institution | Location | PI |
|---|---|---|---|
| SITE-001 | Boston Medical Center | Boston, MA | Dr. Harmon |
| SITE-002 | UCLA Health Sciences | Los Angeles, CA | Dr. Okafor |
| SITE-003 | UT Southwestern Medical | Dallas, TX | Dr. Patel |
| SITE-004 | Johns Hopkins Research Ctr | Baltimore, MD | Dr. Müller |
| SITE-005 | University of Chicago Med | Chicago, IL | Dr. Rivera |

---

## Dashboard Features

### Priority Queue (all dashboard versions)

Participants are ranked into four groups displayed in priority order:

| Priority | Group | Description |
|---|---|---|
| P1 | Screen Failures | Did not meet eligibility criteria |
| P2 | Dropped Out | Enrolled but did not complete the study |
| P3 | Completed | Finished all 5 study visits |
| P4 | Active | Currently enrolled, sorted by expected completion date |

### Data Review Tracking (all dashboard versions)

Each participant has a review status dropdown (Not Started / In Progress / Complete) and a free-text notes field. Status is saved in-session and exportable as `mock_review_status.csv`. Re-uploading that file on the load screen resumes prior progress.

### Site Summary Cards (all dashboard versions)

Color-coded cards for each site display participant counts by status, PI and coordinator names, and the next upcoming scheduled visit. A site filter narrows the participant table to a single site.

### Site Visit Schedule Tab (all dashboard versions)

Displays both participant visits and sponsor/CRA monitoring visits, filterable by site, visit type, status, and date window (past, next 30/60/90 days). Monitoring visits include CRA name and notes fields.



---

## Notes

- All data is randomly generated using `random.seed(42)` (subjects) and `random.seed(99)` (sites) for reproducibility. Re-running scripts with the same seed will produce identical output.
- The `mock_review_status.csv` produced by the dashboard includes `site_code` and `study_condition` columns to support filtering and auditing outside the dashboard.
- Generated with assistance from Anthropic's Claude (Sonnet 4.6). 
