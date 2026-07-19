---
output:
  html_document: default
  pdf_document: default
---
# Animal Movement Analyses for Ecologists

## Workshop Syllabus — 2026

**Dates:** 27 July – 2 August 2026\
**Location:** School of Ecology, Sun Yat-sen University, Shenzhen, PRC\
**Language of instruction:** English\
**Class size:** 32 participants

------------------------------------------------------------------------

## Instructors

| Name | Institution |
|----|----|
| Dr. Jared Stabach | Smithsonian's National Zoo & Conservation Biology Institute |
| Dr. Christen H. Fleming | University of Central Florida |
| Erika Lin | University of Central Florida |
| Dr. Wenjing Xu | University of Massachusetts – Amherst |

## Sponsors

Dr. Wang Fang, Fudan University · Dr. Zhang Lu, Sun Yat-sen University · Dr. William McShea, Smithsonian's National Zoo & Conservation Biology Institute

------------------------------------------------------------------------

## Overview

This intensive one-week workshop introduces graduate students to the analysis of animal movement data using R. You will learn to work with GPS tracking data from data import through advanced space-use and behavioral analysis, combining ecological theory with hands-on computation.

The course is organized around ecological questions rather than methods, so that analytical tools are introduced in the context of the biological problems they help answer. Each day pairs a lecture on ecological background with guided coding practice using a shared white-bearded wildebeest (*Connochaetes taurinus*) GPS tracking dataset from Kenya. Afternoons are reserved for you to apply the same methods to your own data, with instructors available to help.

The workshop covers two analytical frameworks in parallel: discrete-time methods (primarily using `amt`, `move2`, and related packages) and continuous-time movement modeling (using the `ctmm` package). 

------------------------------------------------------------------------

## Learning Goals

By the end of the workshop, you will be able to:

1.  Import, clean, and visualize animal tracking data in R using `move2` and `amt`
2.  Navigate the R movement package ecosystem and convert data between formats
3.  Compute and interpret standardized movement metrics (step length, displacement, intensity of use, diurnality)
4.  Estimate home ranges using discrete (MCP, KDE) and continuous-time (AKDE) methods
5.  Analyze specific behavioral patterns — barrier crossing, revisitation, and periodic movement
6.  Annotate tracking data with environmental covariates and apply Hidden Markov Models
7.  Conduct habitat selection analyses using resource and step selection functions (RSF/SSF)
8.  Apply at least one analytical workflow to your own tracking dataset

------------------------------------------------------------------------

## Who Should Attend

This workshop is designed for graduate students actively working with GPS or other telemetry tracking data. You should have:

-   Working knowledge of R (data manipulation, basic plotting)
-   Basic GIS background (understanding of coordinate systems, raster/vector data)
-   Your own tracking dataset to work on during afternoon practice sessions

Prior experience with movement analysis is not required.

------------------------------------------------------------------------

## Course Structure

**Pre-course (- 27 July)**  asynchronous pre-course modules completed before arrival with an in-person orientation afternoon — instructor presentations, flash intros, and study group formation.

**Days 1–6 (28 July – 2 August)** are the coding days. Each generally opens with a short Q&A and mini-lecture on that day's ecological context, followed by AM tutorial modules and, most afternoons, an additional module or Own Data Practice — applying the day's methods to your own tracking data, with instructors available to help troubleshoot and discuss your specific analytical questions. Exact timing varies by day — Day 3, for instance, opens with a field trip rather than morning modules — so check the Schedule below for the specifics of any given day.

You will be assigned to a study group of 4–5 people (mixed institutions) on Day 0. You are encouraged to work with your group during afternoon practice sessions and to support each other throughout the week.

------------------------------------------------------------------------

## Pre-Course Requirements

Please complete the following **before arriving**:

### 1. Movebank account and data upload

-   Create a free account at [movebank.org](https://www.movebank.org)
-   Upload your tracking dataset to Movebank. You can set fine-grained permissions on who can view your data — your data remains fully under your control.
-   Having your data on Movebank before the course is important: Some tutorials use the `move2` package to pull data directly from Movebank, and you will follow the same workflow with your own data in the afternoon.
-   If your data cannot be uploaded to Movebank for any reason, bring it as a CSV file with at minimum: individual ID, timestamp (with timezone), longitude, and latitude.
-   Step-by-step instructions for uploading your data and configuring permissions [can be found here](https://www.movebank.org/cms/movebank-content/add-data).

### 2. R and RStudio

-   Install the latest versions of [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/)
-   Install the following R packages before arriving (installation instructions are in the pre-course materials): `move2`, `amt`, `tidyverse`, `sf`, `terra`, `momentuHMM`, `BaBA`, `recurse`, `ctmm`, `lme4`, `lubridate`, `mapview`
-   _If you're already comfortable with R and expect to work with large-scale environmental datasets_, also install `rgee` (R interface to Google Earth Engine) and complete its authentication setup before arriving. Earth Engine access can be unreliable from mainland China without a VPN, so this is worth testing well ahead of the course rather than on the day.

### 3. Pre-course modules (complete before arriving)

-   **M1: Introduction to R** — data types, functions, tidyverse, and ggplot2. Work through this carefully if you are relatively new to R; the workshop coding sessions will move at a moderate pace.
-   **M2: Introduction to Data Management in R** — data wrangling, joins, and tidy data principles
-   **M3: Movement package navigation** — data object types, minimum data requirements, and conversions across the full package ecosystem used this week (`move2`, `sf`, `amt`, `ctmm`, `BaBA`, `recurse`, `momentuHMM`, `terra`, `rgee`), plus legacy tools like `adehabitatLT` you may encounter in older papers/scripts
-   **Recommended readings** -  familiarize yourself with fundamental concepts and methods in movement ecology.

------------------------------------------------------------------------

## Schedule (preliminary, subject to change)

### Day 0 — Pre-course + Orientation

| When | Activity |
|----|----|
| Before arrival | M1: Introduction to R · M2: Introduction to Data Management · M3: Movement package navigation — data object types, requirements, and conversions across the workshop's package ecosystem · assigned readings |
| Jul 27, AM | Instructor presentations (20 + 5 min each) |
| Jul 27, PM | Student flash intros (5 min/person: project, data, goals) + study group formation |

### Days 1–6

#### Day 1 — Introduction to Movement Data (Jul 28)

| Time | Module |
|----|----|
| 9:00–9:15 | Mini lecture: why animal movement & course overview (15 min) |
| 9:15–9:45 | Mini lecture: key concepts (30 min) |
| 10:15–11:00 | M1: Data import & Movebank (`move2`) |
| 11:15–12:00 | M2: Data cleaning & QC (`move2`) |
| 1:30–2:15 PM | M3: Trajectory creation & visualization (`amt`) |

#### Day 2 — Movement Metrics (Jul 29)

| Time | Module |
|----|----|
| 9:00–9:30 | Q&A + mini lecture: movement metrics & why they're useful (MoveTraits) |
| 9:30–10:00 | M1: Discrete step metrics — step length, turning angle, speed (`amt`) |
| 10:15–11:00 | M2: Summary movement traits — displacement, max displacement, diurnality (`amt`; MoveTraits) |
| 11:15–12:30 | M3: Introduction to CTMM — autocorrelation models & variogram (`ctmm`) |
| 1:30–2:15 PM | M4: ctmm-based metrics — speed & distance estimation (`ctmm`) |

#### Day 3 — Home Range Estimation (Jul 30)

| Time | Module |
|----|----|
| AM | **Field trip** |
| 1:00–1:30 | Q&A + mini lecture: home range |
| 1:30–2:00 | M1: Discrete home range — MCP & KDE & home range comparison (`amt`) |
| 2:15–3:00 | M2-a: Occurrence vs. range distributions & M2-b: Effective sample sizes (`ctmm`) |
| 3:15–4:15 | M3: AKDE, Home Range Meta-Analysis, Population Ranges (`ctmm`) |

#### Day 4 — Behaviour Analysis (Jul 31)

| Time | Module |
|----|----|
| 9:00–9:15 | Q&A |
| 9:15–9:50 | M1: Barrier behaviour analysis (`BaBA`) |
| 9:50–10:25 | M2: Migration pattern classification with NSD (`amt`) |
| 10:25–11:00 | M3: Site fidelity and revisitation (`recurse`) |
| 11:15–12:30 | M4-a: Periodic movement patterns (`ctmm`) & M4-b: Encounter/interaction analysis (`ctmm`) |
| 1:30–2:15 PM | M5: Path reconstruction & simulations (`ctmm`) |

#### Day 5 — Remote Sensing + HMM (Aug 1)

| Time | Module |
|----|----|
| 9:00–9:30 | Lecture: intro to remote sensing |
| 9:30–10:30 | M1: Environmental annotation — raster extraction & GEE intro (`terra`, `sf`, `rgee`) |
| 10:45–11:45 | M2: Hidden Markov Models with GPS data (`momentuHMM`) |
| 11:45–12:30 | M3: Hidden Markov Models with accelerometer data (`momentuHMM`) |
| PM | Mini lecture on location error + M4: Accounting for Location Error (`ctmm`) |

#### Day 6 — Habitat Selection (Aug 2)

| Time | Module |
|----|----|
| 9:00–9:45 | Q&A + mini lecture: habitat selection |
| 9:45–11:00 | M1: CTMM-based habitat selection — iRSFs & RSF+AKDE (`ctmm`) |
| 11:15–12:30 | Mini lecture on SSF + M2: Resource & step selection functions (`amt`; pre-annotated dataset) |

Time window assignments are tentative and will remain flexible based on everyday's progress. Time not otherwise committed each day is reserved for Own Data Practice — applying that day's methods to your own tracking dataset, with instructors available to help. Day 3 is the exception: the field trip and back-to-back afternoon modules leave no dedicated practice block that day.
