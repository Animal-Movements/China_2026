# Animal Movement Analyses for Ecologists

## Workshop Syllabus — 2026

**Dates:** 27 July – 2 August 2026\
**Location:** School of Ecology, Sun Yat-sen University, Shenzhen, PRC\
**Language of instruction:** English\
**Class size:** 25–30 participants

------------------------------------------------------------------------

## Instructors

| Name | Institution |
|----|----|
| Dr. Jared Stabach | Smithsonian's National Zoo & Conservation Biology Institute |
| Dr. Christen H. Fleming | University of Central Florida |
| Erika Lin | University of Central Florida |
| Dr. Wenjing Xu | University of Massachusetts – Amherst |

## Sponsors

Dr. Wang Fang, Fudan University · Dr. Li Sheng, Peking University · Dr. Zhang Lu, Sun Yat-sen University · Dr. William McShea, Smithsonian's National Zoo & Conservation Biology Institute

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
-   Step-by-step instructions for uploading your data and configuring permissions are provided in the pre-course materials.

### 2. R and RStudio

-   Install the latest versions of [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/)
-   Install the following R packages before arriving (installation instructions are in the pre-course materials): `move2`, `amt`, `tidyverse`, `sf`, `terra`, `momentuHMM`, `BaBA`, `recurse`, `ctmm`, `lubridate`, `mapview`, `tmap`

### 3. Pre-course modules (complete before arriving)

-   **M1: Introduction to R** — data types, functions, tidyverse, and ggplot2. Work through this carefully if you are relatively new to R; the workshop coding sessions will move at a moderate pace.
-   **M2: Introduction to Data Management in R** — data wrangling, joins, and tidy data principles
-   **M3: Movement package navigation** — data object types & conversions across `move2`, `amt`, and `adehabitatLT`
-   Assigned readings (see course GitHub repo)
-   **Movebank data upload guide** — step-by-step instructions for uploading your data and setting permissions (see Item 1 above)
-   **Foundational readings in movement ecology** - familiarize yourself with foundamental concepts in movement ecology.

### 4. Course materials

All workshop materials are hosted on GitHub: [**https://github.com/Animal-Movements/China_2026**](https://github.com/Animal-Movements/China_2026)

Each day's materials will also be shared as a zip file to the WeChat group before the day begins. However, we recommend checking GitHub for the most up-to-date versions, as materials may be revised during the course.

------------------------------------------------------------------------

## Schedule

### Day 0 — Pre-course + Orientation

| When | Activity |
|----|----|
| Before arrival | M1: Introduction to R · M2: Introduction to Data Management · M3: Movement package navigation — data object types & conversions (`move2`, `amt`, `adehabitatLT`) · assigned readings |
| Jul 27, AM | Instructor presentations (20 + 5 min each) |
| Jul 27, PM | Student flash intros (5 min/person: project, data, goals) + study group formation |

### Days 1–6

| Day | Date | Theme | Time | Module |
|----|----|----|----|----|
| Day 1 | Jul 28 | Introduction to Movement Data | 9:00–10:00 | Mini lecture: why animal movement & course overview (15 min) + key concepts (30 min) |
| Day 1 | Jul 28 | | 10:15–11:00 | M1: Data import & Movebank (`move2`) |
| Day 1 | Jul 28 | | 11:15–12:00 | M2: Data cleaning & QC (`move2`) |
| Day 1 | Jul 28 | | 1:30–2:15 PM | M3: Trajectory creation & visualization (`amt`) |
| Day 2 | Jul 29 | Movement Metrics | 9:00–9:30 | Q&A + mini lecture: movement metrics & why they're useful (MoveTraits) |
| Day 2 | Jul 29 | | 9:30–10:00 | M1: Discrete step metrics — step length, turning angle, speed (`amt`) |
| Day 2 | Jul 29 | | 10:15–11:00 | M2: Summary movement traits — displacement, max displacement, intensity of use, diurnality (`amt`; MoveTraits) |
| Day 2 | Jul 29 | | 11:15–12:30 | **[CTMM]** M3: Introduction to CTMM — autocorrelation models & variogram (`ctmm`) |
| Day 2 | Jul 29 | | 1:30–2:15 PM | **[CTMM]** M4: ctmm-based metrics — speed & distance estimation (`ctmm`) |
| Day 3 | Jul 30 | Home Range Estimation | AM | **Field trip** |
| Day 3 | Jul 30 | | 1:00–1:30 | Q&A + mini lecture: home range |
| Day 3 | Jul 30 | | 1:30–2:00 | M1: Discrete home range — MCP & KDE & home range comparison (`amt`) |
| Day 3 | Jul 30 | | 2:15–3:00 | **[CTMM]** M2: Occurrence vs. range distributions & effective sample sizes (`ctmm`) |
| Day 3 | Jul 30 | | 3:15–4:15 | **[CTMM]** M3: AKDE & population-range meta-analysis (`ctmm`) |
| Day 4 | Jul 31 | Behaviour Analysis | 9:00–9:30 | Q&A + mini lecture: behavior |
| Day 4 | Jul 31 | | 9:30–10:00 | M1: Barrier behaviour analysis (BaBA) |
| Day 4 | Jul 31 | | 10:15–11:00 | M2: Migration vs. range residence vs. disperser (migration pattern classification) |
| Day 4 | Jul 31 | | 11:15–12:30 | **[CTMM]** M3: Periodic movement patterns (`ctmm`) |
| Day 4 | Jul 31 | | 1:30–2:15 PM | **[CTMM]** M4: Trajectory/path simulation & encounter/interaction analysis (`ctmm`) |
| Day 5 | Aug 1 | Remote Sensing + HMM | 9:00–9:30 | Lecture: intro to remote sensing |
| Day 5 | Aug 1 | | 9:30–10:00 | M1: Environmental annotation — raster extraction & GEE intro (`terra`, `amt`) |
| Day 5 | Aug 1 | | 10:15–11:00 | M2: Hidden Markov Models with GPS data (`momentuHMM`) |
| Day 5 | Aug 1 | | 11:15–12:30 | M3: Hidden Markov Models with accelerometer data (`momentuHMM`) |
| Day 5 | Aug 1 | | PM | Mini lecture on location error (tentative) + **[CTMM]** M4: Accounting for Location Error |
| Day 6 | Aug 2 | Habitat Selection | 9:00–9:45 | Q&A + mini lecture: habitat selection |
| Day 6 | Aug 2 | | 10:00–11:00 | M1: Resource & step selection functions (`amt`; pre-annotated dataset) |
| Day 6 | Aug 2 | | 11:15–12:30 | **[CTMM]** M2: CTMM-based habitat selection — iRSFs & RSF+AKDE (`ctmm`) |

Time window assignments are tentative and will remain flexible based on everyday's progress. Time not otherwise committed each day is reserved for Own Data Practice — applying that day's methods to your own tracking dataset, with instructors available to help. Day 3 is the exception: the field trip and back-to-back afternoon modules leave no dedicated practice block that day.
