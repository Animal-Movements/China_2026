# Introduction to Animal Movements 🐋🐅🐝🐆🐦
Introductory lectures and code from a course on analyzing animal movement data taught by lecturers from the Smithsonian's National Zoo & Conservation Biology Institute, the University of Central Florida, and the University of Massachusetts Amherst. The course was hosted at Sun Yat-Sen University in China over 5 days in July 2026, with a focus on discrete- and continuous-time analyses.  All materials are publicly available.  We ask that you acknowledge this course if you found the resources valuable to your work.

![R](https://img.shields.io/badge/Language-R-blue)
![Context](https://img.shields.io/badge/Focus-Movement%20Ecology-orange)

## Overview

## Key Topics
1. **Data Import & Cleaning:** Step-by-step instructions on how to import animal movement data from Movebank or a standealone .csv file, with steps to clean your data and assess the qualify of the data prior to analysis. Create a trajectory from the imported points and create a dynamic animation.
2. **Movement Metrics:** Summarize the movements as a discrete set of steps, analyzing step length, turning angle, and speed. Learn to calculate other movement traits, such as net displacement, intensity of use, and diurnality that are commonly calculated by movement ecologists.
3. **Continuous Time Movement Modeling (CTMM, `ctmm`):** Learn how to visualize the autocorrelation structure of your data and account for autocorrelation in your analyses by fitting models to your data. Select among continuous-time movement models to calculate space utilization distributions and other metrics. Compare continuous-time-based metrics with discrete-time metrics.
4. **Home-Range Estimation:** Learn the history of home range estimation, calculating traditional methods such as Minimum Convex Polygons (MCP) & Kernel Density Estimators (KDE). Understand the assumptions and biases of home range estimation methods and occurrence distributions. Calculate an Autocorrelated Kernel Density Estimate (AKDE) for your species of interest and obtain population-level inferences through home-range meta-analysis and population range estimation.
5. **Movement Behaviors:** Barrier Behaviour Analysis (`BaBA`), Migration patterns (`NSD`), Site fidelity (`recurse`). Visualize potential periodic patterns in space use through the autocorrelation structure of your data. Estimate encounter rates and interactions among individuals, through home range overlap, proximity metrics, and pairwise distances.
6. **Movement Simulations:** Simulate paths and distributions, conditioned on the best-fit continuous-time movement model and your telemetry data. Predict the most-likely path.
7. **Earth Observation Integration:** Learn how to integrate earth observation data with animal point locations. 
8. **Behavioral Classification:** Fit Hidden Markov Models (HMMs) with GPS and accelerometer data to classify behaviors from tracked paths.
9. **Location Error:** Learn how location error in GPS positions can affect your movement analyses and account for this with calibration data.
10. **Habitat Selection:** Conduct habitat selection analyses through integrated resource selection functions (iRSFs) and step-selection functions (SSFs), and incorporate resource selection into AKDE home range estimates.

## Instructors
**Jared Stabach**, Research Ecologist & Head - Conservation Technology & Innovation, *Smithsonian's National Zoo & Conservation Biology Institute*

**Wenjing Xu**, Assistant Professor, Department of Environmental Conservation, *University of Massachusetts Amherst*

**Christen Fleming**, Assistant Professor & Head - Ecoinformatics Lab, Department of Biology, *University of Central Florida*

**Erika Lin**, Graduate Student, Department of Biology, *University of Central Florida*

## Repository Structure

Each lecture contains a `.html` file and corresponding `.Rmd` and `.R` files. A `.Rproj` is also provided in each directory to allow for easy mapping to each of the files. Various subdirectories are included in each lecture, with data necessary to execute the lesson provided within a `\Data` folder. 

## Citation

GPS tracking data used in this workshop is subset from a 3-year study on the movements of white-bearded wildebeest (*Connochaetes taurinus*) in southern Kenya. The citation for these data is:

* **Stabach JA, et al. (2022).** Increasing anthropogenic disturbance restricts wildebeest movement across East African grazing systems. *Frontiers in Ecology and Evolution*. [10.3389/fevo.2022.846171](https://doi.org/10.3389/fevo.2022.846171)
* **Stabach JA, et al. (2020).** Data from: Comparison of movement strategies of three populations of white-bearded wildebeest. *Movebank Data Repository*. [doi:10.5441/001/1.h0t27719](https://www.datarepository.movebank.org/handle/10255/move.1095)

Data internal to `ctmm` used in this workshop include the following species, with citations listed below:

African Buffalo (*Syncerus caffer*)

* **Getz WM, et al. (2007).** LoCoH: Nonparameteric kernel methods for constructing home ranges and utilization distributions. *PLoS ONE.* [doi:10.1371/journal.pone.0000207](https://doi.org/10.1371/journal.pone.0000207)
* **Cross PC, et al. (2016).** Data from: Nonparameteric kernel methods for constructing home ranges and utilization distributions. *Movebank Data Repository.* [doi:10.5441/001/1.j900f88t](https://doi.org/10.5441/001/1.j900f88t)

White-Nosed Coati (*Nasua narica*)

* **Powell RA, et al. (2017)** Stink or swim: techniques to meet the challenges for the study and conservation of small critters that hide, swim or climb and may otherwise make themselves unpleasant. In DW Macdonald, C Newman, and LA Harrington (Eds.); *Biology and Conservation of Musteloids.* [doi:10.1093/oso/9780198759805.003.0008](https://doi.org/10.1093/oso/9780198759805.003.0008) 
* **Kays R, and Hirsch BT. (2015).** Data from: Stink or swim: techniques to meet the challenges for the study and conservation of small critters that hide, swim or climb and may otherwise make themselves unpleasant. *Movebank Data Repository.* [doi:10.5441/001/1.41076dq1](https://doi.org/10.5441/001/1.41076dq1)

Lowland Tapir (*Tapirus terrestris*)

* **Medici EP. (2023).** Data from: Study "Lowland tapirs, Tapirus terrestris, in Southern Brazil". *Movebank Data Repository* [doi:10.5441/001/1.03ck4s52](https://www.doi.org/10.5441/001/1.03ck4s52)

Maned Wolf (*Chrysocyon brachyurus*)

* **Cunha de Paula R.** Data from: The Maned Wolf Conservation Program [unpublished]. Please contact Rogerio Cunha de Paula (rogercunha@gmail.com) if you want to publish with these data.

Mongolian Gazelle (*Procapra gutturosa*)

* **Fleming CH, et al. (2014).** Data from: From fine-scale foraging to home ranges: A semi-variance approach to identifying movement modes across spatiotemporal scales. *Dryad Digital Repository.* [doi:10.5061/dryad.45157](https://doi.org/10.5061/dryad.45157)

Wood Turtle (*Glyptemys insculpta*)

* **Akre T.** Data from: Working Land and Seascapes [unpublished]. Please contact Tom Akre (akret@si.edu) if you want to publish with these data.
