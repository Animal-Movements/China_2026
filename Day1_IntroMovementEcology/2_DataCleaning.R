## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----packages------------------------------------------------------------------------------------------------
# Clear R's memory
rm(list=ls())

# install.packages(c("move2", "tidyverse", "lubridate", "sf", "gt", "mapview", "patchwork"))

library(move2)       # move2 spatial data class and helpers
library(tidyverse)   # Data wrangling and visualization
library(lubridate)   # Timestamps
library(sf)          # Spatial vector data
library(gt)          # Formatted summary tables
library(mapview)     # Quick interactive maps
library(patchwork)   # Plot composer


## ----load----------------------------------------------------------------------------------------------------
load("Data/WB_raw.rdata")   # loads WB.mv2, saved at the end of Module 1

# Text summary of import
cat("Starting point:", n_distinct(mt_track_id(WB.mv2)), "animals,", nrow(WB.mv2), "fixes\n")


## ----qc_log_init---------------------------------------------------------------------------------------------
qc_log <- tibble(
  step      = "0. Raw data (Module 1 output)",
  n_fixes   = nrow(WB.mv2),
  n_animals = n_distinct(mt_track_id(WB.mv2))
)


## ----track_meta----------------------------------------------------------------------------------------------
track_meta <- mt_track_data(WB.mv2)
glimpse(track_meta)


## ----study_sites---------------------------------------------------------------------------------------------
# How many study sites are mixed into this Movebank study?
track_meta %>%
  count(study_site, name = "n_animals") %>%
  arrange(desc(n_animals))


## ----map_by_site---------------------------------------------------------------------------------------------
WB.sf_all <- st_as_sf(as.data.frame(WB.mv2)) %>% # Here, we are making the data spatial, as we did in the previous exercise
  mutate(study_site = track_meta$study_site[match(individual_local_identifier,
                                                  track_meta$individual_local_identifier)]) # Can you guess what the function "match()" does?

WB.sf_all %>%
  slice_sample(n = 3000) %>%
  mapview(zcol = "study_site", layer.name = "Study site", cex = 2, alpha = 0.7)


## ----subset_site_deploy--------------------------------------------------------------------------------------
# Build per-track lookup vectors from the track metadata
site_lookup <- setNames(as.character(track_meta$study_site),
                         as.character(track_meta$individual_local_identifier))
on_lookup   <- setNames(track_meta$deploy_on_timestamp,
                         as.character(track_meta$individual_local_identifier))
off_lookup  <- setNames(track_meta$deploy_off_timestamp,
                         as.character(track_meta$individual_local_identifier))

# Filter records based on our lookup tables
WB.mv2 <- WB.mv2 %>%
  filter(
    site_lookup[as.character(individual_local_identifier)] == "Athi-Kaputiei Plains",
    timestamp >= on_lookup[as.character(individual_local_identifier)],
    timestamp <= off_lookup[as.character(individual_local_identifier)]
  )

cat("After filter:", n_distinct(mt_track_id(WB.mv2)), "animals,", nrow(WB.mv2), "fixes\n")
print(table(mt_track_data(WB.mv2)$study_site))   # expect: only Athi-Kaputiei Plains


## ----qc_log_1------------------------------------------------------------------------------------------------
qc_log <- qc_log %>%
  add_row(step      = "1. Subset to study site + deployment window",
           n_fixes   = nrow(WB.mv2),
           n_animals = n_distinct(mt_track_id(WB.mv2)))


## ----units_fix-----------------------------------------------------------------------------------------------
WB.mv2 <- WB.mv2 %>% 
  mutate(gps_dop = as.numeric(gps_dop))


## ----dop_explore---------------------------------------------------------------------------------------------
# Create a tibble with the count of each raw gps fix
WB.mv2 %>%
  as_tibble() %>%
  count(gps_fix_type_raw)


## ----dop_before----------------------------------------------------------------------------------------------
# Plot
plot.raw <- ggplot(WB.mv2, aes(x = gps_dop)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(data = tibble(gps_fix_type_raw = c("2D", "3D"), thresh = c(5, 10)),
             aes(xintercept = thresh), color = "firebrick", linetype = "dashed") +
  facet_wrap(~ gps_fix_type_raw, scales = "free_y") +
  labs(title = "GPS DOP by fix type — before filtering",
       subtitle = "Red line = common threshold (2D: DOP < 5, 3D: DOP < 10)",
       x = "DOP", y = "Count") +
  theme_minimal()

# Print to view the plot
plot.raw


## ----dop_filter----------------------------------------------------------------------------------------------
# Note: fixes with a missing DOP value, or a fix type other than "2D"/"3D",
# evaluate to NA here and are dropped by filter() along with the low-precision fixes.
WB.mv2 <- WB.mv2 %>%
  filter(
    (gps_fix_type_raw == "3D" & gps_dop < 10) |
    (gps_fix_type_raw == "2D" & gps_dop < 5)
  )

cat("After DOP/fix-type filter:", n_distinct(mt_track_id(WB.mv2)), "animals,", nrow(WB.mv2), "fixes\n")


## ----dop_after-----------------------------------------------------------------------------------------------
# First recorder so that graph plots in same was as plot.raw
WB.mv2$gps_fix_type_raw <- reorder(WB.mv2$gps_fix_type_raw, WB.mv2$gps_dop)

# Now plot the filtered subset
plot.filt <- WB.mv2 %>%
  as_tibble() %>%
  ggplot(aes(x = gps_dop)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  facet_wrap(~ gps_fix_type_raw, scales = "free_y") +
  labs(title = "GPS DOP by fix type — after filtering",
       x = "DOP", y = "Count") +
  theme_minimal()

# Plot results with plot.raw to see results together
# Use the patchwork package (| for side by side, / for stacked)
plot.raw / plot.filt


## ----qc_log_2------------------------------------------------------------------------------------------------
qc_log <- qc_log %>%
  add_row(step      = "2. GPS precision filter (DOP × fix type)",
           n_fixes   = nrow(WB.mv2),
           n_animals = n_distinct(mt_track_id(WB.mv2)))


## ----dedup---------------------------------------------------------------------------------------------------
# How many rows exist in the datast?
n_before_dedup <- nrow(WB.mv2)

WB.mv2 <- WB.mv2 %>%
  # Drop exact duplicate animal-timestamp combinations
  distinct(individual_local_identifier, timestamp, .keep_all = TRUE) %>%
  # Drop missing or (0,0) coordinates
  mutate(.lon = st_coordinates(.)[, 1],
         .lat = st_coordinates(.)[, 2]) %>%
  filter(!is.na(.lon), !is.na(.lat), !(.lon == 0 & .lat == 0)) %>%
  select(-.lon, -.lat) %>%
  # Re-establish track + time order (required by many move2/amt functions downstream)
  arrange(individual_local_identifier, timestamp)

cat("Removed", n_before_dedup - nrow(WB.mv2), "duplicate/invalid fixes\n")


## ----qc_log_3------------------------------------------------------------------------------------------------
qc_log <- qc_log %>%
  add_row(step      = "3. Remove duplicates + invalid coordinates",
           n_fixes   = nrow(WB.mv2),
           n_animals = n_distinct(mt_track_id(WB.mv2)))


## ----qc_summary----------------------------------------------------------------------------------------------
qc_log <- qc_log %>%
  mutate(pct_of_raw = round(100 * n_fixes / first(n_fixes), 1))

qc_log


## ----qc_gt---------------------------------------------------------------------------------------------------
qc_log %>%
  gt() %>%
  opt_row_striping() %>%
  tab_header(title = "QC pipeline: effect on dataset size",
             subtitle = "White-bearded wildebeest, Athi-Kaputiei Plains") %>%
  cols_label(step       = "Step",
             n_fixes    = "Fixes remaining",
             n_animals  = "Animals remaining",
             pct_of_raw = "% of raw fixes") %>%
  cols_align(align = "center", columns = c(n_fixes, n_animals, pct_of_raw))


## ----quick_map_clean-----------------------------------------------------------------------------------------
WB.sf_clean <- st_as_sf(as.data.frame(WB.mv2))

WB.sf_clean %>%
  slice_sample(n = 2000) %>%
  mapview(zcol = "individual_local_identifier", layer.name = "Individual", cex = 2, alpha = 0.7)


## ----save----------------------------------------------------------------------------------------------------
save(WB.mv2, file = "Data/WB_clean.rdata")
cat("Saved cleaned WB.mv2 —", n_distinct(mt_track_id(WB.mv2)), "animals,", nrow(WB.mv2), "fixes\n")

