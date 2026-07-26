## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----Load Libraries------------------------------------------------------------------------
# Clear R's memory
rm(list=ls())

# You may need to install these packages first
# install.packages(c("move2", "tidyverse", "lubridate", "sf", "mapview", "units", "gt"))

library(move2)       # Movebank interface and the move2 spatial data class
library(tidyverse)   # Data wrangling and visualization (dplyr, ggplot2, etc.)
library(lubridate)   # Working with timestamps and timezones
library(sf)          # Spatial vector data (move2 is built on sf)
library(mapview)     # Quick interactive maps
library(units)       # Provides support for measurement units
library(gt)          # Makes presentation-ready display tables


## ----Set UTM Zone--------------------------------------------------------------------------
# Set TimeZone
tz_utc <- "UTC"
tz_local <- "Africa/Nairobi"  # East Africa Time (EAT = UTC+3)

# Set Coordinate System
latlong_crs <- "EPSG:4326"   # WGS84 geographic — standard for GPS and Movebank


## ----Credentials, eval=FALSE---------------------------------------------------------------
## # Run this once — credentials are stored securely in your system keychain
## movebank_store_credentials("your_username", "your_password")


## ----Search Studies, eval=FALSE------------------------------------------------------------
## # Search for open-data studies that you can download data from
## movebank_download_study_info(
##   license_type = "CC_0",
##   i_have_download_access = TRUE,
##   attributes = c("name", "id")
## )


## ----Download Movebank, eval=FALSE---------------------------------------------------------
## WB.mv2 <- movebank_download_study(
##   study_id = 208413731 # Use the Movebank ID or the Study Name
##   #study_id = "White-bearded wildebeest (Connochaetes taurinus) movements - Kenya"
## )
## 
## # You may be prompted to accept the data license. If so, you can append the license number to your code and run again, like this:
## # WB.mv2 <- movebank_download_study(
## #   study_id = 208413731,
## #   #study_id = "White-bearded wildebeest (Connochaetes taurinus) movements - Kenya"
## #   'license-md5'='xxxxxx' # <- copy license number here
## # )


## ----Load RData, echo=TRUE, eval=TRUE------------------------------------------------------
# Load
load("./Data/wildebeest_data.rdata")


## ----csv_import, echo=TRUE, eval=FALSE-----------------------------------------------------
## # Import file, changing the date format (formatted here as ymd_hms()) to match your dataset
## WB.csv <- read_csv("Data/WB_FullDataset.csv",
##                    col_types = cols(timestamp = col_character())) %>%
##   mutate(timestamp = ymd_hms(timestamp, tz = tz_utc))
## 
## # Always verify the timestap before moving on
## # The result of this query should be 0:
## # sum(is.na(WB.csv$timestamp))
## 
## # Convert to move2 object
## WB.csv <- mt_as_move2(WB.csv, # Note that I am overwriting the original WB.csv file
##                       coords = c("longitude", "latitude"),  # column names for x, y
##                       crs = latlong_crs,
##                       time_column = "timestamp",
##                       track_id_column  = "individual_local_identifier"
##                       )
## 
## # Note, the reference information was not imported, although it is provided in the Data directory (Data/wildebeest_ref.csv).  If imported, the dataframe would need to be joined to the original by a common field.  This could be easily accomplished using the `left_join()` function on a shared field (e.g., ID).


## ----Mv2 Functions-------------------------------------------------------------------------
# Print the move2 object class
class(WB.mv2)   # move2, sf, tbl_df, tbl, data.frame

# Print a structured summary
WB.mv2

# Print the number of unique animals by accessing the track_id column. 
n_distinct(mt_track_id(WB.mv2)) # n_distinct is the same as length(unique(mt_track_id(WB.mv2))), but easier to write and understand

# Print the number of unique tracks
mt_n_tracks(WB.mv2)

# Print the name of the timestamp column
mt_time_column(WB.mv2)

# Print the name of the track ID column
mt_track_id_column(WB.mv2)

# Overall time range
range(mt_time(WB.mv2), na.rm = TRUE)


## ----track_data----------------------------------------------------------------------------
# mt_track_data() returns a data.frame containing the track attribute data
# glimpse() can then be used to provide a brief summary (a transposed version of print())
track_info <- mt_track_data(WB.mv2)
glimpse(track_info)

# Or just look at the column names from the event data
names(WB.mv2) # Same as colnames(WB.mv2)


## ----dimensions----------------------------------------------------------------------------
nrow(WB.mv2) # Total number of rows in the dataset
ncol(WB.mv2) # Total number of columns in the dataset
dim(WB.mv2) # Total number of rows and columns
glimpse(WB.mv2) # Get a quick view of each column


## ----individual_summary--------------------------------------------------------------------
wb.Summary <- WB.mv2 %>%
  as_tibble() %>%
  summarise(Locations = n(), 
            Start = min(timestamp, na.rm = TRUE), 
            End = max(timestamp, na.rm = TRUE), 
            Duration = round(as.numeric(difftime(max(timestamp, na.rm = TRUE),
                                                   min(timestamp, na.rm = TRUE),
                                                   units = "days")), digits = 1),
            .by = individual_local_identifier) %>%
  arrange(individual_local_identifier)

# Print Results
wb.Summary

# Now make a prettier table and output the file 
gt_gnu <- wb.Summary %>% 
  
  # initialize gt table
  gt() %>%
  
  # Make the table easier to read with alternating grey bars
  opt_row_striping() %>%
  
  # Add title and subtitle, pulling date of creation
  tab_header(
    title = "White-bearded Wildebeest in Kenya: Tracking Data Summary",
    subtitle = paste0("Created: ",Sys.Date())) %>%
  
  # Easy preset date formatting
  fmt_date(
    columns = c(Start, End),
    date_style = 8) %>%
  
  # Change the column labels for the table
  cols_label(individual_local_identifier = "Wildebeest ID",
             Locations = "Total points",
             Start = "First location",
             End = "Last location",
             Duration = "Tracking period (days)") %>%
  
  # Center text inside columns
  cols_align(align = "center") 

# Print result
gt_gnu

# Save as html table to send to the project manager, or a shiny app
gtsave(gt_gnu, filename = "Output/summary_gnu.html")


## ----sampling_interval---------------------------------------------------------------------
# mt_time_lags() returns the time difference to the NEXT location for each row in the dataset
time_lags <- WB.mv2 %>%
  arrange(individual_local_identifier, timestamp) %>%
  mutate(dt_hours = mt_time_lags(.) %>% # A function to retrieve the interval duration between locations, defaults to minutes
           set_units("hours") %>%  # convert to hours
           as.numeric()) %>% # Make the field numeric
  as_tibble()

# Notice the "dt_hours" column
head(time_lags)

# The last time lag of each individual track will always be NA, so the total NA should be equal to the number of individuals
sum(is.na(time_lags$dt_hours))  == n_distinct(time_lags$individual_local_identifier)

# Use filter to remove these NA values
time_lags <- time_lags %>%
  filter(!is.na(dt_hours))

# What is the nominal sampling interval? Different tags/studies/species can have very different fix schedules.
target_interval_h <- round(median(time_lags$dt_hours), 1)
cat("Empirical (median) sampling interval across all individuals:", target_interval_h, "hour\n")

# Summary of sampling intervals per individual
time_lags %>%
  summarise(
    median_interval_h = round(median(dt_hours), 2),
    min_interval_h = round(min(dt_hours), 2),
    max_interval_h = round(max(dt_hours), 2), 
    pct_irregular = round(mean(abs(dt_hours - target_interval_h) > 0.1) * 100, 1), # Calculate the percent irregular from the median value calculated
    .by = individual_local_identifier
  )

# Graph the interval distribution for one individual
time_lags %>%
  filter(individual_local_identifier == first(individual_local_identifier), # Could change this to a different individual or create a loop to print result for every individual
         dt_hours < 12) %>%
  ggplot(aes(x = dt_hours)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "white") +
  geom_vline(xintercept = target_interval_h, color = "firebrick", linetype = "dashed") +
  labs(title = "Distribution of fix intervals",
       subtitle = paste0("Red line = ", target_interval_h, "-hour target interval (empirical median)"),
       x = "Time between fixes (hours)", y = "Count") +
  theme_minimal()


## ----interval_by_hour----------------------------------------------------------------------
# Pull out the hour of the timestamp using the hour() function, converting the result to local time.
# Check to see if (~1h) vs. long (~3h) intervals separate by time of day.
time_lags <- time_lags %>%
  mutate(hour_local = hour(with_tz(timestamp, tz_local))) 

time_lags %>%
  filter(dt_hours < 6) %>%
  ggplot(aes(x = hour_local, y = dt_hours)) +
  geom_jitter(alpha = 0.1, width = 0.3, height = 0, color = "steelblue") +
  geom_hline(yintercept = c(1, 3), color = "firebrick", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Fix interval vs. local hour of day",
       subtitle = "Red lines mark the 1-hour and 3-hour nominal intervals",
       x = "Hour of day (Africa/Nairobi)", y = "Time to next fix (hours)") +
  theme_minimal()


## ----gap_diagnostics-----------------------------------------------------------------------
# Flag long gaps (> 1 day) and ask: Are these gaps spread randomly across individuals and dates? Or, do many individuals lose/regain signal together?
gap_threshold_h <- 24

long_gaps <- time_lags %>%
  filter(dt_hours > gap_threshold_h) %>% # Investigate all records where the time lag is > 24 hours
  mutate(gap_start = timestamp,                       # last fix before signal was lost
         gap_end   = timestamp + dhours(dt_hours)) %>% # first fix after signal returned
  select(individual_local_identifier, gap_start, gap_end, dt_hours)

cat(nrow(long_gaps), "gaps longer than", gap_threshold_h, "hours, across",
    n_distinct(long_gaps$individual_local_identifier), "of",
    n_distinct(time_lags$individual_local_identifier), "individuals\n")

# Bin gap *resolutions* (gap_end) by week: a spike in the number of distinct individuals resolving a long gap in the same week is the signature of a synchronized, fleet-wide event rather than independent tag failures.
synchronized_gaps <- long_gaps %>%
  mutate(resume_week = floor_date(gap_end, "week")) %>% # Floor_date rounds down to the nearest boundary of the time unit
  group_by(resume_week) %>%
  summarise(n_individuals_affected = n_distinct(individual_local_identifier),
            n_gaps = n(), .groups = "drop") %>%
  arrange(desc(n_individuals_affected))

# Display the first 6 rows
head(synchronized_gaps)


## ----outage_window-------------------------------------------------------------------------
# Treat the week with the most individuals affected as a candidate system-wide outage (require >= 5 individuals so isolated coincidences don't get flagged). Then recover the full span -- earliest signal loss to latest signal recovery -- so that the temporal coverage can be plotted.
outage_start <- as.POSIXct(NA)
outage_end <- as.POSIXct(NA)

if (nrow(synchronized_gaps) > 0 & synchronized_gaps$n_individuals_affected[1] >= 5) {
  outage_week <- synchronized_gaps$resume_week[1]
  outage_gaps <- long_gaps %>% filter(floor_date(gap_end, "week") == outage_week)

  outage_start <- min(outage_gaps$gap_start)
  outage_end   <- max(outage_gaps$gap_end)

  cat("Candidate synchronized outage:", as.character(outage_start), "to",
      as.character(outage_end), "--", n_distinct(outage_gaps$individual_local_identifier),
      "individuals affected\n")
}


## ----temporal_plot-------------------------------------------------------------------------
# Setup a blank graph with all indiviudals
p <- WB.mv2 %>%
  as_tibble() %>%
  mutate(date = as_date(timestamp)) %>%
  count(individual_local_identifier, date) %>%
  ggplot(aes(x = date, y = individual_local_identifier, fill = n))

# Create a rectangular to place on the graph if a synchronized outage window occurred
if (!is.na(outage_start)) { # Only include if a system-wide outage occurred (if outage_start is null, do nothing)
  p <- p +
    annotate("rect",
             xmin = as_date(outage_start), xmax = as_date(outage_end),
             ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.15)
}

# Graph the number of fixes collected over time for each animal
p +
  geom_tile() +
  scale_fill_viridis_c(name = "Fixes/day", option = "plasma") +
  labs(title = "Temporal coverage — Wildebeest GPS data",
       subtitle = if (!is.na(outage_start)) "Shaded band = candidate synchronized outage" else NULL,
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #tilt the labels on the axis


## ----quick_map-----------------------------------------------------------------------------
# Convert to sf object
WB.sf <- st_as_sf(as.data.frame(WB.mv2))

# Create graph, plotting a subset of the data for speed
WB.sf %>%
  slice_sample(n = 2000) %>%    # random subset of 2000 points
  mapview(zcol = "individual_local_identifier",
          layer.name = "Individual",
          cex = 2,
          alpha = 0.7)


## ----Save, eval=T--------------------------------------------------------------------------
save(WB.mv2, WB.sf, file = "Data/WB_raw.rdata")
cat("Saved WB.mv2 —", n_distinct(mt_track_id(WB.mv2)), "animals,", nrow(WB.mv2), "fixes\n")

