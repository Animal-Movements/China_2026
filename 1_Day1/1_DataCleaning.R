## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Clean Libraries, message=FALSE, warning=FALSE----------------------------------------------------
# Remove from memory
rm(list=ls())

# You may need to install these packages first
#install.packages('tidyverse', 'move2', 'lubridate', 'gt', 'sf', 'tmap')

# Load required libraries
library(tidyverse)
library(move2)
library(lubridate)
library(gt)
library(sf)
library(tmap)


## ----Clean Timezone, message=FALSE, warning=FALSE-----------------------------------------------------
# Set TimeZone
# Other timezones can be found at: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
Timezone1 <- 'UTC'
Timezone2 <- "Africa/Nairobi"


## ----Set UTM Zone, message=FALSE, warning=FALSE-------------------------------------------------------
# UTM Zone
LatLong.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # EPSG:4326
#UtmZone.proj <- "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #This is EPSG:32737"
UtmZone.proj <- "EPSG:32737"


## ----Load, message=FALSE, warning=FALSE---------------------------------------------------------------
# Load the movement file as a CSV
WB <- data.frame(read_csv("Data/WB_FullDataset.csv"))

# Load the reference dataset as a CSV
WB.ref <- data.frame(read_csv("Data/WB_FullDataset_ref.csv"))


# Alternatively, you could directly pull from Movebank after creating an account and accepting the user agreement.
# To pull data from Movebank directly

# Store credentials
#movebank_store_credentials("myUserName", "myPassword")

# Specify the study id of interest
#WB <- movebank_download_study(study_id = "White-bearded wildebeest (Connochaetes taurinus) movements - Kenya") 

# Note the format from pulling directly from Movebank is a bit different and requires some wrangling (it's a move2 object)
# WB <- WB %>% 
#   as_tibble() %>% # A tibble is a dataframe of dataframes 
#   mutate(longitude = st_coordinates(WB)[,1], # Pull out the X coordinates
#          latitude = st_coordinates(WB)[,2]) %>% # Pull out the y coordinates
#   select(-geometry) %>% 
#   data.frame() 

# Reference Dataset, import and convert to dataframe
# WB.ref <- data.frame(movebank_download_deployment(study_id = "White-bearded wildebeest (Connochaetes taurinus) movements - Kenya"))

# **********************
# **********************

# Look at the data
head(WB)
head(WB.ref)


## ----Verify, message=FALSE, warning=FALSE, results='hide', echo=FALSE---------------------------------
# View the dataset
head(WB)
tail(WB)
head(WB, 10)
WB[1:10,]

# What is the structure of the dataset?
# What is the data type of the timestamp column?
str(WB)
str(WB$timestamp)
str(WB.ref)

# How many rows and columns are there in the movement dataset?
dim(WB)
nrow(WB)
ncol(WB)

# How many tags?
sort(unique(WB$individual_local_identifier))
length(unique(WB$individual_local_identifier))

# How many study sites?  What are there names?
length(unique(WB.ref$study_site))
unique(WB.ref$study_site)

# What is the timezone of the dataset?
tz(WB$timestamp)


## ----Clean Merge, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------
# Clean the reference file, selecting only the columns that you want to include
WB <- # Note, this will overwrite your existing dataset
  WB %>% 
 
  # Pull in the WB.ref dataset, joining by a key field
  left_join(WB.ref, by = join_by("individual_local_identifier" == "individual_local_identifier")) %>%

  # Select columns of interest, rename if necessary
  mutate(id = tag_local_identifier,
         animal_id = individual_local_identifier,
         latitude,
         longitude,
         sex,
         DOP = gps_dop,
         fixType = gps_fix_type_raw,
         temp = external_temperature,
         # Convert timestamps from Timezone1 (UTC) to Timezone2 -> EAT
         timestamp = with_tz(timestamp, tz=Timezone2),
         # Set the timezones for the deploy on and deploy off fields
         deploy_on = with_tz(deploy_on_timestamp, tz=Timezone2),
         deploy_off = with_tz(deploy_off_timestamp, tz=Timezone2),
         study_site,
         .keep = "none") %>% 

  # The 'across' function allows me to apply a function (as.factor) across multiple fields 
  mutate(across(c(id,sex,study_site), as.factor)) %>%

  # Make sure no duplicate id and timestamp exist.
  distinct(animal_id, timestamp, .keep_all = TRUE) %>%

  # Remove any records that don't have a timestamp or a Lat/Long location
  filter(!is.na(timestamp),
         !is.na(latitude),
         !is.na(longitude),
         latitude != 0,
         longitude != 0,

         # Subset: Grab only the Athi-Kaputiei Data
         study_site == "Athi-Kaputiei Plains",

         # And use the deploy on and off dates to further subset
         timestamp >= deploy_on & timestamp <= deploy_off) %>%

  # Remove fields that are now unnecessary - > remove deployment fields
  dplyr::select(-c(deploy_on, deploy_off)) %>%
  
  # Remove extra levels (important since subsetting to a single study area)
  droplevels() %>%

  # Arrange the dataset by id and timestamp
  arrange(id, timestamp)

# Look again (yes again!) at your data
head(WB)


## ----Clean Verify, message=FALSE, warning=FALSE, results='hide', echo=FALSE---------------------------
# How many records
dim(WB)
nrow(WB)

# What is the structure of the dataset?
str(WB)

# How many tags?
sort(unique(WB$animal_id))
length(unique(WB$animal_id))

# What is the timezone of the dataset?
tz(WB$timestamp) 


## ----Summarize, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------------------
# Create summary object
wb.Summary <- WB %>% 
  
  summarize(
    Locations = n(),
    Sex = unique(sex),
    Start = min(timestamp),
    End = max(timestamp),
    Duration = round(End - Start, digits = 1),
    .by = animal_id) %>% 
  
  # Arrange results
  arrange(animal_id, Start, desc(Locations))

# Print Results
wb.Summary

# Now make prettier, saving the results to your Output folder 
gt_gnu <- wb.Summary %>% 
  
  # initialize gt table
  gt() %>%
  
  # Make the table easier to read with alternating grey bars
  opt_row_striping() %>%
  
  # Add title and subtitle, pulling date of creation
  tab_header(
    title = "White-bearded Wildebeest in Kenya: Tracking Data Summary",
    subtitle = Sys.Date()) %>%
  
  # Easy preset date formatting
  fmt_date(
    columns = c(Start, End),
    date_style = 8) %>%
  
  # Change the column labels for the table
  cols_label(animal_id = "Wildebeest ID",
             Sex = "Sex",
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


## ----Visualize, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------------------
# Create very simple plot (non-spatial)
plot(WB$longitude, WB$latitude,
     col = WB$id,
     pch = 16,
     cex = 0.5,
     ylab = 'Latitude',
     xlab = 'Longitude',
     asp = 1)


## ----Visualize1, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------
# Convert
WB.sf <- WB %>% 
  st_as_sf(coords = c('longitude', 'latitude'), 
           crs = LatLong.proj) %>% 
  st_transform(UtmZone.proj)

# You could check the coordinate system by:
#st_crs(WB.sf)

# Look at the data
#head(WB.sf)
#str(WB.sf)
class(WB.sf)


## ----Visualize2, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------
# Plot using basic R function
# plot(WB.sf["animal_id"],
#      main = paste("Wildebeest: Athi-Kaputiei Plains ( n = ", length(unique(WB.sf$animal_id)),")"))

# GGPlot using the spatial object
WB.sf %>%
  ggplot() +
  geom_sf(aes(fill = animal_id),
          alpha = 0.6,
          shape = 21,
          col = "black") +
  scale_fill_discrete(name = "Animal ID") +
  ggtitle(paste("Wildebeest: Athi-Kaputiei Plains (n =", length(unique(WB.sf$animal_id)),")")) +
  coord_sf(datum = st_crs(UtmZone.proj)) + # Note, this line is necessary unless we want the data plotted in Lat/Long
  theme_minimal()

# or use the facet_wrap command to separate each individual into its own plot
# This is perhaps a bit more useful to look for potential data errors.
# WB.sf %>%
#   ggplot() +
#   geom_sf(aes(fill = animal_id),
#           alpha = 0.6,
#           shape = 21,
#           col = "black") +
#   scale_fill_discrete(name = "Animal ID") +
#   theme_minimal() +
#   facet_wrap(~ animal_id)


## ----Visualize3, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------
# Read in shapefile boundary of Nairobi National Park
# Data are projected to DD WGS84 and must be reprojected to UTM 37S, WGS
NNP <- st_read("Data/NNP_DDWGS84.shp", quiet = TRUE) %>% 
  st_transform(crs = UtmZone.proj)

# Convert the wildebeest spatial points to spatial lines (more accurately, a “LINESTRING”). We’ll use a few functions that we have yet to see and will use the summarise function to create an individual line (do_union=FALSE) for each animal we monitored with a tracking device.
WB.lines <- WB.sf %>% 
  group_by(id) %>% 
  dplyr::summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

# Create color palette to use in graphing across individuals
col.pal <- viridis::viridis(length(unique(WB.sf$id)))

# Set tmap mode
tmap_mode("view")
 
# Select from a range of basemaps:.
# https://leaflet-extras.github.io/leaflet-providers/preview/
# You can also type "providers" in the console to see all the options.

# We'll pull a world satellite map and a world street map.
# We'll also plot the National Park boundary.
Athi.Map <-
  tm_basemap(c("Esri.WorldImagery",
             "OpenStreetMap")) +
  
  # Display WB - points
  tm_shape(WB.sf,
           name = paste("Wildebeest: Athi-Kaputiei Plains (n =", length(unique(WB.sf$animal_id)),")")) +
    tm_dots(col = 'id', 
            palette = col.pal, 
            size = 0.001, # Size of the dots
            legend.show = TRUE) +
  
  # Load the wildebeest data - Lines
  tm_shape(WB.lines,
           name = "Wildebeest Tracks") +
  tm_lines(lwd = 0.25, 
           col = 'id',
           palette = col.pal,
           legend.col.show = FALSE) +

  # Display NNP boundary
  tm_shape(NNP, name = 'NNP') +
    tm_fill(col = 'green', alpha = 0.5) +
    tm_borders(col = 'green')

# Create the map
Athi.Map

# Save Output
tmap_save(Athi.Map, filename = "Output/Athi_GnuMap.html")


## ----Save, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------------
# Save both files together
save(WB, WB.sf, file = "Data/wildebeest_data.rdata")

