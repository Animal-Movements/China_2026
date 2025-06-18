# ******************************************************************
# ******************************************************************

# Project: Naboisho Wildebeest Movement Analyses
# Description: Script to read movement data from Movebank
# Author: Jared Stabach
# Date: 09 September 2024

# ******************************************************************
# ******************************************************************

# Remove anything in memory
rm(list=ls())

# Load Packages
library(tidyverse)
library(lubridate)
library(tmap)
library(move2)
library(svDialogs)

# Settings ---------------------------------------------------------


# Set TimeZone and UTM Zone
Timezone1 <- 'UTC'
Timezone2 <- "Africa/Nairobi"

# UTM Zone
LatLong.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # EPSG:4326
UTMZone.proj <- "+proj=utm +south +zone=36 +units=m +datum=WGS84 +init=EPSG:32736"

# Set Movebank Login Details 
UN <- dlgInput("Enter Movebank UserName: ", Sys.info()[""])$res
PW <- dlgInput("Enter Movebank Password: ", Sys.info()[""])$res

# Details
login <- movebankLogin(username=UN, password=PW)

# Pull Data from Movebank and convert to a dataframe
WB <- as.data.frame(getMovebankData(study = "White-bearded wildebeest (Connochaetes taurinus) movements - Kenya", login = login))

# Reference Data - No need to import here, as data are already subset in Movebank
# WB.ref <- getMovebankReferenceTable(study = "White-bearded wildebeest (Connochaetes taurinus) movements - Kenya", login = login)

# Check 
# What tags are included in the study?  How many?
sort(unique(WB$tag_local_identifier))
length(unique(WB$tag_local_identifier))

# Look at Data
head(WB)


# Data cleaning ----------------------------------------------------

# WB.ref2 <- WB.ref %>% 
#   mutate(deploy = ymd_hms(deploy_on_timestamp),
#          timestart = with_tz(deploy, tz=Timezone2))

# Create new columns, remove duplicates, filter, and arrange
# No need to do a subset here with the reference data.  Data are already subset in Movebank, based on the start dates provided for each animal.  Otherwise, would need to import reference dataset and then filter by the date/time included for each tag.

WB <- WB %>%
  # Rename columns and create timestamp - UTC to local
  transmute(id = tag_local_identifier,
            animal_id = local_identifier,
            deploy_id = paste0(id,'-',animal_id),
            latitude = location_lat,
            longitude = location_long,
            sex = sex,
            species = taxon_canonical_name,
            DOP = gps_dop,
            fixType = substr(gps_fix_type_raw,1,2),
            timestamp = with_tz(timestamp, tz=Timezone2)) %>%
  
  # Make sure no duplicate id and timestamp exist.
  distinct(animal_id, timestamp, .keep_all = TRUE) %>% 
  
  # Remove any records that don't have a timestamp or a Lat/Long location
  filter(!is.na(timestamp),
         !is.na(latitude),
         !is.na(longitude),
         latitude != 0, 
         longitude != 0) %>% 
  
  # Arrange the dataset by id and timestamp
  arrange(id, timestamp)

# Look at header
head(WB)