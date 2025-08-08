# this script demonstrate how to use package rgee to annotate your spatial data
# setting up rgee can be a bit tricky for different computers with different python and R versions
# we focus on how to use rgee for data annotation after installation 

# yet, you can find many resources online in terms of how to install rgee.

# --------- load packages and set up ------------------
library(rgee)
library(sf)
library(tidyverse)
library(mapview)

# initialize RGEE
ee_Initialize()

# --------- read data ------------------
sample_sites <- read_csv("./Archive_HMM_and_Annotation/Data/Sampling_site.csv") %>% 
  st_as_sf(., coords = c("x", "y"), crs = st_crs(32736))

study_site_box <- read_sf("./Archive_HMM_and_Annotation/Data/Study_area_bbox_3kmbuffer.gpkg")

# always check CRS
st_crs(sample_sites)
st_crs(study_site_box)

# visualize the sampling points and area
mapView(study_site_box, color = "grey", layer.name = "Fencelines") +
  mapView(sample_sites, zcol = "Transect",layer.name = "Sampling points", cex = 4) 


### here we etract rain pattern for the study area for the three years when we collected data
### we then plot it against the 20 year rain history
### -------------------- extract precipitation data from the Terraclimate dataset -----------
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%  # identify the dataset from earth engine data catalog 
  ee$ImageCollection$filterDate("2018-01-01", "2021-01-01") %>% # filter to target sates
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands 
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:36)) # rename the bands of an image for identification

ee_mara_rain <- ee_extract(x = terraclimate, y = study_site_box["Id"], fun = ee$Reducer$mean(), sf = FALSE) %>% #extract to the whole area and take the mean value
  pivot_longer(-Id, names_to = "month", values_to = "pr") %>% # organize the dataframe 
  mutate(month = as.numeric(gsub("PP_", "", month)), 
         year = case_when(month >=1 & month <= 12 ~ "2018",
                          month >12 & month <= 24 ~ "2019",
                          month >24 & month <= 36 ~ "2020"),
         month = rep(1:12, 3))

## ---- also extract all rain info from the past 20 years getting estimate for later model prediction ------
terraclimate_20yr <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2021-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:240)) # rename the bands of an image

ee_mara_rain_20yr <- ee_extract(x = terraclimate_20yr, y = study_site_box["Id"], fun = ee$Reducer$mean(), sf = FALSE) %>%
  pivot_longer(-Id, names_to = "month_id", values_to = "pr") %>%
  mutate(month_id = as.numeric(gsub("PP_", "", month_id)),
         date = lubridate::ymd("2001-01-01") + months(month_id - 1),
         month =lubridate:: month(date), year =lubridate:: year(date))

# now we can compare the three year rain pattern with the 20-year history
# the three years will be hilighted in blue, and the historical background in gray 
ggplot() +
  geom_line(data = ee_mara_rain_20yr, aes(x = factor(month), y = pr, group = year),
              size = 1, alpha = 0.5, color = "#8497b5") +
  geom_line(data = ee_mara_rain, aes(x = factor(month), y = pr, group = year),
            size = 1, alpha = 0.8, color = "#0063ff") +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal() +
  theme(legend.position = "none")


### here we extract NDVI for each sampling site over the study period
### and make timeseries plot to compare NDVI across all sites
### -------------------- extract NDVI data from MODIS -----------
NDVI.ic <- ee$ImageCollection('MODIS/006/MOD13Q1') %>%  #Vegetation Indices 16-Day L3 Global 250m
  ee$ImageCollection$filterDate("2018-01-01", "2021-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("NDVI")) # Select only NDVI bands

# extract image dates from the image collection to later combine with NDVI 
date.df <- data.frame( date = ee_get_date_ic(NDVI.ic)$time_start , serial = seq(1:69))

# image collection to image - prep for extraction 
NDVI <- NDVI.ic %>% 
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("NDVI_%02d",1:69)) # rename the bands of an image

# extract NDVI values for each site 
ee_mara_NDVI <- ee_extract(x = NDVI, y = sample_sites["Name"], sf = FALSE) %>%
  pivot_longer(-Name, names_to = "serial", values_to = "NDVI") %>%
  mutate(serial = as.numeric(gsub("NDVI_", "", serial)))

# join NDVI and date 
ee_mara_NDVI <- ee_mara_NDVI %>% left_join(date.df, by = "serial") %>%
  mutate (Transect = str_sub(Name, 1,1),
          Site = str_sub(Name, 2)) %>%
  select(-serial)

# now we can visualize the variations of NDVI across sites across time 
ee_mara_NDVI %>%
  ggplot(aes(x = date, y = NDVI, group = Name)) +
  geom_line(size = 1, alpha = 0.3, color = "#558c46") +
  xlab("Date") +
  ylab("NDVI") +
  theme_minimal() +
  theme(legend.position = "none")

### ----------------------- END ----------------------------
