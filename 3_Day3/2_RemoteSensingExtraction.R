## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Clean Libraries, message=FALSE, warning=FALSE---------------------------------
# Remove from memory
rm(list=ls())

# You may need to install these packages first
#install.packages('terra', 'sf', 'mapview')

# Load required libraries
library(terra)
library(sf)
library(mapview)


## ----Data Import, message=FALSE, warning=FALSE, echo=TRUE--------------------------
# Data load/import
load("Data/wildebeest_data.rdata")

# Check the Coordinate Reference system.  Should be 32737
#WB.sf

# Set Projection Information
LatLong.proj <- "EPSG:4326"
UtmZone.proj <- "EPSG:32737"

# Transform the dataset to dd
WB.sf.dd <- WB.sf %>% 
  st_transform(LatLong.proj)

# Check project
WB.sf.dd

# View your data in leaflet.  We'll use mapview to do this.
mapview(WB.sf.dd)


## ----List Files, message=FALSE, warning=FALSE--------------------------------------
# List the files with the pattern hgt in the srtm folder
dir.srtm <- paste0(getwd(),"/Data/srtm/")
AllFiles <- list.files(path = dir.srtm, pattern = ".hgt", full.names = TRUE)

# Load all raster files as a list
raster_list <- lapply(AllFiles, rast)

# Images can be visualized using image
image(raster_list[[1]])

# Note the coordinate reference
raster_list[[1]]


## ----Mosaic, message=FALSE, warning=FALSE, echo=TRUE, eval = TRUE------------------
# Mosaic the rasters together.
raster_srtm <- do.call(merge, raster_list)
# Or if overlap between the rasters exists
#raster_srtm <- do.call(mosaic, c(raster_list, fun = mean))


# Check to determine if the raster image and data points overlap
image(raster_srtm)
points(WB.sf.dd)


## ----Extract, message=FALSE, warning=FALSE, echo=TRUE, eval = TRUE-----------------
# Extract raster value at points
Vals <- extract(raster_srtm, WB.sf.dd)

# Append the data back to the WB.sf.dd object. Remove the first column from the result, keeping everything else.
WB.sf.dd$srtm <- Vals[,-1] 
# Done!  Easy!


## ----Reproj Verify, message=FALSE, warning=FALSE, results='hide', echo=FALSE, eval = FALSE----
## # Create a temporary matrix to hold the results
## Temp.vals <- matrix(NA, nrow = nrow(WB.sf.dd), ncol = length(raster_list))
## 
## # Cycle through each raster and extract
## for(i in 1:length(raster_list)){
##   Vals <- extract(raster_list[[i]], WB.sf.dd)
##   # Put the values in the Temp.vals matrix, organized by column
##   Temp.vals[,i] <- Vals[,-1]
## }
## 
## # Remove na values by summing across rows
## srtm <- rowSums(Temp.vals, na.rm = TRUE)
## 
## # Combine back into the original dataframe
## WB.sf.dd$srtm2 <- srtm

