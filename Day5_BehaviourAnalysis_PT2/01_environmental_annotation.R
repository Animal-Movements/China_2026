## ----setup, include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----packages--------------------------------------------------------------------------
# install.packages(c("terra", "sf", "tidyverse", "lubridate", "mapview"))
# rgee is only needed for Section 4 (Route 3) - see that section for install/auth notes

library(terra)      # Raster read, mosaic, terrain, extract
library(sf)         # Vector points/polygons
library(tidyverse)  # Data wrangling and plotting
library(lubridate)  # Date matching for NDVI composites
library(mapview)    # Quick interactive maps


## ----load_data-------------------------------------------------------------------------
load("Data/WB_clean.rdata")   # loads WB.mv2, the cleaned wildebeest track data

WB_sf <- st_as_sf(as.data.frame(WB.mv2))
st_crs(WB_sf)$epsg   # 4326 - Movebank's native geographic CRS


## ----sotua_subset----------------------------------------------------------------------
sotua_sf <- WB_sf %>% filter(individual_local_identifier == "Sotua")
nrow(sotua_sf)


## ----mcp-------------------------------------------------------------------------------
WB_mcp <- WB_sf %>% st_union() %>% st_convex_hull()

mapview(WB_mcp, alpha.regions = 0.1, layer.name = "Area of interest (MCP)") +
  mapview(sotua_sf, cex = 2, layer.name = "Sotua fixes")


## ----srtm_list-------------------------------------------------------------------------
srtm_files <- list.files("Data/srtm", pattern = "\\.hgt$", full.names = TRUE)
srtm_files


## ----srtm_mosaic-----------------------------------------------------------------------
srtm_tiles <- lapply(srtm_files, rast)

# merge() mosaics adjoining, non-overlapping tiles into one continuous raster.
# (If tiles overlapped, terra::mosaic() with a reducer function like mean would be the right tool instead.)
elev <- do.call(merge, srtm_tiles)
names(elev) <- "elevation_m"

plot(elev, main = "SRTM elevation (m) — Athi-Kaputiei Plains")


## ----srtm_extract----------------------------------------------------------------------
sotua_sf$elevation_m <- terra::extract(elev, sotua_sf, ID = FALSE)[, 1]
summary(sotua_sf$elevation_m)


## ----landcover_load--------------------------------------------------------------------
lc_2010 <- rast("Data/Landcover/MCD12Q1.061_LC_Type1_doy2010001_aid0001.tif")

landcover_levels <- c(
  "Evergreen needleleaf forests", "Evergreen broadleaf forests", "Deciduous needleleaf forests",
  "Deciduous broadleaf forests", "Mixed forests", "Closed shrublands", "Open shrublands",
  "Woody savannas", "Savannas", "Grasslands", "Permanent wetlands", "Croplands",
  "Urban and built-up lands", "Cropland/natural vegetation mosaics", "Snow and ice",
  "Barren", "Water bodies"
)

sotua_sf$landcover <- terra::extract(lc_2010, sotua_sf, ID = FALSE)[, 1] %>%
  factor(levels = 1:17, labels = landcover_levels)

table(sotua_sf$landcover, useNA = "ifany")


## ----ndvi_dates------------------------------------------------------------------------
extract_ndvi_date <- function(filename) {
  doy_str <- sub(".*doy([0-9]{7}).*", "\\1", filename)
  yr  <- as.numeric(substr(doy_str, 1, 4))
  doy <- as.numeric(substr(doy_str, 5, 7))
  as.Date(paste0(yr, "-01-01")) + (doy - 1)
}

ndvi_files <- list.files("Data/NDVI", pattern = "\\.tif$", full.names = TRUE)
ndvi_dates <- do.call(c, lapply(ndvi_files, extract_ndvi_date))

ndvi_stack <- rast(ndvi_files)
names(ndvi_stack) <- as.character(ndvi_dates)
nlyr(ndvi_stack)
range(ndvi_dates)


## ----ndvi_extract_wide-----------------------------------------------------------------
ndvi_wide <- terra::extract(ndvi_stack, sotua_sf, ID = FALSE)
dim(ndvi_wide)


## ----ndvi_nearest----------------------------------------------------------------------
nearest_layer <- vapply(
  sotua_sf$timestamp,
  function(t) which.min(abs(as.numeric(difftime(ndvi_dates, as.Date(t), units = "days")))),
  integer(1)
)

# MOD13Q1 v061 stores NDVI as a scaled integer - the 0.0001 factor converts back to the -1..1 range
sotua_sf$NDVI <- ndvi_wide[cbind(seq_len(nrow(ndvi_wide)), nearest_layer)] * 0.0001

summary(sotua_sf$NDVI)


## ----scale_all-------------------------------------------------------------------------
WB_sf$elevation_m <- terra::extract(elev, WB_sf, ID = FALSE)[, 1]
WB_sf$landcover    <- terra::extract(lc_2010, WB_sf, ID = FALSE)[, 1] %>%
  factor(levels = 1:17, labels = landcover_levels)

ndvi_wide_all   <- terra::extract(ndvi_stack, WB_sf, ID = FALSE)
nearest_all     <- vapply(
  WB_sf$timestamp,
  function(t) which.min(abs(as.numeric(difftime(ndvi_dates, as.Date(t), units = "days")))),
  integer(1)
)
WB_sf$NDVI <- ndvi_wide_all[cbind(seq_len(nrow(ndvi_wide_all)), nearest_all)] * 0.0001


## ----envdata_illustration--------------------------------------------------------------
tribble(
  ~individual_local_identifier, ~timestamp,            ~location_lat, ~location_long, ~`MODIS Land Surface Temperature (K)`, ~`ECMWF ERA5 10m U Wind`,
  "Sotua",                      "2011-03-04 06:00:00", -2.51,          37.02,          301.2,                                 -1.8
)


## ----rgee_setup, eval=FALSE------------------------------------------------------------
## # install.packages("rgee")
## library(rgee)
## 
## # Earth Engine requires an explicit Google Cloud project ID -
## # create/select one at https://console.cloud.google.com and enable the
## # Earth Engine API on it first.
## ee_Authenticate()
## 
## # Use ee$Initialize() (not ee_Initialize()) - rgee's own wrapper still
## # assumes EE's old "user"-based auth and currently throws a false
## # "credential expired" error on the project-based flow; calling the
## # underlying Python method directly avoids that bug.
## ee$Initialize(project = "your-gcp-project-id")


## ----rgee_extract, eval=FALSE----------------------------------------------------------
## sotua_ee <- sotua_sf %>% st_transform(4326)   # ee_extract expects an sf object
## 
## start_date <- format(min(sotua_ee$timestamp) - 16*86400, "%Y-%m-%d")
## end_date   <- format(max(sotua_ee$timestamp) + 16*86400, "%Y-%m-%d")
## 
## ndvi_ic <- ee$ImageCollection("MODIS/061/MOD13Q1") %>%
##   ee$ImageCollection$filterDate(start_date, end_date) %>%
##   ee$ImageCollection$select("NDVI")
## 
## ndvi_image <- ndvi_ic$toBands()   # collapse the time series into one multi-band image
## 
## ndvi_task <- ee_extract(
##   x = ndvi_image, y = sotua_ee, scale = 250,
##   via = "drive", lazy = TRUE, sf = FALSE
## )
## 
## ndvi_ee_vals <- ndvi_task %>% ee_utils_future_value()   # this line will report status. and you will see something like "State: COMPLETED" once the extration is done.


## ----rgee_area, eval=FALSE-------------------------------------------------------------
## terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
##   ee$ImageCollection$filterDate("2010-01-01", "2013-01-01") %>%
##   ee$ImageCollection$map(function(x) x$select("pr")) %>%
##   ee$ImageCollection$toBands()
## 
## mcp_wgs84 <- WB_mcp %>% st_sf() %>% st_transform(4326)
## 
## rain_by_month <- ee_extract(x = terraclimate, y = mcp_wgs84, fun = ee$Reducer$mean(), sf = FALSE)


## ----viz_ndvi_map----------------------------------------------------------------------
ggplot() +
  geom_sf(data = WB_mcp, fill = NA, color = "grey40", linewidth = 0.4) +
  geom_sf(data = WB_sf, aes(color = NDVI), size = 0.6, alpha = 0.7) +
  scale_color_viridis_c(option = "viridis", name = "NDVI") +
  labs(title = "Wildebeest GPS fixes colored by annotated NDVI",
       subtitle = "Athi-Kaputiei Plains, all individuals") +
  theme_minimal()


## ----viz_ndvi_elev---------------------------------------------------------------------
WB_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(NDVI), !is.na(elevation_m)) %>%
  ggplot(aes(x = elevation_m, y = NDVI)) +
  geom_point(alpha = 0.15, size = 0.5, color = "#2c7fb8") +
  geom_smooth(method = "loess", color = "black", linewidth = 0.6) +
  labs(title = "NDVI vs. elevation across all annotated fixes",
       x = "Elevation (m)", y = "NDVI") +
  theme_minimal()

