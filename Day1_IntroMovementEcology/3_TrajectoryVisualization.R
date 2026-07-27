## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----packages----------------------------------------------------------------------------------------------------
# install.packages(c("amt", "sf", "move2", "tidyverse", "lubridate", "gt", "mapview", "gganimate"))

library(amt)         # Tracks, resampling, step metrics
library(sf)          # Spatial vector data — points and lines
library(move2)       # move2 helpers (mt_track_id, etc.)
library(tidyverse)   # Data wrangling and visualization
library(lubridate)   # Timestamps and durations
library(mapview)     # Quick interactive maps


## ----load--------------------------------------------------------------------------------------------------------
load("Data/WB_clean.rdata")   # loads WB.mv2, saved at the end of Module 2

cat("Starting point:", n_distinct(mt_track_id(WB.mv2)), "animals,", nrow(WB.mv2), "fixes\n")


## ----crs---------------------------------------------------------------------------------------------------------
latlong_crs <- "EPSG:4326"   # WGS84 geographic — what move2/Movebank gives us
utm_crs     <- "EPSG:32737"  # UTM Zone 37S — metric, appropriate for Athi-Kaputiei, Kenya


## ----flatten-----------------------------------------------------------------------------------------------------
WB_df <- WB.mv2 %>%
  mutate(x = sf::st_coordinates(.)[, 1],
         y = sf::st_coordinates(.)[, 2]) %>%
  as_tibble() %>%
  transmute(id = individual_local_identifier,
            x, y,
            t = timestamp) %>%
  arrange(id, t)

head(WB_df)


## ----make_track--------------------------------------------------------------------------------------------------
trk_all <- make_track(WB_df, x, y, t, id = id, crs = latlong_crs)

class(trk_all)     # track_xyt, track_xy, tbl_df, tbl, data.frame
head(trk_all)


## ----reproject---------------------------------------------------------------------------------------------------
trk_all <- transform_coords(trk_all, utm_crs)

head(trk_all)
plot(trk_all)


## ----sf_lines----------------------------------------------------------------------------------------------------
WB.sf <- sf::st_as_sf(as.data.frame(WB.mv2)) %>%
  sf::st_transform(utm_crs)

WB.lines <- WB.sf %>%
  group_by(individual_local_identifier) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING")

WB.lines


## ----sf_static_map-----------------------------------------------------------------------------------------------
ggplot() +
  geom_sf(data = WB.lines, aes(color = individual_local_identifier), linewidth = 0.4) +
  scale_color_viridis_d(name = "Individual") +
  labs(title = paste("Wildebeest trajectories — Athi-Kaputiei Plains (n =",
                      n_distinct(WB.lines$individual_local_identifier), ")")) +
  theme_minimal()


## ----sf_interactive_map------------------------------------------------------------------------------------------
mapview(WB.lines, zcol = "individual_local_identifier", layer.name = "Trajectories", lwd = 2) +
  mapview(WB.sf %>% slice_sample(n = 2000), zcol = "individual_local_identifier",
          layer.name = "Fixes", cex = 1.5, alpha = 0.6)


## ----nnp_overlay, eval=FALSE-------------------------------------------------------------------------------------
## # Read in your boundary (reproject to match your trajectories' CRS):
## # boundary <- sf::st_read("Data/your_boundary.shp", quiet = TRUE) %>%
## #   sf::st_transform(utm_crs)
## #
## # mapview(WB.lines, zcol = "individual_local_identifier", layer.name = "Trajectories") +
## #   mapview(boundary, col.regions = "green", alpha.regions = 0.3, layer.name = "Reserve boundary")


## ----animation_subset--------------------------------------------------------------------------------------------
# install.packages(c("gganimate", "gifski", "moveVis", "units"))
library(gganimate)   # animate ggplot2 plots through time

# For a *quick*, in-class demo we restrict to a few individuals and a short
# time window: animating the full population over the full deployment means
# thousands of frames, which is impractical to render live.
demo_ids   <- unique(as.character(WB.mv2$individual_local_identifier))[1:3]
demo_start <- min(WB.mv2$timestamp)
demo_end   <- demo_start + weeks(2)

WB_demo <- WB.mv2 %>%
  filter(as.character(individual_local_identifier) %in% demo_ids,
         timestamp >= demo_start, timestamp <= demo_end)

WB_demo_df <- WB_demo %>%
  mutate(x = sf::st_coordinates(.)[, 1],
         y = sf::st_coordinates(.)[, 2]) %>%
  as_tibble() %>%
  transmute(id = as.character(individual_local_identifier), x, y, t = timestamp) %>%
  arrange(id, t)

cat(n_distinct(WB_demo_df$id), "animals,", nrow(WB_demo_df), "fixes in the demo window\n")


## ----gganimate_build, eval=FALSE---------------------------------------------------------------------------------
## anim <- ggplot(WB_demo_df, aes(x = x, y = y, color = id)) +
##   geom_point(size = 3) +
##   scale_color_viridis_d(name = "Individual") +
##   coord_quickmap() +
##   labs(title = "Wildebeest movement — {format(frame_time, '%Y-%m-%d %H:%M')}",
##        x = "Longitude", y = "Latitude") +
##   theme_minimal() +
##   transition_time(t) + # maps `t` onto animation frames and linearly interpolates each individual's position between consecutive real fixes. Effective for visualization but not robust enough for analyses
##   shadow_wake(wake_length = 0.2, size = TRUE) # leaves each point a short fading trail so direction of travel is easy to read.
## 
## animate(anim, nframes = 100, fps = 10, width = 800, height = 600,
##         renderer = gifski_renderer())   # preview


## ----gganimate_render, eval=FALSE--------------------------------------------------------------------------------
## anim_gif <- animate(anim, nframes = 200, fps = 12, width = 900, height = 700,
##                      renderer = gifski_renderer())
## anim_save("wildebeest_movement.gif", anim_gif)


## ----source_align_move_fixed-------------------------------------------------------------------------------------
# install.packages(c("moveVis", "units"))
library(moveVis)
library(units)

source("./align_move_fixed.R")


## ----movevis_align, eval=FALSE-----------------------------------------------------------------------------------
## WB_aligned <- align_move_fixed(WB_demo, res = units::set_units(6, "hours"))
## 
## # sanity check: the original bug collapsed every individual's interpolated
## # positions onto a single point. Confirm each individual still has many
## # distinct coordinates after alignment (should be close to its row count,
## # not 1):
## sapply(split(WB_aligned, mt_track_id(WB_aligned)),
##        function(x) length(unique(sf::st_coordinates(x)[, 1])))


## ----movevis_frames, eval=FALSE----------------------------------------------------------------------------------
## frames <- frames_spatial(WB_aligned, map_service = "osm", map_type = "topographic",
##                           path_legend = TRUE, path_legend_title = "Individual") %>%
##   add_northarrow(position = "bottomleft") %>%
##   add_scalebar(colour = "black", position = "bottomright") %>%
##   add_timestamps(type = "label") %>%
##   add_progress()
## 
## frames[[1]]   # preview a single frame before rendering the full animation


## ----movevis_render, eval=FALSE----------------------------------------------------------------------------------
## animate_frames(frames, out_file = "wildebeest_movement_movevis.gif", overwrite = TRUE)

