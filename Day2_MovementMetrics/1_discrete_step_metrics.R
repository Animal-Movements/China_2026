## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----packages-------------------------------------------------------------------------------------------------------------------------------
# Clear R's memory
rm(list=ls())

# You may need to install these packages first
# install.packages(c("amt", "move2", "sf", "tidyverse", "lubridate", "units"))

library(amt)         # Tracks, resampling, step metrics
library(move2)       # move2 helpers (mt_distance, mt_speed, mt_turnangle, ...)
library(sf)          # Spatial vector data
library(tidyverse)   # Data wrangling and visualization
library(lubridate)   # Timestamps and durations
library(units)       # Explicit unit conversions


## ----load-----------------------------------------------------------------------------------------------------------------------------------
load("../Day1_IntroMovementEcology/Data/WB_clean.rdata")  # loads WB.mv2, saved at the end of Day 1 Module 2

latlong_crs <- "EPSG:4326"   # WGS84 geographic — what move2/Movebank gives us
utm_crs     <- "EPSG:32737"  # UTM Zone 37S — metric, appropriate for Athi-Kaputiei, Kenya


## ----rebuild_trk_all------------------------------------------------------------------------------------------------------------------------
WB_df <- WB.mv2 %>%
  mutate(x = sf::st_coordinates(.)[, 1],
         y = sf::st_coordinates(.)[, 2]) %>%
  as_tibble() %>%
  transmute(id = individual_local_identifier,
            x, y,
            t = timestamp) %>%
  arrange(id, t)

trk_all <- make_track(WB_df, x, y, t, id = id, crs = latlong_crs)
trk_all <- transform_coords(trk_all, utm_crs)

class(trk_all)        # track_xyt, track_xy, tbl_df, tbl, data.frame
n_distinct(trk_all$id) # number of individuals


## ----nest-----------------------------------------------------------------------------------------------------------------------------------
trk_nested <- trk_all %>% nest(data = -id)
trk_nested


## ----sampling_rate--------------------------------------------------------------------------------------------------------------------------
sampling_rates <- trk_nested %>%
  mutate(rate = map(data, summarize_sampling_rate)) %>%
  select(id, rate) %>%
  unnest(rate)

sampling_rates


## ----resample-------------------------------------------------------------------------------------------------------------------------------
trk_resampled <- trk_nested %>%
  mutate(resampled = map(data, ~ track_resample(.x, rate = hours(1), tolerance = minutes(15)))) %>%
  select(id, resampled) %>%
  unnest(resampled)

head(trk_resampled)


## ----resample_3h----------------------------------------------------------------------------------------------------------------------------
trk_resampled_3h <- trk_nested %>%
  mutate(resampled = map(data, ~ track_resample(.x, rate = hours(3), tolerance = minutes(30)))) %>%
  select(id, resampled) %>%
  unnest(resampled)

head(trk_resampled_3h)


## ----one_individual-------------------------------------------------------------------------------------------------------------------------
one_trk <- trk_resampled %>% filter(id == "Sotua")

one_steps <- steps_by_burst(one_trk)
head(one_steps)


## ----speed_one------------------------------------------------------------------------------------------------------------------------------
one_steps <- one_steps %>%
  mutate(dt_s = as.numeric(dt_, units = "secs"),
         speed_m_s = sl_ / dt_s)

head(one_steps)


## ----all_steps------------------------------------------------------------------------------------------------------------------------------
trk_resampled_nested <- trk_resampled %>% nest(data = -id)

wb_steps <- trk_resampled_nested %>%
  mutate(steps = map(data, steps_by_burst)) %>%
  select(id, steps) %>%
  unnest(steps) %>%
  mutate(dt_s = as.numeric(dt_, units = "secs"),
         speed_m_s = sl_ / dt_s)

head(wb_steps)


## ----plot_sl--------------------------------------------------------------------------------------------------------------------------------
wb_steps %>%
  ggplot(aes(x = log(sl_ + 0.00001), fill = factor(id))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(x = "log step length (m)", fill = "ID", title = "Step length distribution")


## ----plot_ta--------------------------------------------------------------------------------------------------------------------------------
wb_steps %>%
  ggplot(aes(x = ta_, fill = factor(id))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(x = "turning angle (rad)", fill = "ID", title = "Turning angle distribution")


## ----plot_ta_circular-----------------------------------------------------------------------------------------------------------------------
wb_steps %>%
  mutate(ta_deg = ta_ * 180 / pi) %>%
  filter(!is.na(ta_deg)) %>%
  ggplot(aes(x = ta_deg, fill = factor(id))) +
  geom_density(alpha = 0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = "turning angle (degrees)", fill = "ID")


## ----plot_speed-----------------------------------------------------------------------------------------------------------------------------
wb_steps %>%
  ggplot(aes(x = speed_m_s, fill = factor(id))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(x = "speed (m/s)", fill = "ID", title = "Speed distribution")


## ----move2_speed----------------------------------------------------------------------------------------------------------------------------
WB_metric <- WB.mv2 %>%
  st_transform(utm_crs) %>%
  mutate(step_length_m = mt_distance(., units = "m"),
         speed_m_s     = mt_speed(., units = "m/s"))

WB_metric %>%
  st_drop_geometry() %>%
  select(individual_local_identifier, timestamp, step_length_m, speed_m_s) %>%
  head()


## ----move2_angle----------------------------------------------------------------------------------------------------------------------------
WB_angles <- WB.mv2 %>%
  st_transform(latlong_crs) %>%   # mt_turnangle()/mt_azimuth() are only implemented for geographic coordinates
  mutate(turn_angle = mt_turnangle(.),
         heading     = mt_azimuth(.))

WB_angles %>%
  st_drop_geometry() %>%
  select(individual_local_identifier, timestamp, turn_angle, heading) %>%
  head()

