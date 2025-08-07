
## ----install and load BaBA, eval = FALSE---------------------------------------
## require("devtools")
## devtools::install_github("wx-ecology/BaBA")


## ----install and load BaBA2, eval = FALSE--------------------------------------
## require("devtools")
## devtools::install_local("path/to/repository-folder") # they are under ./Package/


# SET WORKING DIRECTORY 
# setwd()

## ----packages, warning=FALSE, message=FALSE, results='hide'--------------------
# Remove from memory
rm(list=ls())

# load libearyß
library(BaBA)
library(sf)
library(mapview)
library(move2)
library(tidyverse) 
library(RColorBrewer)
library(gridExtra)


## ----read pronghorn and fence data---------------------------------------------
data("pronghorn")
data("fences")


## ----data check----------------------------------------------------------------
st_crs(pronghorn) == st_crs(fences)


## ----data visualization 1------------------------------------------------------

pronghorn.lines <- pronghorn %>% 
  mt_as_move2(., time_column = "date", track_id_column = "Animal.ID") %>%
  mt_track_lines() 

# the first line will be plotted first, with later lines overlaid on top
mapView(fences, color = "grey", layer.name = "Fencelines") +
mapView(pronghorn.lines, zcol = "Animal.ID",layer.name = "Pronghorn lines", cex = 1) 
    


## ----run baba 1----------------------------------------------------------------

# results_prong <- BaBA(animal = pronghorn, 
#                       barrier = fences, 
#                       b_time = 4, p_time = 36, d = 110, max_cross = 4,
#                       export_images = T, 
#                       img_path = "./Output/BaBA_viz_pronghorn/", img_suffix = "pronghorn")

# you can read results here:
results_prong <- read_rds("./Output/BaBA_results_prong.rds")


## ----view baba result 1--------------------------------------------------------
# View BaBA results

head(results_prong$classification)

# plot encounter event locations

  mapView(results_prong$encounters, zcol = "eventTYPE",
          col.regions = brewer.pal(n = 5, name = "Set1"), layer.name = "Pronghorn BaBA", cex = 4) +
    mapView(fences, color = "grey", layer.name = "Fencelines") 



## ----read WB data--------------------------------------------------------------
# read fence lines
fence.kenya <-  st_read("./Data/Fencelines2010_UTM37S.gpkg") 

# load data and filter out the unreliable locations
load("./Data/wildebeest_3hr_data.rdata")


WB <- WB.move %>%
  as_tibble() %>% 
  rename(Animal.ID = animal_id) %>%
  mutate(Animal.ID = factor(Animal.ID)) %>% # BaBA requires the ID column to be named "Animal.ID" and timestamp column to be namaed "date"
  drop_na(x, y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(32737))
head(WB)


## ----data transform------------------------------------------------------------
st_crs(WB) == st_crs(fence.kenya)



## ----data viz------------------------------------------------------------------

WB.lines <- WB %>% 
  mt_as_move2(., time_column = "date", track_id_column = "id") %>%
  mt_track_lines() 

mapView(WB.lines, zcol = "id",
          layer.name = "Wildebeest", cex = 1) +
    mapView(fence.kenya, color = "grey", layer.name = "Fencelines") 


## ----run baba on 2 ind---------------------------------------------------------

# pick two individuals with relatively large range in the study area 
# Nitishya and Sotua also have seperated range so the two of them should be a good representation 
# for the population 
WB.sf.2ind = WB %>% 
  filter(id %in% c("Ntishya", "Sotua")) %>%
  arrange(id, date)

# results_WB1 <- BaBA(animal = WB.sf.2ind,
#                       barrier = fence.kenya,
#                       d = 100,
#                       interval = 3, units = "hours",
#                       b_time = 3, p_time = 36,
#                       max_cross = 10, # setting it to a relatively big number to account for most intersections are not real fence crossing due to the moderate data resolution and high fence density
#                       export_images = T,
#                       img_path = "./Output/BaBA_viz_wb/b3_d100/", img_suffix = "wb_b3_d100")

# directly read results here
 results_WB1 <- read_rds("./Output/BaBA_2ind_WB1.rds")

# results_WB2 <- BaBA(animal = WB.sf.2ind,
#                       barrier = fence.kenya,
#                       d = 1000,
#                       interval = 3, units = "hours",
#                       b_time = 3, p_time = 36,
#                       max_cross = 10,
#                       export_images = T,
#                       img_path = "./Output/BaBA_viz_wb/b3_d1000/", img_suffix = "wb_b3_d1000")

# directly read results here
 results_WB2 <- read_rds("./Output/BaBA_2ind_WB2.rds")


## ----run baba 5----------------------------------------------------------------
# event = tibble()
# for (dist in seq(100,1000, by = 100)) {
# results_WB_2ind_dist <- BaBA(animal = WB.sf.2ind,
#                       barrier = fence.kenya,
#                       d = dist,
#                       interval = 3, units = "hours",
#                       b_time = 3, p_time = 36,
#                       max_cross = 10,
#                       tolerance = 1, # add the tolerance parameter
#                       export_images = F)
# 
# event.dist <- tibble(results_WB_2ind_dist$classification) %>% mutate(buffer_dist = dist)
# event <- rbind(event, event.dist)
# }

# directly read results here
event <- read_rds("./Output/BaBA_2ind_WB_dist.rds")



## ----compare dist results------------------------------------------------------
event_summary <- event %>%
  group_by(buffer_dist, eventTYPE) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(buffer_dist) %>%
  mutate(
    sum = sum(count),
    proportion = count / sum(count)) 

# stack bar plots to visualize total event counts and counts of different behaviors
p_all_type <- event_summary %>%
  ggplot(aes(x = factor(buffer_dist), y = count, fill= eventTYPE)) +
  geom_bar(stat = "identity") +
  labs(x = "Buffer Distance (m)", y = "Event Count", fill = "Event Type") +
  theme_minimal()

# line plots for each behavior to visualize the change in their proportion in total events
p_all_type_line <- event_summary %>%
  ggplot(aes(x = factor(buffer_dist), y = proportion, group= eventTYPE, color = eventTYPE)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Buffer Distance (m)", y = "Event Count", fill = "Event Type") +
  theme_minimal()
grid.arrange(p_all_type, p_all_type_line)


## ----baba WB full set----------------------------------------------------------
# results_WB_full <- BaBA(animal = WB,
#                       barrier = fence.kenya,
#                       d = 300,
#                       interval = 3, units = "hours",
#                       b_time = 3, p_time = 36,
#                       export_images = F)

results_WB_full <- read_rds("./Output/BaBA_results_WB_full.rds")


## ----baba WB full set plotting-------------------------------------------------
# reclassify quick-cross as bounce (due to reasons discussed above)
results_WB_full$encounters <- results_WB_full$encounters %>% 
  mutate(eventTYPE = ifelse(eventTYPE == "Quick_Cross", "Bounce", eventTYPE))

# Define colors for event types
event_colors <- brewer.pal(n = 6, name = "Set1")

ggplot() +
  # Plot fencelines
  geom_sf(data = fence.kenya, color = "grey", size = 0.3) +
  # Plot encounter points, colored by event type
  geom_sf(data = results_WB_full$encounters, aes(color = eventTYPE), size = 2) +
  scale_color_manual(values = event_colors) +
  theme_minimal() +
  labs(title = "Wildebeest Barrier Behavior Events",
       color = "Event Type") +
  theme(legend.position = "right")

