# ## ----setup, include=FALSE------------------------------------------------------
# knitr::opts_chunk$set(echo = TRUE) # display both code and results 
# knitr::opts_chunk$set(fig.pos = 'H')


## ----packages, warning=FALSE, message=FALSE, results='hide'--------------------
library(tidyverse, quietly = TRUE) #the tidyverse is an collection of R packages designed for data science. All packages share an underlying design philosophy, grammar, and data structures. Relavant packages for us includ `dplyr` (for data manipulation), `lubridate` (for dealing with time), `ggplot2` (for data visualization). 
library(sf, quietly = TRUE) #"simple features", lets us work with spatial vector data
library(amt, quietly = TRUE) # amt = animal movement tool. It is used to manage and analyze animal movement data. It can be used to calculate home range, track statistics, and prep the data for further analysis such as resource selection function (which you will learn more on day 3)
library(move2, quietly = TRUE) #handle, manipulate, and explore trajectory data with direct connection with MOVEBANK and sf 
library(units, quietly = TRUE) #support for measurement units 
library(rnaturalearth, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(leaflet,quietly = TRUE) # to create interactive maps 


## ----movebank credentials, eval = FALSE----------------------------------------
## movebank_store_credentials("myUserName", "myPassword") # replace user name and password with your own credentials


## ----read data-----------------------------------------------------------------
dat.mv <- movebank_download_study(study_id = 577226894,  
                        remove_movebank_outliers = T) # accept license when needed
dat.mv


## ----m2 viz, message=FALSE, warning=FALSE--------------------------------------
dat.lines <- dat.mv %>%
  mt_track_lines() 

pal <- colorFactor(palette = "Set1", domain = dat.lines$individual_local_identifier)

dat.lines %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(color = ~pal(individual_local_identifier), label = ~individual_local_identifier, weight = 2, opacity = 1) %>%
  addLegend("bottomright", pal = pal, values = ~individual_local_identifier, title = "Track ID")


## ----time lag------------------------------------------------------------------
# make sure the data is ordered
dat.mv <- dat.mv %>%
  group_by(individual_local_identifier) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  ungroup()

timeLags <- mt_time_lags(dat.mv)
head(timeLags)


## ----time lag2-----------------------------------------------------------------
timeLags <- units::set_units(timeLags, hours)

dat.mv <- dat.mv %>% 
  mutate(time_lags_h = mt_time_lags(., units = "hour")) 

lag.summary <- dat.mv %>%
  group_by(individual_local_identifier) %>%
  summarise(median_interval = median(time_lags_h, na.rm=T),
            sd_interval = round(sd(time_lags_h, na.rm=T),2))

lag.summary


## ----time lag3-----------------------------------------------------------------
table(is.na(dat.mv$time_lags_h))


## ----step speed 0, warning = FALSE---------------------------------------------
dat.mv <- st_transform(dat.mv, 29172)


## ----step speed 1, warning = FALSE---------------------------------------------
dat.mv <- dat.mv %>%
  mutate(
    step_length_m = mt_distance(., units = "m"),
    speed_m_s = mt_speed(., units = "m/s")
  )

p1 <- dat.mv %>% ggplot(aes(x = step_length_m)) +
  geom_histogram(bins = 50) +
  theme_minimal() 
p2 <- dat.mv %>% ggplot(aes(x = speed_m_s)) +
  geom_histogram(bins = 50) +
  theme_minimal() 
grid.arrange(p1, p2, ncol = 2)


## ----step speed2, warning = FALSE----------------------------------------------
p3 <- dat.mv %>% ggplot(aes(x = log(step_length_m))) +
  geom_histogram(bins = 50) +
  theme_minimal() 
p4 <- dat.mv %>% ggplot(aes(x = log(speed_m_s))) +
  geom_histogram(bins = 50) +
  theme_minimal() 
grid.arrange(p3, p4, ncol = 2)


## ----step/speed, eval = FALSE, echo = FALSE------------------------------------
## 
## pal <- colorNumeric(palette = "viridis", domain = log(as.numeric(dat.mv$speed_m_s) + 0.00001), na.color = "transparent")
## dat.mv %>%
##   st_transform(., 4326) %>% #leaflet maps require long-lat projection
##   mutate(speed_m_s = log(as.numeric(dat.mv$speed_m_s) + 0.00001)) %>%
##   leaflet() %>%
##   addTiles() %>%
##   addCircleMarkers(
##     radius = 3,
##     color = ~pal(speed_m_s),
##     stroke = FALSE,
##     fillOpacity = 0.8,
##     label = ~paste0("ID: ", individual_local_identifier, " Speed: ", round(speed_m_s, 2), " m/s"),
##     labelOptions = labelOptions(direction = "auto")
##   ) %>%
##   addLegend("bottomright", pal = pal, values = ~speed_m_s, title = "Speed (m/s)")
## 


## ----turning angles, warning = FALSE-------------------------------------------
dat.mv <- dat.mv %>% 
  st_transform(., 4326) %>% # then the angle calculation require geographic coordinate system again 
  mutate(turnAngles = mt_turnangle(.),
         heading = mt_azimuth(.))

p5 <- dat.mv %>% 
  filter(individual_local_identifier %in% c("Piloto"),
         timestamp > ymd("2019-03-24")) %>% 
  mutate(state = ifelse(timestamp < ymd("2019-04-08"), "A", "B") ) %>%
  ggplot(aes(x = turnAngles, fill = state)) +
  geom_density(alpha = 0.8) +
  theme_minimal()

p6 <- dat.mv %>% 
  filter(individual_local_identifier %in% c("Piloto"),
         timestamp > ymd("2019-03-24")) %>% 
  mutate(state = ifelse(timestamp < ymd("2019-04-08"), "A", "B") ) %>%
  ggplot(aes(color = state)) + geom_sf() +
  theme_minimal()
  
grid.arrange(p5, p6, ncol = 2)


## ----Data Import, message=FALSE, warning=FALSE---------------------------------
load("./Data/wildebeest_data.rdata")


## ----data clean----------------------------------------------------------------
# check is all observations are complete 
WB <- WB %>%
  as_tibble() %>%
  filter(
    fixType == "3D" & as.numeric(DOP) < 10 | fixType == "2D" & as.numeric(DOP)  < 5)

all(complete.cases(WB)) 


## ----check duplication---------------------------------------------------------
WB %>% group_by(animal_id) %>% summarise(any(duplicated(timestamp)))


## ----create track--------------------------------------------------------------
wb.trk <- make_track(WB, 
                            # tell the function which column contains the coordinates
                            .x = longitude, .y = latitude, 
                            # tell the function which CRS the coordinates are based on. 
                            crs = 4326, 
                            .t = timestamp,
                            # any additional information you want to attach to the track
                            # such as animal ID and month
                            id = animal_id, sex = sex, temp = temp) 

class(wb.trk)


## ----samling rate--------------------------------------------------------------
summarize_sampling_rate(wb.trk)


## ----amt step0-----------------------------------------------------------------
wb.trk.UTM <- transform_coords(wb.trk, 32737)


## ----amt step------------------------------------------------------------------
# first we filter the data to the individual of interest
one.wb.trk.UTM <- wb.trk.UTM %>% filter(id == "Sotua")

one.wb.steps <- steps(one.wb.trk.UTM)

head(one.wb.steps)


## ----nest----------------------------------------------------------------------

wb.trk.nested <- wb.trk.UTM %>% nest(data = -"id")

wb.trk.nested
  


## ----turn to step--------------------------------------------------------------

wb.step.nested <- wb.trk.nested %>% mutate (steps = map(data, function(x) {x %>% steps()} )) 

wb.step.nested

# let's look at one element in the column "steps"
wb.step.nested$steps[1]


## ----unnest--------------------------------------------------------------------

wb.step <- wb.step.nested %>% 
  select (id, steps) %>% #select the two columns of interest
  unnest(cols = steps) #unnest the steps column

head(wb.step)


## ----all individual sl distribution--------------------------------------------

wb.step %>% 
  ggplot (aes(x = log(sl_+0.00001), fill = factor(id))) +  #sometimes animals have 0 step length. To avoid issue when doing "log", we can add a very small number to all steps
  geom_density(alpha = 0.3) + #alpha determines transparency 
  theme_minimal() +
  labs ( x = "log step length", fill = "ID", title = "The Distribution of Wildebeest Step Length")



## ----all individual angle distribution, warning=F------------------------------

wb.step %>% ggplot (aes(x = ta_, fill = factor(id))) +
  geom_density(alpha = 0.3) + #alpha determines transparency
  theme_minimal() +
  labs (x = "turning angles (rad)", fill = "ID")



## ----circlur plotting----------------------------------------------------------
wb.step %>% 
  mutate(ta_degree = ta_ * 180/pi) %>%
  filter(!is.na(ta_degree)) %>%
  ggplot(aes(x = ta_degree, fill = id)) +
  geom_density(alpha = 0.4) +
  coord_polar() +
  theme_minimal() +
  labs (x = "turning angles (degree)")



## ----amt track metrics---------------------------------------------------------
wb.trk.nested  <-  wb.trk.nested %>% mutate (
  tot_dist = map_dbl(data, function(x) {x %>% tot_dist()}),
  straightness = map_dbl(data, function(x) {x %>% straightness()}), 
  intensity_use = map_dbl(data, function(x) {x %>% intensity_use()})) 
wb.trk.nested 


## ----amt track nsd-------------------------------------------------------------
wb.trk.nsd <- wb.trk.nested %>% 
  mutate (data = map(data, function(x) {x %>% add_nsd()})) %>% 
  select (id, data) %>% #select the two columns of interest
  unnest(cols = data)

p_nsd <- wb.trk.nsd %>% ggplot(aes(x = t_, y = nsd_, color = id)) +
  geom_line() +
  theme_minimal() 
p_nsd

