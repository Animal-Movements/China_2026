## ----setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE) # display both code and results 
knitr::opts_chunk$set(fig.pos = 'H')


## ----packages, warning=FALSE, message=FALSE, results='hide'--------
library(tidyverse, quietly = TRUE) 
library(amt, quietly = TRUE) 
library(leaflet,quietly = TRUE)
library(RColorBrewer)  # for color palettes
library(sf)


# load data and filter out the unreliable locations
load("./Data/wildebeest_data.rdata")

WB <- WB %>%
  as_tibble() %>%
  filter(
    fixType == "3D" & as.numeric(DOP) < 10 | fixType == "2D" & as.numeric(DOP)  < 5)



## ----track creation------------------------------------------------
wb.trk <- make_track(WB, 
                            # tell the function which column contains the coordinates
                            .x = longitude, .y = latitude, 
                            # tell the function which CRS the coordinates are based on. 
                            crs = 4326, 
                            .t = timestamp,
                            # any additional information you want to attach to the track
                            # such as animal ID and month
                            id = animal_id, sex = sex, temp = temp) 

# because we want to calculate home range area in m2 or km2, 
# we will transform the data to projected coordinate system.
wb.trk.UTM <- transform_coords(wb.trk, 32737)



## ----amt filter to one---------------------------------------------
# first we filter the data to the individual of interest
one.wb.trk.UTM <- wb.trk.UTM %>% filter(id == "Ole Nagol")


## ----MCP-----------------------------------------------------------
# estimate mcp at the level of 50% and 95%
one.wb.mcp <- hr_mcp(one.wb.trk.UTM, levels = c(0.5, 0.95))
plot(one.wb.mcp, main = paste("mcp")) 


## ----locoh---------------------------------------------------------

one.wb.locoh.20 <- hr_locoh(one.wb.trk.UTM, type = "k", n= 20, levels =  c(0.5, 0.95))
one.wb.locoh.50 <- hr_locoh(one.wb.trk.UTM, type = "k", n= 50, levels =  c(0.5, 0.95))
one.wb.locoh.100 <- hr_locoh(one.wb.trk.UTM, type = "k", n= 100, levels =  c(0.5, 0.95))

par(mfrow=c(1,3))
plot(one.wb.locoh.20, main = "k-locoh 5")
plot(one.wb.locoh.50, main = "k-locoh 50")
plot(one.wb.locoh.100, main = "k-locoh 100")


## ----KDE-----------------------------------------------------------
# Create a template raster for the KDE
trast <- make_trast(one.wb.trk.UTM, res = 50)
one.wb.kde <- hr_kde(one.wb.trk.UTM, trast = trast, levels = c(0.5, 0.95))
plot(one.wb.kde, main = "kde")


## ----mcp and kde and locoh for one wb - area-----------------------
hr_area_one_wb <- cbind(tibble(method = rep(c("mcp", "k-locoh 100", "kde"), each = 2)),
                        rbind(hr_area(one.wb.mcp), hr_area(one.wb.locoh.100), hr_area(one.wb.kde))) %>%
  select(-what)

hr_area_one_wb %>% ggplot(aes(x = method, y = area/1000000, #convert m2 to km2
                              group = factor(level), fill = factor(level))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + 
  ylab("area km^2")



## ----mcp_all_ind, warning=FALSE------------------------------------

# step 1: nest the track by id

wb.trk.nested <- wb.trk.UTM %>% nest(data = -"id")

# step 2: calculate mcp 

wb.trk.nested <- wb.trk.nested %>% mutate(mcp = map(data, function(x) {
    mcp = hr_mcp(x, levels = 0.95)
    return(mcp$mcp)
    }
  ))

# step 3: combine mcp of multiple individuals to one sf object

wb.mcp <- bind_rows(wb.trk.nested$mcp) %>% mutate (id = wb.trk.nested$id)

wb.mcp



## ----mcp area bar, eval = FALSE, echo = FALSE----------------------
## 
## wb.mcp %>% ggplot() +
##   # turn area from m^2 to km^2 for easier interpretation
##   geom_bar(aes( x = id, y = as.numeric(area)/1000000),
##            stat = "identity") +
##   theme_minimal() +
##   labs (y = "Area (km^2)")
## 


## ----all wb mcp----------------------------------------------------
p_mcp <-  ggplot() +
  geom_sf(data = wb.mcp, aes(color = id), fill = NA, alpha = 0.4, linewidth = 0.8) +
  theme_minimal()
p_mcp



## ----leaflet-------------------------------------------------------
# leaflet only takes long-lat data, so we transform it back to epsg:4326

wb.mcp.geo <- wb.mcp %>% st_transform(crs = st_crs(4326))

pal <- colorFactor(
  palette = brewer.pal(length(unique(wb.mcp.geo$id)), "Set3"),
  domain = wb.mcp.geo$id
)

# Create leaflet map
leaflet(wb.mcp.geo) %>%
  addProviderTiles("CartoDB.Positron") %>%  # add a base map
  addPolygons(
    fillColor = ~pal(id),
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.6,
    popup = ~paste("Animal ID:", id)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~id, title = "Animal ID")


## ----same results in a data frame----------------------------------
wb.mcp.df <- st_drop_geometry(wb.mcp)

