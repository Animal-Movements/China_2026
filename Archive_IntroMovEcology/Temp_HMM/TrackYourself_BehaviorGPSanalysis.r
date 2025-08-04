# Duke Movement Ecology Course, Spring Block C, Track Yourself Project!
# Instructor and code author: Dr. Autumn-Lynn Harrison (HarrisonAL@si.edu)
# GPS Movement Data Analysis
# This script processes and analyzes GPS tracking data to extract behavioral insights using a simple k-means clustering
# Designed as a first step to learning behavioral classification with more advanced modeling methods like Hidden Markov Models 

# Load required packages
library(tidyverse)    # For data manipulation and visualization
library(lubridate)    # For handling date-time data
library(sf)           # For spatial data processing
library(leaflet)      # For interactive maps
library(geosphere)    # For geographical calculations
library(zoo)          # For regularizing time series data
library(RColorBrewer) # for picking effective color pallets

# 1. Import and prepare the data
# -----------------------------------------------

# Import GPS data (adjust file path as needed)
# EXAMPLE FROM MY OWN TRACK
path = "/Users/harrisonAL/Dropbox (Personal)/ProfessionalActivities/Courses/Duke_MovementEcology/Week2/Project_TrackYourself/"
gps_data <- read.csv(paste(path,"CapeLookout.csv",sep=""), stringsAsFactors = FALSE)

# Display structure and summary
str(gps_data)
summary(gps_data)

# 2. Convert GMT to local time
# -----------------------------------------------

# Parse date and time columns
# Note: You may need to adjust the format based on your actual data format
# Check the first few rows to determine the correct format
head(gps_data$`Date.GMT.`)
head(gps_data$`Date.Local.`)

# If the dates are in a standard format like "YYYY-MM-DD HH:MM:SS"
gps_data$timestamp_gmt <- as.POSIXct(gps_data$`Date.GMT.`, tz = "GMT")
gps_data$timestamp_local <- as.POSIXct(gps_data$`Date.Local.`, tz = "America/New_York")

# If your local timezone isn't correctly set, specify it
# Common timezones: "America/New_York", "Europe/London", "Australia/Sydney", etc.
# gps_data$timestamp_local <- force_tz(gps_data$timestamp_local, tzone = "Your/Timezone")

# Create columns for hour, day, etc. for easier analysis
gps_data <- gps_data %>%
  mutate(
    hour = hour(timestamp_local),
    minute = minute(timestamp_local),
    day = day(timestamp_local),
    date = as.Date(timestamp_local)
  )

# 3. Calculate average sampling frequency
# -----------------------------------------------

# Sort by timestamp if not already sorted
gps_data <- gps_data %>% arrange(timestamp_gmt)

# Calculate time difference between consecutive points in seconds
# Using the Time(sec) column if it represents elapsed time
if("Time(sec)" %in% colnames(gps_data)) {
  gps_data <- gps_data %>%
    mutate(time_diff = c(NA, diff(`Time(sec)`)))
} else {
  # Otherwise calculate from timestamps
  gps_data <- gps_data %>%
    mutate(time_diff = c(NA, diff(as.numeric(timestamp_gmt))))
}

# Calculate average sampling frequency (in seconds)
avg_sampling_freq <- mean(gps_data$time_diff, na.rm = TRUE)
cat("Average sampling frequency:", round(avg_sampling_freq, 2), "seconds\n")

# 4. Plot sampling events through time
# -----------------------------------------------

# Create dot plot showing sampling events over time
sampling_plot <- ggplot(gps_data, aes(x = timestamp_local, y = 1)) +
  geom_point(size = 1, alpha = 0.7) +
  theme_minimal() +
  labs(title = "GPS Sampling Events Throughout the Day",
       x = "Local Time",
       y = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Display the plot
print(sampling_plot)

# Save the plot
ggsave("sampling_events.png", plot = sampling_plot, width = 8, height = 4)

# 5. Create a map of the movement path
# -----------------------------------------------

# Convert data to sf object for spatial operations
gps_sf <- st_as_sf(gps_data, 
                   coords = c("Longitude", "Latitude"), 
                   crs = 4326)  # WGS84 coordinate reference system

# Create a basic interactive map
movement_map <- leaflet(gps_sf) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    radius = 3,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.5
  ) 
# %>%
#addPolylines(color = "red", weight = 2, opacity = 0.7)

#Error for polylines in to_ring.default(x) : 
#  Don't know how to get polygon data from object of class XY,POINT,sfg

# Display the map
movement_map

# 6. Regularize the data to a common time step
#    This allows students to see places on the map where they were traveling faster or slower
# -----------------------------------------------

# Define desired time interval in minutes
time_interval <- 1  # 1 minute intervals

# Create a sequence of regular timestamps
start_time <- min(gps_data$timestamp_gmt)
end_time <- max(gps_data$timestamp_gmt)
regular_times <- seq(from = start_time, 
                     to = end_time, 
                     by = paste(time_interval, "min"))

# COULD USE FUNCTION FROM ESTABLISHED MOVEMENT PACKAGE
# Function to interpolate points
interpolate_points <- function(times, data) {
  # Create empty dataframe for results
  result <- data.frame(
    timestamp = times,
    Latitude = NA,
    Longitude = NA
  )
  
  # Interpolate lat/long values for each regular timestamp
  for (i in 1:length(times)) {
    t <- times[i]
    
    # Find closest points before and after
    before <- which(data$timestamp_gmt <= t)
    after <- which(data$timestamp_gmt >= t)
    
    if (length(before) > 0 && length(after) > 0) {
      before_idx <- max(before)
      after_idx <- min(after)
      
      # If exact match, use that point
      if (before_idx == after_idx) {
        result$Latitude[i] <- data$Latitude[before_idx]
        result$Longitude[i] <- data$Longitude[before_idx]
      } else {
        # Linear interpolation
        t_before <- data$timestamp_gmt[before_idx]
        t_after <- data$timestamp_gmt[after_idx]
        
        # Calculate weights
        w_after <- as.numeric(difftime(t, t_before, units = "secs")) / 
          as.numeric(difftime(t_after, t_before, units = "secs"))
        w_before <- 1 - w_after
        
        # Interpolate coordinates
        result$Latitude[i] <- w_before * data$Latitude[before_idx] + 
          w_after * data$Latitude[after_idx]
        result$Longitude[i] <- w_before * data$Longitude[before_idx] + 
          w_after * data$Longitude[after_idx]
      }
    }
  }
  
  return(result)
}

# Apply interpolation
regular_gps <- interpolate_points(regular_times, gps_data)

# Convert to local time
regular_gps$timestamp <- as.POSIXct(regular_gps$timestamp, tz = "GMT")
regular_gps$timestamp_local <- with_tz(regular_gps$timestamp, tzone = "America/New_York")

# Create sampling plot to confirm interpolation
sampling_plot_reg <- ggplot(regular_gps, aes(x = timestamp_local, y = 1)) +
  geom_point(size = 1, alpha = 0.7) +
  theme_minimal() +
  labs(title = "GPS Sampling Events Throughout the Day",
       x = "Local Time",
       y = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Display the plot
# Students compare to their original sampling plot to see that gaps are filled in / points are regularized
print(sampling_plot_reg)


# -----------------------------------------------

# Plot regularized data-start to see patterns
# -----------------------------------------------
head(regular_gps)

# Convert data to sf object for spatial operations
regular_gps = regular_gps[2:dim(regular_gps)[1],]
regular_sf <- st_as_sf(regular_gps, 
                       coords = c("Longitude", "Latitude"), 
                       crs = 4326)  # WGS84 coordinate reference system

# Create a basic interactive map
movement_map_reg <- leaflet(regular_sf) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    radius = 3,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.5
  ) 
# %>%
#addPolylines(color = "red", weight = 2, opacity = 0.7)

# Display the map
movement_map_reg

# In the example Cape Lookout track, can now see large gaps along the road (fast movement) and clustered locations where 
# I was moving slowly
# can compare to movement_map that was at the original high frequency sampling rate

# 7. Calculate movement metrics
# -----------------------------------------------

# This section could be greatly streamlined and swapped with function from a movement pacakge

# Function to calculate distance between points (in meters)

haversine_distance <- function(lon1, lat1, lon2, lat2) {
  # Convert degrees to radians
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180
  
  # Earth radius in meters
  R <- 6371000
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  d <- R * c
  
  return(d)
}

# Calculate step length, speed, and turn angle
movement_metrics <- data.frame(
  timestamp = regular_gps$timestamp[-1],
  timestamp_local = regular_gps$timestamp_local[-1],
  Longitude = regular_gps$Longitude[-1],
  Latitude = regular_gps$Latitude[-1],
  step_length = NA,
  speed = NA,
  turn_angle = NA
)

for (i in 1:(nrow(regular_gps)-1)) {
  # Calculate step length (distance in meters)
  movement_metrics$step_length[i] <- haversine_distance(
    regular_gps$Longitude[i], regular_gps$Latitude[i],
    regular_gps$Longitude[i+1], regular_gps$Latitude[i+1]
  )
  
  # Calculate speed (meters per second)
  time_diff <- as.numeric(difftime(regular_gps$timestamp[i+1], 
                                   regular_gps$timestamp[i], 
                                   units = "secs"))
  
  movement_metrics$speed[i] <- movement_metrics$step_length[i] / time_diff
  
  # Calculate bearing for turn angle
  if (i > 1) {
    # Calculate bearings for consecutive segments
    bearing1 <- geosphere::bearing(
      c(regular_gps$Longitude[i-1], regular_gps$Latitude[i-1]),
      c(regular_gps$Longitude[i], regular_gps$Latitude[i])
    )
    
    bearing2 <- geosphere::bearing(
      c(regular_gps$Longitude[i], regular_gps$Latitude[i]),
      c(regular_gps$Longitude[i+1], regular_gps$Latitude[i+1])
    )
    
    # Calculate turn angle (0 to 180 degrees)
    turn <- abs((bearing2 - bearing1) %% 360)
    if (turn > 180) turn <- 360 - turn
    
    movement_metrics$turn_angle[i-1] <- turn
  }
}

# Convert speed to km/h for easier interpretation
movement_metrics$speed_kmh <- movement_metrics$speed * 3.6

# Summary statistics of movement metrics
summary(movement_metrics[c("step_length", "speed_kmh", "turn_angle")])

# Compare calculated speed with the original dataset
cat("Note: The original dataset from my GPS app already contains speed data.\n")
cat("You can compare your calculated speed with the provided measurements.\n")

# 8. Create color-coded maps based on movement metrics
# -----------------------------------------------

# Convert to sf object
movement_sf <- st_as_sf(movement_metrics, 
                        coords = c("Longitude", "Latitude"), 
                        crs = 4326)

# Create color palettes
speed_pal <- colorNumeric(
  palette = "viridis",
  domain = movement_sf$speed_kmh
)

step_pal <- colorNumeric(
  palette = "plasma",
  domain = movement_sf$step_length
)

turn_pal <- colorNumeric(
  palette = "magma",
  domain = movement_sf$turn_angle,
  na.color = "transparent"
)

# Create map by speed
speed_map <- leaflet(movement_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 3,
    color = ~speed_pal(speed_kmh),
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste("Speed:", round(speed_kmh, 1), "km/h<br>Time:", timestamp_local)
  ) %>%
  #addPolylines(color = ~speed_pal(speed_kmh), weight = 3, opacity = 0.7) %>%
  addLegend(
    position = "bottomright",
    pal = speed_pal,
    values = ~speed_kmh,
    title = "Speed (km/h)"
  )

# Map by step length
step_map <- leaflet(movement_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 3,
    color = ~step_pal(step_length),
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste("Step length:", round(step_length, 1), "m<br>Time:", timestamp_local)
  ) %>%
  #addPolylines(color = ~step_pal(step_length), weight = 3, opacity = 0.7) %>%
  addLegend(
    position = "bottomright",
    pal = step_pal,
    values = ~step_length,
    title = "Step Length (m)"
  )

# Map by turn angle
turn_map <- leaflet(movement_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 3,
    color = ~turn_pal(turn_angle),
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste("Turn angle:", round(turn_angle, 1), "degrees<br>Time:", timestamp_local)
  ) %>%
  #addPolylines(color = ~turn_pal(turn_angle), weight = 3, opacity = 0.7) %>%
  addLegend(
    position = "bottomright",
    pal = turn_pal,
    values = ~turn_angle,
    title = "Turn Angle (°)"
  )

# Display maps
speed_map
step_map
turn_map

# Check to make sure this makes sense "biologically"

# 9. Identify behavioral states using a practical clustering approach
# -----------------------------------------------

# Simple clustering approach based on speed and turn angle
movement_metrics_clean <- movement_metrics %>%
  drop_na() %>%  # Remove rows with NAs
  select(speed_kmh, turn_angle, step_length)

# Scale the data for clustering
scaled_metrics <- scale(movement_metrics_clean)

# Determine optimal number of clusters
# We'll manually select the number of clusters you choose here based on your knowledge of your own movements
# But, after generating your first map, you might decide that it didn't cluster things the way you expected

# K-means clustering with 3 clusters
## NOTE: you can choose the number of clusters here based on your own knowledge of your movement path. 
# Play around with how it affects the map
# In the example dataset (Cape Lookout) I know I was driving, walking, and taking a boat. So I started with 3. 
# But I was also stopped at some points, and my walking likely varied between fast walking and exploring so I might later try 4 or 5

# centers = # of clusters you would like to try
centers = 5

set.seed(123)  # For reproducibility
km <- kmeans(scaled_metrics, centers = centers)

# Add cluster assignments to the data
movement_metrics$cluster <- NA
movement_metrics$cluster[!is.na(movement_metrics$turn_angle)] <- km$cluster

# Label clusters based on characteristics
cluster_summary <- aggregate(
  movement_metrics_clean,
  by = list(Cluster = km$cluster),
  FUN = mean
)
print(cluster_summary)

# Assign behavioral labels based on cluster characteristics
# This is a simple example - you may want to customize based on your results
#behavior_labels <- c("Stationary/Slow", "Walking", "Fast Movement")

# BELOW ARE EXAMPLES FROM MY ANALYSIS :)
# I assigned these based on the knowledge of my movements and how it matches the speed/distance/angle measures
# So, the fastest speeds were driving, low turn angles (Cluster 1)
# Exploring had a high turn angle as you would expect with area restricted search and low speed (Cluster 3). I was looking at shells on the beach
# and so on

# 3 cluster classification
behavior_labels <- c("Driving", "Boating", "Walking")

# 4 cluster classification
behavior_labels <- c("Driving", "Walking", "Exploring", "Boating")

# 5 cluster classification
behavior_labels <- c("Driving", "Boating", "Stopped", "Walking", "Exploring")

cluster_behavior <- behavior_labels[km$cluster]

# Add behavior labels to the data
movement_metrics$behavior <- NA
movement_metrics$behavior[!is.na(movement_metrics$turn_angle)] <- cluster_behavior

# Check that your assignment works well by checking the new cluster number and behavior columns
head(movement_metrics)

# 10. Map behavioral states
# -----------------------------------------------

# Create behavior-colored map
behavior_sf <- st_as_sf(movement_metrics, 
                        coords = c("Longitude", "Latitude"), 
                        crs = 4326)

# The number of colors are generated from the number of behavior labels you have (length(behavior_labels)) 
colors <- colorRampPalette(
  RColorBrewer::brewer.pal(length(behavior_labels), "Dark2")
)(length(levels(factor(behavior_sf$behavior))))

palBehav <- leaflet::colorFactor(colors,
                                 domain = levels(factor(behavior_sf$behavior)))

## Note, you can also create your own appropriate palette depending on how well the defaults
## show up on your base layer. I chose Dark2 from the RColorBrewer palette to show up better than RGY
# https://r-graph-gallery.com/38-rcolorbrewers-palettes.html


# Behavioral map
behavior_map <- leaflet(behavior_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 4,
    #color = c("red", "blue", "green"),
    color = ~palBehav(behavior),
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste("Behavior:", behavior, 
                   "<br>Speed:", round(speed_kmh, 1), "km/h",
                   "<br>Turn angle:", round(turn_angle, 1), "degrees",
                   "<br>Time:", timestamp_local)
  ) %>%
  #addPolylines(color = ~factor(behavior), weight = 3, opacity = 0.7) %>%
  addLegend(
    position = "bottomright",
    colors = colors,
    #labels = behavior_labels,
    # NOTE, these should be changed to match your interpretation
    labels = levels(factor(behavior_sf$behavior)), #colors should match the behavior classifications
    #labels = c("Boating", "Driving", "Exploring", "Walking","Stopped"), # example if you need/want to hand-code which color matches which behavior on the map
    title = "Behavioral State"
  )

# Display behavioral map
behavior_map

# Now check...did this classify things correctly?
# In the cape lookout example, Driving looks correct
# But, Walking looks like when I was stopped, and Boating is a mix of exploring and walking
# Rerun for 5 categories

# For 5 categories, its a bit closer to my actual behavior.
# Misclassified some driving speeds as boating...when i was off the highway and driving in town
# Classified exploring by foot and exploring along the boat path (when we stopped to look at dolphins) similarly
# etc.
# A good approach to see how automated behavioral classification is only as good 
# as the biological knowledge for classification and movement metrics used

# 10. Export results for report
# -----------------------------------------------

# Save processed data
write.csv(movement_metrics, "processed_movement_data.csv", row.names = FALSE)

# Print completion message
cat("Analysis complete! All visualizations have been created and data has been processed.\n")
cat("The data also contains additional metrics like Altitude, Heart Rate, etc. that we didn't use. How might this provide more context?\n")
cat("Consider how any pictures you took while you were tracking yourself might provide more context.\n")
