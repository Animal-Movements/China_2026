# Track Yourself Movement Analysis with momentuHMM
# This script analyzes GPS movement data to classify behavioral states using Hidden Markov Models
# It uses the momentuHMM package with crawl integration for irregular data

# Install required packages if needed
packages <- c("momentuHMM", "crawl", "dplyr", "ggplot2", "lubridate", "sf")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)

# Load packages
library(momentuHMM)
library(crawl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(tidyverse)
library(geosphere)

#---- PART 1: Data Preparation and Exploration ----

# Import GPS data (adjust file path as needed)
#gps_data <- read.csv("/Users/harrisonAL/Downloads/Wednesday Morning Track.csv", stringsAsFactors = FALSE)
gps_data <- read.csv("/Users/harrisonAL/Downloads/CapeLookout.csv", stringsAsFactors = FALSE)


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


# 3. Create a map of the movement path
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

# Display the map
movement_map

# 4. Regularize the data to a common time step
# -----------------------------------------------

# Define desired time interval in minutes
time_interval <- 1  # 1 minute intervals

# Create a sequence of regular timestamps
start_time <- min(gps_data$timestamp_gmt)
end_time <- max(gps_data$timestamp_gmt)
regular_times <- seq(from = start_time, 
                     to = end_time, 
                     by = paste(time_interval, "min"))

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
    radius = 5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.5
  ) 

# Display the map
movement_map_reg

#---- PART 2: Fitting Hidden Markov Models with momentuHMM ----

# ID column required by momentuHMM
regular_gps$ID = "Autumn-Lynn"
gps_data$ID = "Autumn-Lynn"

# Create a momentuHMM data object
hmm_data <- prepData(
  regular_gps,
  type = "LL",  # Latitude/Longitude
  coordNames = c("Longitude", "Latitude")  # Note the order: x, y
)

head(hmm_data) # note that it calculates the step length and turn angle for you

# Explore the distribution of your turn angles and step lengths
hist(hmm_data$angle) # looks pretty normal
hist(hmm_data$step) # looks like gamma

# Visualize the data
plot(hmm_data, ask = FALSE)

# 1. Try a 2 state model
# Typically: 
# State 1: Slow tortuous movements. Large turn angle and small step lengths
# State 2: Fast directected movements. Small turn angle and large step lengths
# In other words, when is the animal moving fast and when is the animal stopping to do something 

# Define and fit a 2-state HMM
nbStates <- 2

# These are priors based on the distributions we expect from turn angles and step lengths (gamma)
# Set up distribution parameters for step length and turning angle
# For step length (gamma distribution): c(mean1, mean2, sd1, sd2)

# Define the distributions for each data stream
# Gamma distribution for step length
# von Mises distribution for radians 
# these are recommended in the vinette
dist <- list(step = "gamma", angle = "vm")

### Fit HMM
# initial step distribution natural scale parameters
stepPar0 <- c(0.25,1,0.025,1) # (mu_1,mu_2,sd_1,sd_2)

# initial angle distribution natural scale parameters
# initial means, one expected for each state
anglePar0 <- c(0,pi,10,1) # (mean_1,mean_2,concentration_1,concentration_2)

hmm2 <- fitHMM(data = hmm_data, nbStates = 2,
                    dist = list(step = "gamma", angle = "vm"),
                    Par0 = list(step = stepPar0, angle = anglePar0),
                    formula = ~ 1,
                    estAngleMean = list(angle=TRUE))

# Examine the 2-state model
print(hmm2)
plot(hmm2, ask = FALSE)

probs = stateProbs(hmm2, hierarchical = FALSE)
probs = data.frame(probs)

# # Extract state decoding (most likely state sequence)
states <- viterbi(hmm2)
probs$state = states
#probs$state[probs$state.1>0.75] = 1
#probs$state[probs$state.2>0.75] = 2

# Convert data to sf object for spatial operations
regular_gps_m2 = cbind(regular_gps, probs)
sf_m2 <- st_as_sf(regular_gps_m2, 
                       coords = c("Longitude", "Latitude"), 
                       crs = 4326)  # WGS84 coordinate reference system

# The number of colors are generated from the number of behavior labels you have (length(behavior_labels)) 
colors <- colorRampPalette(
  RColorBrewer::brewer.pal(length(unique(probs$state)), "Dark2")
)(length(levels(factor(probs$state))))

palBehav <- leaflet::colorFactor(colors,
                                 domain = levels(factor(probs$state)))

# Create a basic interactive map
movement_map_m2 <- leaflet(sf_m2) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    radius = 5,
    color = ~palBehav(state),
    #color = "red",
    stroke = FALSE,
    fillOpacity = 0.9
  ) 

# Display the map
movement_map_m2


# -----------------------------------------------
## 3 state model
# -----------------------------------------------


# Define and fit a 3-state HMM
nbStates <- 3

# Visualize the data
plot(hmm_data, ask = FALSE)

# Step length parameters (means and SDs for gamma distribution)
# c(mean1, mean2, mean3, sd1, sd2, sd3)
stepPar0 <- c(0.25, 0.75, 1.5, 0.25, 0.75, 1.5)

# Turning angle parameters (means and concentrations for von Mises distribution)
# c(mean1, mean2, mean3, concentration1, concentration2, concentration3)
anglePar0 <- c(pi, 1.5, 0, 1, 15, 10)

hmm3 <- fitHMM(data = hmm_data, nbStates = 3,
               dist = list(step = "gamma", angle = "vm"),
               Par0 = list(step = stepPar0, angle = anglePar0),
               formula = ~ 1,
               estAngleMean = list(angle=TRUE))

# Examine the 3-state model
print(hmm3)
plot(hmm3, ask = FALSE)

probs = stateProbs(hmm3, hierarchical = FALSE)
probs = data.frame(probs)
probs$state = viterbi(hmm3)


#probs$state[probs$state.1>0.75] = 1
#probs$state[probs$state.2>0.75] = 2
#probs$state[probs$state.3>0.75] = 3

# Convert data to sf object for spatial operations
regular_gps_m3 = cbind(regular_gps, probs)
sf_m3 <- st_as_sf(regular_gps_m3, 
                  coords = c("Longitude", "Latitude"), 
                  crs = 4326)  # WGS84 coordinate reference system

# The number of colors are generated from the number of behavior labels you have (length(behavior_labels)) 
colors <- colorRampPalette(
  RColorBrewer::brewer.pal(length(unique(probs$state)), "Dark2")
)(length(levels(factor(probs$state))))

palBehav <- leaflet::colorFactor(colors,
                                 domain = levels(factor(probs$state)))

# Create a basic interactive map
movement_map_m3 <- leaflet(sf_m3) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    radius = 5,
    color = ~palBehav(state),
    #color = "red",
    stroke = FALSE,
    fillOpacity = 0.8
  ) 

# Display the map
movement_map_m3

# -----------------------------------------------
## 4 state model
# -----------------------------------------------


# Define and fit a 4-state HMM
nbStates <- 4

# Visualize the data
plot(hmm_data, ask = FALSE)


# Step length parameters (means and SDs for gamma distribution)
# c(mean1, mean2, mean3, mean4, sd1, sd2, sd3, sd4)
stepPar0 <- c(0.01, 0.2, 0.7, 1.5,0.01, 0.2, 0.7, 1.5)

# Turning angle parameters (means and concentrations for von Mises distribution)
# c(mean1, mean2, mean3, mean4, concentration1, concentration2, concentration3, concentration4)
anglePar0 <- c(pi, 2.5, 0.785, 0.1)

hmm4 <- fitHMM(
  data = hmm_data,
  dist = list(step = "gamma", angle = "vm"),
  nbStates = 4,
  #Par0 = getPar0(hmm3),
  Par0 = list(step = stepPar0, angle = c(pi, 2.5, 0.785, 0.1)),
  formula = ~1
)

### FAILED :) :) :)

# Examine the 4-state model
print(hmm4)
plot(hmm4, ask = FALSE)


probs = stateProbs(hmm3, hierarchical = FALSE)
probs = data.frame(probs)
probs$state = NA
probs$state[probs$state.1>0.75] = 1
probs$state[probs$state.2>0.75] = 2
probs$state[probs$state.3>0.75] = 3
probs$state[probs$state.4>0.75] = 4

# Convert data to sf object for spatial operations
regular_gps_m4 = cbind(regular_gps, probs)
sf_m4 <- st_as_sf(regular_gps_m4, 
                  coords = c("Longitude", "Latitude"), 
                  crs = 4326)  # WGS84 coordinate reference system

# The number of colors are generated from the number of behavior labels you have (length(behavior_labels)) 
colors <- colorRampPalette(
  RColorBrewer::brewer.pal(length(unique(probs$state)), "Dark2")
)(length(levels(factor(probs$state))))

palBehav <- leaflet::colorFactor(colors,
                                 domain = levels(factor(probs$state)))

# Create a basic interactive map
movement_map_m4 <- leaflet(sf_m4) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    radius = 5,
    color = ~palBehav(state),
    #color = "red",
    stroke = FALSE,
    fillOpacity = 0.8
  ) 

# Display the map
movement_map_m4



#---- PART 4: Model Selection and Evaluation ----

# Create a data frame with model comparison metrics
model_comparison <- data.frame(
  Model = c("2-state", "3-state"),
  AIC = c(AIC(hmm2), AIC(hmm3)),
  #BIC = c(BIC(hmm2), BIC(hmm3)),
  NLL = c(-hmm2$mod$minimum, -hmm3$mod$minimum)
)

# Print the model comparison table
print(model_comparison)

# Identify the best model based on AIC
best_model_aic <- model_comparison[which.min(model_comparison$AIC), "Model"]
cat("Best model based on AIC:", best_model_aic, "\n")

# Identify the best model based on BIC
#best_model_bic <- model_comparison[which.min(model_comparison$BIC), "Model"]
#cat("Best model based on BIC:", best_model_bic, "\n")

#---- PART 5: Interpreting the Best Model ----
# We'll assume the 3-state model is selected for this example
# But you should use the model with the lowest AIC or BIC

# Extract state decoding (most likely state sequence)
states <- viterbi(hmm3)

# Get state probabilities
state_probs <- stateProbs(hmm3)

# Add state and probabilities to the regularized data
results_data <- cbind(
  regularData,
  state = states,
  state_prob1 = state_probs[,1],
  state_prob2 = state_probs[,2],
  state_prob3 = state_probs[,3]
)

# Examine state-dependent distributions
plot(hmm3, plotCI = TRUE, ask = FALSE)

# Get transition probabilities
print("Transition probability matrix:")
print(hmm3$mle$gamma)

# Get stationary state probabilities
print("Stationary state probabilities:")
print(hmm3$mle$delta)

# Get state-dependent distribution parameters
print("State-dependent distribution parameters:")
print(hmm3$mle$step)
print(hmm3$mle$angle)

#---- PART 6: Visualize Results on Map ----

# Create an sf object for spatial plotting
results_sf <- st_as_sf(results_data, coords = c("x", "y"), crs = 4326)

# Define colors for each state
state_colors <- c("1" = "blue", "2" = "green", "3" = "red")

# Plot the path colored by state
state_map <- ggplot() +
  geom_sf(data = results_sf, aes(color = factor(state)), size = 1) +
  scale_color_manual(values = state_colors, name = "Behavioral State",
                     labels = c("Resting", "Foraging", "Traveling")) +
  theme_minimal() +
  labs(title = "GPS Track with Classified Behavioral States",
       subtitle = "Hidden Markov Model Results") +
  theme(legend.position = "bottom")

print(state_map)

# Plot state transitions over time
state_timeline <- ggplot(results_data, aes(x = time, y = factor(state), color = factor(state))) +
  geom_point() +
  scale_color_manual(values = state_colors, name = "Behavioral State",
                     labels = c("Resting", "Foraging", "Traveling")) +
  theme_minimal() +
  labs(title = "Behavioral State Transitions Over Time",
       x = "Time", y = "State") +
  theme(legend.position = "bottom")

print(state_timeline)

# Create dwell time plots (time spent in each state)
state_summary <- results_data %>%
  group_by(state) %>%
  summarise(
    count = n(),
    proportion = n() / nrow(results_data),
    mean_speed = mean(speed, na.rm = TRUE)
  )

print(state_summary)

# Visualize state proportions
state_props <- ggplot(state_summary, aes(x = factor(state), y = proportion, fill = factor(state))) +
  geom_col() +
  scale_fill_manual(values = state_colors, name = "Behavioral State",
                   labels = c("Resting", "Foraging", "Traveling")) +
  theme_minimal() +
  labs(title = "Proportion of Time in Each Behavioral State",
       x = "State", y = "Proportion") +
  theme(legend.position = "bottom")

print(state_props)

#---- PART 7: Create Final Dataset ----

# Merge state classifications back to original data
# First, create a function to find the nearest time point
find_nearest_time <- function(time_point, time_vector) {
  which.min(abs(difftime(time_point, time_vector, units = "secs")))
}

# Apply function to each time point in original data
gps_data$nearest_idx <- sapply(gps_data$timestamp_gmt, 
                               find_nearest_time, 
                               results_data$time)

# Add state and probability columns to original data
gps_data$state <- results_data$state[gps_data$nearest_idx]
gps_data$state_prob1 <- results_data$state_prob1[gps_data$nearest_idx]
gps_data$state_prob2 <- results_data$state_prob2[gps_data$nearest_idx]
gps_data$state_prob3 <- results_data$state_prob3[gps_data$nearest_idx]

# Rename states with behavioral labels
gps_data$behavior <- factor(gps_data$state, 
                            levels = c(1, 2, 3),
                            labels = c("Resting", "Foraging", "Traveling"))

# Verify the final data structure
str(gps_data)
head(gps_data[, c("timestamp_gmt", "Latitude", "Longitude", "Speed.m.s.", "state", "behavior", 
                  "state_prob1", "state_prob2", "state_prob3")])

# Calculate summary statistics by behavioral state
behavior_summary <- gps_data %>%
  group_by(behavior) %>%
  summarise(
    count = n(),
    proportion = n() / nrow(gps_data),
    mean_speed = mean(Speed.m.s., na.rm = TRUE),
    mean_heart_rate = mean(Heart.Rate..bpm., na.rm = TRUE),
    mean_glide_ratio = mean(Glide.Ratio, na.rm = TRUE)
  )

print(behavior_summary)

# Save the final dataset
write.csv(gps_data, "CapeLookout_with_states.csv", row.names = FALSE)

# Print completion message
cat("\nAnalysis complete! The classified dataset has been saved as 'CapeLookout_with_states.csv'.\n")
