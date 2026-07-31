## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----load-pacakages, message=FALSE, warning=FALSE, echo=TRUE-------------------
# Remove items from memory/clean your workspace
rm(list=ls())

# You may need to install these packages first
#install.packages('momentuHMM', 'lubridate', 'tidyverse', `dplyr`, `stringr`)

# Load libraries
library(momentuHMM)
library(lubridate)
library(tidyverse)
library(dplyr)
library(stringr)

# Load functions for later
source("utility_functions.R")


## ----load-data, message=FALSE, warning=FALSE, echo=TRUE------------------------
# Load data 
# ID: 32733 - pacoca
pacoca <- read_csv("data/2022_pacoca_ACC.csv")

# ID: 32866 - juba
juba <- read_csv("data/2022_juba_ACC.csv")


## ----time-conversion, message=FALSE, warning=FALSE, echo=TRUE------------------
# Create ID for each animal
pacoca$ID <- "32733"
juba$ID <- "32866"

# Bind files together and create time field
data <- rbind(pacoca, juba)

# Create time field from available information in separate columns 
data <- data %>% 
  mutate(year = "2022",
         datetime = paste(year,
                          UTC_Date,
                          UTC_Time),
         time = parse_date_time(datetime,
                                 orders = "Y d-b HMS",
                                 tz = "UTC")) %>%
  dplyr::select(ID, time, x, y, z, temp)

# Remove unnecessary files
rm(pacoca, juba)


## ----diff-time, message=FALSE, warning=FALSE, echo=TRUE------------------------
# Table of time intervals in data
plot(table(diff(data$time)), xlim = c(0, 200000),
     xlab = "time interval (sec)", ylab = "count")

# Look at time gaps
diff_data <- as.numeric(diff(data$time), units = "secs")
diff_data[diff_data > 2]


## ----utility gaps, message=FALSE, warning=FALSE, echo=TRUE---------------------
# Use function from utility_function.R to split data at gaps > 30 minutes
data <- split_at_gap(data, max_gap = 30, shortest_track = 0)


## ----fill NAs, message=FALSE, warning=FALSE, echo=TRUE-------------------------
# Function to pad each split track to a regular 2-second time series
pad_time_series <- function(df, interval = 2){
  # Create a full (regular) time sequence for the entire tracking period (min and max)
  full_time <- data.frame(
    time = seq(
      from = min(df$time),
      to   = max(df$time),
      by   = interval
    )
  )

  # Merge observations together.
  # This will automatically put a NA in the time series where a record previously did not exist
  full_data <- left_join(
    full_time,
    df,
    by = "time"
  )

  # restore ID for inserted NA rows
  full_data$ID <- unique(df$ID)

  return(full_data)
}

# Apply padding to each track, split by the ID we created.
data <- data %>%
  group_split(ID) %>%
  map_dfr(pad_time_series)

# check the data summary
summary(data)


## ----ODBA, message=FALSE, warning=FALSE, echo=TRUE-----------------------------
## use a single summary activity variable - ODBA
data$ODBA <- rowSums(abs(data[, c("x", "y", "z")]))

# Clean dataframe and correct the ID field, removing the burst IDs
data <- data %>%
  # Remove the old ID
  mutate(ID = str_remove(ID, "-.*")) %>%
  dplyr::select(ID, time, temp, ODBA)

# CLean up unneeded objects
rm(diff_data)

# Plot
plot.ODBA <- ggplot(data, aes(x = time, y = ODBA, color = ID)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ ID) +
  theme_minimal() +
  labs(
    x = "Time",
    y = "ODBA",
    title = "ODBA over time by Individual"
  )

# Print to view the plot
plot.ODBA


## ----prep-data1, message=FALSE, warning=FALSE, echo=TRUE-----------------------
# Prepare data for HMM 
data_hmm <- prepData(data, coordNames = NULL, covNames = "temp")
data_hmm


## ----fit-hmm1, message=FALSE, warning=FALSE, echo=TRUE-------------------------
# Observation distribution for ODBA
# ODBA is continuous and strictly positive, so a gamma distribution is appropriate.
# Missing ODBA values (created during temporal regularization) are retained in data_hmm
# and handled by momentuHMM.
dist <- list(ODBA = "gamma")

# Initial parameter values (Par0)
# Remove only missing values temporarily for calculating starting parameters.
# The original NA values remain in data_hmm for HMM fitting.
ODBA_obs <- data$ODBA[!is.na(data$ODBA)]

# Define initial means for the two activity states
# State 1: low-activity state
# Initialized using the lower half of observed ODBA values.
mu1 <- mean(ODBA_obs[ODBA_obs <= median(ODBA_obs)])  

# State 2: high-activity state
# Initialized using the upper quartile of observed ODBA values.
mu2 <- mean(ODBA_obs[ODBA_obs >= quantile(ODBA_obs, 0.75)])

# Define initial standard deviations for each state
# These values describe the expected variability within each activity state.
# Small positive values are required because gamma parameters cannot be zero.
sd1 <- sd(ODBA_obs[ODBA_obs <= median(ODBA_obs)])
sd2 <- sd(ODBA_obs[ODBA_obs >= quantile(ODBA_obs, 0.75)])

# Combine into Par0
# The order is important: c(mu1, mu2, sd1, sd2)
Par0_2s <- list(
  ODBA = c(mu1, mu2, sd1, sd2)
)

# Fit a 2-state HMM
# Missing ODBA observations from the regularized time series remain as NA and are handled internally by momentuHMM.
hmm1 <- fitHMM(data_hmm, 
               nbStates = 2, 
               dist = dist, 
               Par0 = Par0_2s)

# Print parameter estimates 
hmm1


## ----look-hmm1, fig.keep = 1:4-------------------------------------------------
# Plot estimated distributions and state-coloured tracks
plot(hmm1, ask = FALSE)


## ----viterbi, message=FALSE, warning=FALSE, echo=TRUE--------------------------
# get most likely sequence of states 
head(viterbi(hmm1))

# save most likely state sequence from 2-state model
data_hmm$state_2st <- factor(viterbi(hmm1))

# plot OBDA over time, colored by states
ggplot(data_hmm, aes(x = time, y = ODBA, col = state_2st, group = ID))+
  geom_point(size = 0.5, alpha = 0.2)+
  facet_wrap(~ID)


## ----fit-hmm2, message=FALSE, warning=FALSE, echo=TRUE-------------------------
# Fit 2-state HMM with temperature covariate 
hmm2 <- fitHMM(data_hmm, 
               nbStates = 2, 
               dist = dist, 
               Par0 = Par0_2s, 
               formula = ~ temp)

# show summary of model 2 
hmm2


## ----look-hmm2-----------------------------------------------------------------
# plot stationary state probabilities as functions of temperature 
plotStationary(hmm2, 
               plotCI = TRUE)


## ----AIC-----------------------------------------------------------------------
# Compare models using AIC 
AIC(hmm1, 
    hmm2)

