## ----setup, include=FALSE---------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----load-pacakages, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------
# Remove items from memory/clean your workspace
rm(list=ls())

# You may need to install these packages first
#install.packages('momentuHMM', 'lubridate', 'tidyverse')

# Load libraries
library(momentuHMM)
library(lubridate)
library(tidyverse)
library(adehabitatLT)

# Load functions for later
source("utility_functions.R")


## ----load-data, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------
# Load data 

## ID: 32733 - pacoca
pacoca <- read.csv("data/32733_PACOCA/2022_pacoca_ACC.csv")

## ID: 32866 - juba
juba <- read.csv("data/32866_JUBA/2022_juba_ACC.csv")


## ----time-conversion, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------
## create ID for each dataframe 
pacoca$ID <- "32733"
juba$ID <- "32866"

## create year column for each dataframe 
pacoca$year <- "2022"
juba$year <- "2022"

## currently, the data contains several date/time columns -- year and date and time need to combine into one column first 
## combine year, date, and time
pacoca$datetime_str <- paste(pacoca$year,
                             pacoca$UTC_Date,
                             pacoca$UTC_Time)

juba$datetime_str <- paste(juba$year,
                           juba$UTC_Date,
                           juba$UTC_Time)

## convert to POSIXct
pacoca$time <- lubridate::parse_date_time(
  pacoca$datetime_str,
  orders = "Y d-b HMS",
  tz = "UTC"
)

juba$time <- lubridate::parse_date_time(
  juba$datetime_str,
  orders = "Y d-b HMS",
  tz = "UTC"
)

## only select columns that will be used for this study 
pacoca <- pacoca %>% dplyr::select(ID, time, x, y, z, temp)
juba <- juba %>% dplyr::select(ID, time, x, y, z, temp)


## ----diff-time, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------
# Table of time intervals in data - can apply to both individuals
plot(table(diff(juba$time)), xlim = c(0, 200000),
     xlab = "time interval (sec)", ylab = "count")

plot(table(diff(pacoca$time)), xlim = c(0, 200000),
     xlab = "time interval (sec)", ylab = "count")


## ----identify gaps, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------------
diff_pacoca <- as.numeric(diff(pacoca$time), units = "secs")
diff_pacoca[diff_pacoca > 2]

diff_juba <- as.numeric(diff(juba$time), units = "secs")
diff_juba[diff_juba > 2]


## ----utility gaps, message=FALSE, warning=FALSE, echo=TRUE------------------------------------------
# Use function from utility_function.R to split data at gaps > 30 minutes
data_split_juba <- split_at_gap(data = juba, max_gap = 30, shortest_track = 0)
data_split_pacoca <- split_at_gap(data = pacoca, max_gap = 30, shortest_track = 0)


## ----ODBA, message=FALSE, warning=FALSE, echo=TRUE--------------------------------------------------
## use a single summary activity variable - ODBA
data_split_pacoca$ODBA <-
  rowSums(abs(data_split_pacoca[, c("x", "y", "z")]))

data_split_juba$ODBA <-
  rowSums(abs(data_split_juba[, c("x", "y", "z")]))


## ----fill NAs, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------
## Function to pad each split track to a regular 2-second time series
pad_time_series <- function(df, interval = 2){

  ## create regular timestamp sequence
  full_time <- data.frame(
    time = seq(
      from = min(df$time),
      to   = max(df$time),
      by   = interval
    )
  )

  ## merge observations onto regular timeline
  full_data <- dplyr::left_join(
    full_time,
    df,
    by = "time"
  )

  ## restore ID for inserted NA rows
  full_data$ID <- unique(df$ID)

  ## arrange columns
  full_data <- full_data %>%
    dplyr::select(ID, time, x, y, z, temp, ODBA)

  return(full_data)
}


## Apply padding separately to each split track
pacoca_regular <- data_split_pacoca %>%
  group_split(ID) %>%
  purrr::map_dfr(pad_time_series)

juba_regular <- data_split_juba %>%
  group_split(ID) %>%
  purrr::map_dfr(pad_time_series)


## ----final-data, message=FALSE, warning=FALSE, echo=TRUE--------------------------------------------
# select columns of interest
pacoca <- pacoca_regular %>%
  select(ID, ODBA, time, temp)

juba <- juba_regular %>%
  select(ID, ODBA, time, temp)

# combine datasets
data <- rbind(pacoca, juba)

# check the data summary
summary(data)


## ----prep-data1, message=FALSE, warning=FALSE, echo=TRUE--------------------------------------------
# Prepare data for HMM 
data_hmm <- prepData(data, coordNames = NULL, covNames = "temp")

data_hmm


## ----fit-hmm1, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------
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
# Missing ODBA observations from the regularized time series remain as NA
# and are handled internally by momentuHMM.
hmm1 <- fitHMM(data_hmm, 
               nbStates = 2, 
               dist = dist, 
               Par0 = Par0_2s)

# Print parameter estimates 
hmm1


## ----look-hmm1, fig.keep = 1:4----------------------------------------------------------------------
# Plot estimated distributions and state-coloured tracks
plot(hmm1, ask = FALSE)


## ----viterbi, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------------------
# get most likely sequence of states 
head(viterbi(hmm1))

# save most likely state sequence from 2-state model
data_hmm$state_2st <- factor(viterbi(hmm1))

# plot OBDA over time, colored by states
ggplot(data_hmm, aes(x = time, y = ODBA, col = state_2st, group = ID))+
  geom_point(size = 0.5, alpha = 0.2)+
  facet_wrap(~ID)


## ----fit-hmm2, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------
# Fit 2-state HMM with temperature covariate 
hmm2 <- fitHMM(data_hmm, 
               nbStates = 2, 
               dist = dist, 
               Par0 = Par0_2s, 
               formula = ~ temp)

# show summary of model 2 
hmm2


## ----look-hmm2--------------------------------------------------------------------------------------
# plot stationary state probabilities as functions of temperature 
plotStationary(hmm2, 
               plotCI = TRUE)


## ----AIC--------------------------------------------------------------------------------------------
# Compare models using AIC 
AIC(hmm1, 
    hmm2)

