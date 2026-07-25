## ----setup, include=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----load-pacakages, message=FALSE, warning=FALSE, echo=TRUE-----------------------------
# Remove items from memory/clean your workspace
rm(list=ls())

# You may need to install these packages first
#install.packages('momentuHMM', 'lubridate', 'tidyverse')

# Load libraries
library(momentuHMM)
library(lubridate)
library(tidyverse)


## ----load-data, message=FALSE, warning=FALSE, echo=TRUE----------------------------------
# Load data 

## ID: 32733 - pacoca
act_32733 <- read.csv("data/ACT_Collar32733_20260401143616.csv")

## ID: 32866 - juba
act_32866 <- read.csv("data/ACT_Collar32866_20260401143833.csv")


## ----time-conversion, message=FALSE, warning=FALSE, echo=TRUE----------------------------
## currently, the data contains several date/time columns -- date and time need to combine into one column first 
act_32733$datetime_str <- paste(act_32733$UTC_Date, act_32733$UTC_Time)
act_32866$datetime_str <- paste(act_32866$UTC_Date, act_32866$UTC_Time)

## convert to POSIXct 
act_32733$time <- lubridate::mdy_hms(act_32733$datetime_str, tz = "UTC")
act_32866$time <- lubridate::mdy_hms(act_32866$datetime_str, tz = "UTC")


## ----diff-time, message=FALSE, warning=FALSE, echo=TRUE----------------------------------
# Table of time intervals in data - can apply to both individuals
plot(table(diff(act_32866$time)), xlim = c(0, 600),
     xlab = "time interval (sec)", ylab = "count")


## ----ODBA, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------
## use a single summary activity variable - ODBA
act_32733$ODBA <- rowSums(act_32733[, c("ActivityX", "ActivityY", "ActivityZ")], na.rm= TRUE)
act_32866$ODBA <- rowSums(act_32866[, c("ActivityX", "ActivityY", "ActivityZ")], na.rm = TRUE)


## ----final-data, message=FALSE, warning=FALSE, echo=TRUE---------------------------------
# select the columns of interest
pacoca <- act_32733 %>% 
  select(ID = CollarID, 
         ODBA, 
         time, 
         temp = Temp...C.)

juba <- act_32866 %>% 
  select(ID = CollarID, 
         ODBA, 
         time, 
         temp = Temp...C.)

# combine the datasets
data <- rbind(pacoca, juba)

# Summarize
summary(data)


## ----prep-data1, message=FALSE, warning=FALSE, echo=TRUE---------------------------------
# Prepare data for HMM 
data_hmm <- prepData(data, coordNames = NULL, covNames = "temp")


## ----fit-hmm1, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------
# Observation distributions (ODBA)
dist <- list(ODBA = "gamma")

# Initial parameters
# Use simple rules-of-thumb based on the data:
# - Low activity state (state 1): roughly half the mean ODBA
# - High activity state (state 2): roughly 1.5 times the mean ODBA
# The 1e-2 ensures strictly positive values.  Not necessary here, but might save you some problems.
mu1 <- max(mean(data$ODBA) * 0.5, 1e-2)  # state 1: low activity 
mu2 <- max(mean(data$ODBA) * 1.5, 1e-2)  # state 2: high activity

# Use the standard deviation of the positive observations as a guide.
# Ensure strictly positive values (1e-2) to satisfy momentuHMM bounds.
sd1 <- max(sd(data$ODBA) * 0.5, 1e-2)
sd2 <- max(sd(data$ODBA) * 1.5, 1e-2)

# Zero-inflation: probability of observing exact zeros
# Can estimate from data, or use small non-zero values as starting points -- Must be strictly < 1
zm1 <- 0.01
zm2 <- 0.01

# Combine into Par0
# The order is important: c(mu1, mu2, sd1, sd2, zm1, zm2)
Par0_2s <- list(
  ODBA = c(mu1, mu2, sd1, sd2, zm1, zm2)
)

# Fit a 2-state HMM
hmm1 <- fitHMM(data_hmm, 
               nbStates = 2, 
               dist = dist, 
               Par0 = Par0_2s)

# Print parameter estimates 
hmm1


## ----look-hmm1, fig.keep = 1:4-----------------------------------------------------------
# Plot estimated distributions and state-coloured tracks
plot(hmm1, ask = FALSE)


## ----viterbi, message=FALSE, warning=FALSE, echo=TRUE------------------------------------
# get most likely sequence of states 
head(viterbi(hmm1))

# save most likely state sequence from 2-state model
data_hmm$state_2st <- factor(viterbi(hmm1))

# plot OBDA over time, colored by states
ggplot(data_hmm, aes(x = time, y = ODBA, col = state_2st, group = ID))+
  geom_point(size = 0.5, alpha = 0.2)+
  facet_wrap(~ID)


## ----fit-hmm2, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------
# Fit 2-state HMM with temperature covariate 
hmm2 <- fitHMM(data_hmm, 
               nbStates = 2, 
               dist = dist, 
               Par0 = Par0_2s, 
               formula = ~ temp)

# show summary of model 2 
hmm2


## ----look-hmm2---------------------------------------------------------------------------
# plot stationary state probabilities as functions of temperature 
plotStationary(hmm2, 
               plotCI = TRUE)


## ----AIC---------------------------------------------------------------------------------
# Compare models using AIC 
AIC(hmm1, 
    hmm2)


## ----pseudo-res--------------------------------------------------------------------------
# plot pseudo-residuals for 2-state models 
plotPR(hmm1)
plotPR(hmm2)

