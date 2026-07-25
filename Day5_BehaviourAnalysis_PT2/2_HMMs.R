## ----setup, include=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----Setup, message=FALSE, warning=FALSE, echo=TRUE--------------------------------------
# Remove items from memory/clean your workspace
rm(list=ls())

# You may need to install these packages first
#install.packages('momentuHMM', 'lubridate', 'tidyverse')

# Load libraries
library(momentuHMM)
library(lubridate)
library(tidyverse)


## ----Load, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------
# Read the dataset into R, selecting the necessary columns (x, y, date, id) for analyses.
# We will also grab the temperature column, convert the x/y values to km (e.g., x/1000), and create an hour field. 
# We many also want to summarize results by the sex of each animal.  Since sex is not included in our dataset, we can join this information from a reference table that we will import.

# Load dataset
load("Data/WB_3h_resampled.rdata")

# Read in Reference dataset
WB.ref <- read_csv("data/WB_ref.csv")

# Here, we'll do a little cleaning of the reference dataset, join to the 3h tracking dataset, and prepare the file for further analysis.
WB.data <- WB.ref %>%
  #filter(study_site == "Athi-Kaputiei Plains") %>%
  mutate(id = as.factor(as.character(individual_local_identifier)), # Make format the same as trk_resampled_3h field
         sex = as.numeric(as.factor(sex))) %>%
  select(
    id,
    sex
  ) %>%
  right_join( # Keep all from trk_resampled_3h
    trk_resampled_3h,
    by = join_by(id)
  ) %>%
  # Now, arrange the dataframe, manipulate the columns, and select the fields of interest
  arrange(id, t_) %>% # Not really necessary, but always good practice to make sure the data are in correct order
  select(x = x_,
         y = y_,
         t = t_,
         ID = id,
         temp,
         sex) %>% 
  mutate(hour = hour(t),
         x = x/1000, 
         y = y/1000, 
         temp = as.numeric(temp)
         ) %>%
  # Make the file a dataframe
  as.data.frame()

# Clean up your environment
rm(trk_resampled_3h, WB.ref)


## ----Create, message=FALSE, warning=FALSE, echo=TRUE-------------------------------------
# Create Object
WB.move <- WB.data  %>% 
  prepData(type = "UTM",
           # Specify coordinate names
           coordNames = c("x","y"), # Note the order: x, y
           covNames = c("temp", "hour"))

# What's the class of the object?
class(WB.move)


## ----Move Summary, message=FALSE, warning=FALSE, echo=TRUE-------------------------------
# Summarize the object
summary(WB.move)

# What are the column headings included in this move object?
names(WB.move)

# We can plot all animals together.  Please try!
# plot(WB.move,
#      compact = TRUE, # Make sure you specify compact = TRUE.  If you don't specify, each plot will be drawn separately, which can be tedious
#      ask = FALSE)

# Or, each animal separately
# unique(WB.move$ID) # We can always query the dataframe for the names of each animal.
plot(WB.move[WB.move$ID == "Kikaya",],
     ask = FALSE)

# We can also investigate the steplength and turning separately, using standard R commands
# Note that these summaries are for all animals together
# hist(WB.move$step)
# summary(WB.move$step)
# quantile(WB.move$step,
#          probs = 0.90,
#          na.rm = TRUE)
# hist(WB.move$angle)


## ----Zero Mass, message=FALSE, warning=FALSE, echo=TRUE----------------------------------
# Let's first determine if we have any step lengths of 0.  If yes, we need to include a zero mass parameter.  If no, setting a zeromass value is not necessary.

# The slice_min() command allows us to view the 10 lowest values of the steplength parameter.  It's a convenient function to order by the minimum steplengths.
slice_min(WB.move,
          order_by = step,
          n = 10)

# You could also query the dataset and summarize numerically 
whichzero <- which(WB.move$step == 0) 
# Calculate the proportion of steps of length zero 
(prop.0 <- length(whichzero)/nrow(WB.move))


## ----Start, message=FALSE, warning=FALSE, echo=TRUE--------------------------------------
# Check distributions
hist(WB.move$step, xlab="Step Length") 
hist(WB.move$angle, xlab="Turning Angle") 

# Define Starting Values
# ************************************
# For Step Length (gamma distribution): c(mean1, mean2, sd1, sd2, zeromass1, zeromass2)
# Mean
mu0 <- c(0.1, 1)

# SD
sigma0 <- c(1, 1)

# Zero Mass
zeromass0 <- c(prop.0, prop.0)

# Combine together
stepPar0 <- c(mu0, sigma0, zeromass0)

# For Turning Angle (von mises distribution): c(mean1, mean2, concentration1, concentration2)
# In radians, pi, or 3.14 represents 180 degrees.
# We expect the concentration to be larger when directed movement (mean = 2) occurs
anglePar0 <- c(pi, 0, 1, 10)


## ----Fitting, message=FALSE, warning=FALSE, echo=TRUE------------------------------------
# Fit NULL model
WB.null <- fitHMM(data = WB.move, 
                  nbStates = 2,
                  dist = list(step = "gamma", angle = "vm"), # Need to add these as a list
                  Par0 = list(step = stepPar0, angle = anglePar0), # Need to add these as a list
                  formula = ~ 1,
                  estAngleMean = list(angle=TRUE))

# Examining the 2-state model
WB.null


## ----Fitting Plots, message=FALSE, warning=FALSE, echo=TRUE------------------------------
# Plot the results of the predictions.  
# Colored states (State 1 is orange; state 2 is blue) provide the predicted state in each trajectory.  
# Plot all animals
# plot(WB.null,
#      ask = F)

# Plot individual animals
plot(WB.null,
     animals = "Kiranto",
     ask = FALSE)


## ----Viterbi, message=FALSE, warning=FALSE, echo=TRUE------------------------------------
# Run the algorithm on the fitted model
WB.states <- viterbi(WB.null)

# Look at the state assignments.  The result is just a simple vector of state assignments (class 1 or 2). 
WB.states[1:25]

# What's the proportion of time spent in each state?
prop.table(table(WB.states))

# How does this differ between individuals?
# To answer this question, we need to combine the state assignments with the move object
WB.v.Props <- WB.move %>% 
  mutate(state = WB.states) %>%   # create state column from WB.states (merge this with the dataframe)
  # add new column that is the total locations for each animal...used to calculate percentages
  mutate(locs = n(),
         .by = ID) %>% 
  # summarize for each animal, and each state, the proportion of locations.  Using the reframe() command here
  reframe(stateProp = n()/locs,
          sex = unique(sex),
          .by = c(ID, state)) %>%
  
  # Reduce dataset to one with just the unique rows of information.
  distinct() %>% 
  arrange(ID, state)

# Graph results, color by sex to look for any potential patterns.  Just a graph summary.
WB.v.Props %>%
  filter(state == 1) %>% 
  mutate(ID = fct_reorder(ID, 
                          stateProp)) %>% 
  ggplot(aes(y = stateProp,
             x = ID, 
             fill = sex)) +
  geom_col(col = "black",
           position = position_dodge()) +
  coord_flip() +
  labs(y = "Prop. time in State 1 (foraging/encamped)") +
  theme_bw()

# Look at Kiranto
plot(WB.move[WB.move$ID == "Kiranto",],
     ask = FALSE)
# Interestingly, Kiranto made some long distance movements, although most of his time was spent in state 1

# How does his movement compare with Paita, for example?
# plot(WB.move[WB.move$ID == "Paita",],
#      ask = FALSE)
# A very different movement pattern, even though the amount of time in state 1 is very similar to Kiranto.


## ----State Probabilities, message=FALSE, warning=FALSE, echo=TRUE------------------------
# Calculate state probabilities
WB.probs <- stateProbs(WB.null)
# head(WB.probs)

# Visualize the state sequences for 1 animal
plotStates(WB.null,
          animals = "Kiranto",
          ask = FALSE)

# We can use the built-in plot functions (see help(plot.momentuHMM)) or create our own plots
WB.move %>% 
  mutate(state = as.factor(WB.states)) %>%
  filter(ID == "Kiranto") %>% 
  ggplot(aes(x = x*1000,
             y = y*1000,
             col = state,
             fill = state)) +
  geom_path(alpha = 0.5) +
  geom_point(shape = 21,
             alpha = 0.8,
             col = "black") +
  scale_fill_manual(values = c("orange",
                               "cornflowerblue")) +
  scale_color_manual(values = c("orange",
                                "cornflowerblue")) +
  theme_classic() +
  labs(x = "Easting",
       y = "Northing",
       title = "Kiranto")



## ----Temp Model, message=FALSE, warning=FALSE, echo=TRUE---------------------------------
# Fit covariate model - temperature
WB.temp <- fitHMM(data = WB.move, 
                  nbStates = 2, 
                  dist = list(step = "gamma", angle = "vm"),
                  Par0 = list(step = stepPar0, angle = anglePar0), 
                  formula = ~ temp,
                  estAngleMean = list(angle=TRUE))

# Show model summary
WB.temp

# Built-in plotting function to evaluate the impacts of a covariate
plotStationary(WB.temp,
               plotCI = TRUE)


## ----Time Model, message=FALSE, warning=FALSE, echo=TRUE---------------------------------
# Fit covariate model
WB.tod.2state <- fitHMM(data = WB.move,
                 nbStates = 2,
                 dist = list(step = "gamma", angle = "vm"),
                 Par0 = list(step = stepPar0, angle = anglePar0), 
                 formula = ~ cosinor(hour, period = 24),
                 estAngleMean = list(angle=TRUE))

# Summarize
WB.tod.2state


## ----MultiState, message=FALSE, warning=FALSE, echo=TRUE---------------------------------
# Starting Values - Steplengths
# *****************************
# For Step Length (gamma distribution): c(mean1, mean2, sd1, sd2, zeromass1, zeromass2, zeromass3)
# Mean
mu0 <- c(0.1, 1, 3)

# SD
sigma0 <- c(1, 1, 1)

# Zero Mass
zeromass0 <- c(prop.0, prop.0, prop.0)

# Combine together
stepPar0 <- c(mu0, sigma0, zeromass0)

# Starting values - Turning angles
# ********************************
# For turning angle (von mises): c(mean1, mean2, mean3, conc1, conc2, conc3)
anglePar0 <- c(pi, 0, 1.5, 1, 10, 5)

# Fit model
WB.tod.3state <- fitHMM(data = WB.move,
                 nbStates = 3,
                 dist = list(step = "gamma", angle = "vm"),
                 Par0 = list(step = stepPar0, angle = anglePar0), 
                 formula = ~ cosinor(hour, period = 24),
                 estAngleMean = list(angle=TRUE))

# Summarize
WB.tod.3state


## ----Model Comparison, message=FALSE, warning=FALSE, echo=TRUE---------------------------
# Which of these two models is a better fit to the data?
# Results indicate that the 2 state cosinor model is the best
AIC(WB.null,
    WB.temp,
    WB.tod.2state,
    WB.tod.3state)

WB.tod.2state

# Plot the results
plot(WB.tod.2state,
     ask = FALSE)

# Plot an individual
plot(WB.tod.2state,
      animals = "Kiranto",
      ask = FALSE)


## ----Applications, message=FALSE, warning=FALSE, echo=TRUE-------------------------------
# Encode the behaviors using the Viterbi algorithm
WB.tod.states <- viterbi(WB.tod.2state)

# add to the dataset and remove some extra fiels
WB.states <- WB.move %>% 
  mutate(state = as.factor(WB.tod.states)) %>% 
  # Remove some fields to make dataset smaller
  select(-c(step,angle,temp,hour))

# Create plot of Resident behaviors (State 1) for 1 individual
WB.State1 <- WB.states %>% 
  filter(state == 1 & ID == "Sawani") %>% 
  ggplot(aes(x = x*1000,
             y = y*1000,
             col = state,
             fill = state)) +
  #geom_path(alpha = 0.5) +
  geom_point(shape = 21,
             alpha = 0.8,
             col = "black",
             show.legend = FALSE) +
  scale_color_manual(values = "orange") +
  scale_fill_manual(values = "orange") +
  theme_classic() +
  labs(x = "x",
       y = "y",
       title = "Sawani - Resident Only")

# Create plot of exploratory behaviors (State 2) for 1 individual
WB.State2 <- WB.states %>% 
  filter(state == 2 & ID == "Sawani") %>% 
  ggplot(aes(x = x*1000,
             y = y*1000,
             col = state,
             fill = state)) +
  #geom_path(alpha = 0.5) +
  geom_point(shape = 21,
             alpha = 0.8,
             col = "black",
             show.legend = FALSE) +
  scale_color_manual(values = "cornflowerblue") +
  scale_fill_manual(values = "cornflowerblue") +
  theme_classic() +
  labs(x = "x",
       y = "y",
       title = "Sawani - Exploratory Only")

# Use the cowplot package to combine these plots next to each other.
two_plots <- cowplot::plot_grid(WB.State1,
                                WB.State2,
                                ncol = 2,
                                rel_widths = c(1, 1))

# Show the plots
two_plots

# Save the plots
ggsave(plot = two_plots,
       filename = "Output/sawani_residencelocations.tiff",
       units = "in",
       width = 6.5,
       height = 4)

