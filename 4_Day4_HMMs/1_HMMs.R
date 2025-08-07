## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Setup, message=FALSE, warning=FALSE, echo=TRUE--------------------
# Remove items from memory/clean your workspace
rm(list=ls())

# You may need to install these packages first
#install.packages('momentuHMM', 'lubridate', 'tidyverse')

# Load libraries
library(momentuHMM)
library(lubridate)
library(tidyverse)


## ----Load, message=FALSE, warning=FALSE, echo=TRUE---------------------
# Read the dataset into R, selecting the necessary columns (x, y, date, id) for analyses.
# We will also grab the temperature column and the sex of each animal and convert the x/y values to km (e.g., x/1000). 
# We'll also create an hour field from the timestamp and make sex a numeric variable using the mutate() function. 

# Data load/import
load("Data/wildebeest_3hr_data.rdata")

# Process the movement dataset.  We'll use the dataset created from adehabitat
WB.data <- WB.move %>%
  arrange(id, date) %>% # Not necessary here, but good practice
  select(x,
         y,
         t = date,
         ID = id,
         temp,
         sex) %>% 
  mutate(hour = hour(t),
         x = x/1000, 
         y = y/1000, 
         sex = as.numeric(sex))


## ----Create, message=FALSE, warning=FALSE, echo=TRUE-------------------
# Create Object
WB.move <- WB.data  %>% 
  prepData(type = "UTM",
           # Specify coordinate names
           coordNames = c("x","y"), # Note the order: x, y
           covNames = c("temp", "sex", "hour"))

# What's the class of the object?
class(WB.move)


## ----Move Summary, message=FALSE, warning=FALSE, echo=TRUE-------------
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
# Note, however, the way these are writtent, they are summarizing all animals together
# hist(WB.move$step)
# summary(WB.move$step)
# quantile(WB.move$step,
#          probs = 0.90,
#          na.rm = TRUE)
# hist(WB.move$angle)


## ----Start, message=FALSE, warning=FALSE, echo=TRUE--------------------
# Check distributions
# hist(WB.move$step) #Looks like gamma
# hist(WB.move$angle) #Looks pretty normal

# Define Starting Values
# ************************************
# For Step Length (gamma distribution): c(mean1, mean2, sd1, sd2)
stepPar0 <- c(0.1, 1, 0.1, 1)

# For Turning Angle (von mises distribution): c(mean1, mean2, concentration1, concentration2)
# In radians, pi, or 3.14 represents 180 degrees.
anglePar0 <- c(pi, 0, 1, 1)


## ----Fitting, message=FALSE, warning=FALSE, echo=TRUE------------------
# Fit NULL model
WB.null <- fitHMM(data = WB.move, 
                  nbStates = 2,
                  dist = list(step = "gamma", angle = "vm"), # Need to add these as a list
                  Par0 = list(step = stepPar0, angle = anglePar0), # Need to add these as a list
                  formula = ~ 1,
                  estAngleMean = list(angle=TRUE))

# Examing the 2-state model
WB.null


## ----Fitting Plots, message=FALSE, warning=FALSE, echo=TRUE------------
# Plot the results of the predictions.  
# Colored states (State 1 is orange; state 2 is blue) provide the predicted state in each trajectory.  
# Plot all animals
# plot(WB.null,
#      ask = F)

# Plot individual animals
plot(WB.null,
     animals = "Kiranto",
     ask = FALSE)


## ----Viterbi, message=FALSE, warning=FALSE, echo=TRUE------------------
# Run the algorithm on the fitted model
WB.states <- viterbi(WB.null)

# Look at the state assignments.  The result is just a simple vector of state assignments (class 1 or 2). 
WB.states[1:25]

# What's the proportion of time spent in each state?
prop.table(table(WB.states))

# How does this differ between individuals?
# To answer this question, we need to combine the state assignments with the move object
WB.v.Props <- WB.move %>% 
  # create state column
  mutate(state = WB.states) %>%
  
  # add new column that is the total locations for each animal...used to calculate percentages
  mutate(locs = n(),
         .by = ID) %>% 
  
  # summarize for each animal, and each state, the proportion of locations.  
  # Using reframe, as summarize has been deprecated in latest version of dplyr
  reframe(stateProp = n()/locs,
            sex = unique(sex),
            .by = c(ID, state)) %>% 
  
  # This reduces our data frame from the same initial size, to one with just the unique rows of information.
  distinct() %>% 
  arrange(ID, state)

# Graph results, color by sex to look for any potential patterns.  Just a graphical summary.
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
# plot(WB.move[WB.move$ID == "Kiranto",],
#      ask = FALSE)
# Interestingly, Kiranto made some long distance movements, although most of his time was spent in state 1

# How does his movement compare with Paita, for example?
# plot(WB.move[WB.move$ID == "Paita",],
#      ask = FALSE)
# A very different movement pattern, even though the amount of time in state 1 is very similar to Kiranto.


## ----State Probabilities, message=FALSE, warning=FALSE, echo=TRUE------
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



## ----Temp Model, message=FALSE, warning=FALSE, echo=TRUE---------------
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


## ----Time Model, message=FALSE, warning=FALSE, echo=TRUE---------------
# Fit time covariate model
WB.tod.2state <- fitHMM(data = WB.move,
                 nbStates = 2,
                 dist = list(step = "gamma", angle = "vm"),
                 Par0 = list(step = stepPar0, angle = anglePar0), 
                 formula = ~ cosinor(hour, period = 24),
                 estAngleMean = list(angle=TRUE))

# Summarize
WB.tod.2state


## ----Model Comparison, message=FALSE, warning=FALSE, echo=TRUE---------
# Create a data frame with model comparison metrics
model_comparison <- data.frame(
  Model = c("2-state Null", "2-state Temp", "2-state time"),
  AIC = c(AIC(WB.null), AIC(WB.temp), AIC(WB.tod.2state)))

# Print the model comparison table
print(model_comparison)

# Identify the best model based on AIC
# Results indicate that the 2 state cosinor model is the best
best_model_aic <- model_comparison[which.min(model_comparison$AIC), "Model"]
paste("Best model based on AIC: ", best_model_aic, "model")

# Plot the results
# plot(WB.tod.2state,
#      ask = FALSE)

# Plot an individual
# plot(WB.tod.2state,
#       animals = "Kiranto",
#       ask = FALSE)


## ----Applications, message=FALSE, warning=FALSE, echo=TRUE-------------
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

