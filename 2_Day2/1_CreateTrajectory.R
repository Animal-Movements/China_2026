## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Setup, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------
# Remove items from memory/clean your workspace
rm(list=ls())

# You may need to install these packages first
#install.packages('adehabitatLT', 'amt', 'sf', 'tidyverse', 'units', 'gridExtra', 'lubridate', 'date')

# Load libraries
library(adehabitatLT)
library(amt)
library(sf)
library(tidyverse)
library(units)
library(gridExtra)
library(lubridate)
library(date)


## ----Data Import, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------
# Data load/import
load("Data/wildebeest_data.rdata")

# Check the projection.
# st_crs(WB.sf)

# AdeHabitatLT requires a dataframe for analyses.  This means we need to remove the geographic information from the header of the file.  We'll use the WB.sf dataset, converting the file to a flat dataframe with coordinate information contained in columns.

# Dataframe conversion
WB.data <- WB.sf %>%
  as_tibble() %>%
  mutate(X = st_coordinates(WB.sf)[,1],
         Y = st_coordinates(WB.sf)[,2]) %>%
  dplyr::select(-geometry) %>% 
  drop_units()

# Look at the data. How does this file differ from the WB.sf object?
head(WB.data)


## ----DOP, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------------------
# Plot the DOP values for confirmation.  
# For this dataset, we will separate 2D and 3D positions and then use a qualitative filter to remove data above a threshold.  We will be more restrictive on 2D positions (dop < 5.0) than 3D positions (dop < 10.0).

# Let's first get a summary of how many records are in the dataset before removing records.  This way we can track how much this filtering impacts the size of the dataset.
(val1 <- nrow(WB.data))

# Plot 2D  and then 3D positions
P1.FT2 <- 
  ggplot(WB.data[WB.data$fixType == "2D",], aes(x = DOP)) +
  geom_histogram(color = "black", fill = "white", bins = 25) +
  labs(title = "GPS Data (2D)", x = "DOP", y = "Frequency") +
  geom_vline(xintercept = 5, color = "red", linetype = "dotted", linewidth = 1) +
  theme_classic()

# Plot 3D positions (most of data)
P1.FT3 <- 
  ggplot(WB.data[WB.data$fixType == "3D",], aes(x = DOP)) +
  geom_histogram(color = "black", fill = "white", bins = 25) +
  labs(title = "GPS Data (3D)", x = "DOP", y = "Frequency") +
  geom_vline(xintercept = 10, color = "red", linetype = "dotted", linewidth = 1) +
  theme_classic()

# Create filter to only accept positions with a 3D fixtype and DOP less than 10 or a 2D fixtype and DOP less than 5
WB.data <- 
  WB.data %>% 
  filter(
    fixType == "3D" & DOP < 10 | fixType == "2D" & DOP < 5) 

# How many records now after filtering?
(val2 <- nrow(WB.data))

# Plot again
P2.FT2 <- 
  ggplot(WB.data[WB.data$fixType == "2D",], aes(x = DOP)) +
  geom_histogram(color = "black", fill = "white", bins = 25) +
  labs(title = "GPS Data (2D) - DOP Filtered", x = "DOP", y = "Frequency") +
  geom_vline(xintercept = 5, color = "red", linetype = "dotted", linewidth = 1) +
  theme_classic()

P2.FT3 <- 
  ggplot(WB.data[WB.data$fixType == "3D",], aes(x = DOP)) +
  geom_histogram(color = "black", fill = "white", bins = 25) +
  labs(title = "GPS Data (3D) - DOP Filtered", x = "DOP", y = "Frequency") +
  geom_vline(xintercept = 10, color = "red", linetype = "dotted", linewidth = 1) +
  theme_classic()

# Plot the data together
grid.arrange(P1.FT2, P2.FT2, P1.FT3, P2.FT3, ncol = 2)

# What's the percent of data that have been removed?
round((val1-val2)/val1, digits = 4)


## ----Trajectory, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------
# Let's check to make sure all observations are complete
all(complete.cases(WB.data))

# And check for duplicated timestamps
WB.data %>% 
  group_by(id) %>% 
  summarise(any(duplicated(timestamp)))

# If duplicates exists, you need to remove them before moving forward
#WB.data <- WB.data[!duplicated(WB.data),c('id','timestamp')]

# Create a separate dataframe object with the X and Y coordinates to use with the as.ltraj function
XY <- WB.data %>% 
  dplyr::select(X:Y) %>% 
  as.data.frame()

# Create trajectory
WB.traj <-as.ltraj(xy = XY,
                       date = WB.data$timestamp,
                       id = WB.data$animal_id,
                       typeII = TRUE, 
                       infolocs = WB.data[,3:8],
                       slsp = "missing")

# Look at the summary of the created object
WB.traj


## ----Resample, message=FALSE, warning=FALSE, echo=TRUE------------------------------------------
# Here, we will use the first location in the dataset as the reference date/time.  
# Set reference date/time
refda <- WB.traj[[1]]$date[1]

# Set all null values to NA.  Based on 3 hour interval.
WB.traj.na <- setNA(WB.traj, 
                    date.ref = refda, 
                    dt = 3, 
                    units = "hour") 

# Summarize the new dataset
# WB.traj.na

# Is the trajectory regular?
is.regular(WB.traj.na)

# Create a regular trajectory by rounding the time to the exact time intervals.
WB.traj.reg <- sett0(WB.traj.na, 
                      date.ref = refda, 
                      dt = 3, 
                      units = "hour")

# Summarize the new dataset
WB.traj.reg

# Is the trajectory regular?
is.regular(WB.traj.reg)


## ----Summarize, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------------
# Summarize the movement trajectory
Summary.traj <- summary(WB.traj.reg)

# Note that nb.reloc is the total number of relocations collected at a 3 Hour sampling interval.  Any missing data have been filled with NA, allowing us to calculate the percent complete. 

# Add details to the summary table
Summary.traj <-
  Summary.traj %>% 
  mutate(
    Duration = round(difftime(date.end, 
                              date.begin,
                              units = "days"),
                     digits = 2),
    Records = nb.reloc - NAs,
    PctComplete = round((Records/nb.reloc)*100,
                        digits = 2))

# View the table
Summary.traj


## ----Plotting, message=FALSE, warning=FALSE, echo=TRUE------------------------------------------
# Plot all animals together, helpful since all animals are plotted on the same scale.
plot(WB.traj.reg)

# View each animal separately using vector notation
plot(WB.traj.reg[5])

# View the columns included in the data object 
#**Note**, you must use double brackets [[]] because the output is a list
#names(WB.traj.reg[[1]])
#head(WB.traj.reg[[1]])

# Contents of the ltraj object:
# dt: time between locations in seconds
# dist: distance between the next location
# R2n: net squared displacement
# abs.angle: absolute turning angle
# rel.angle: relative turning angle
# dx and dy represent the change in the x and y directions.

# A variety of additional plotting options also exist in the package.  See help(plotltr).
#plotltr(WB.traj.reg[5], which = "dist") # Distance moved between discrete points (3 hour interval)
#plotltr(WB.traj.reg[5], which = "dt/60") # This shows that the data are regular.  y-axis corrected (dt/60) to show minutes
#plotltr(WB.traj.reg[5], which = "DOP") # DOP.  As expected, all values are < 10 DOP.


## ----Plotting2, message=FALSE, warning=FALSE, echo=TRUE-----------------------------------------
# Here I will use basic R plotting functions
# This same workflow, however, could be adopted using GGPLot (A good homework assignment!)

# Convert trajectory to a flat dataframe (all data appended into a single file)
WB.move <- ld(WB.traj.reg)

# Create a reference id so can switch easily between individuals
Id.val <- unique(WB.move$id)
 
# Determine which animal you want to plot.  Here, I will plot the 5th animal in the object we created above
i <- 5

# Subset the data to our animal of interest and remove NAs from the dataset
WB.sub <- WB.move %>% 
  filter(id == Id.val[i],
         !is.na(x))

# Calculate the total days tracked
time.diff <- WB.sub %>% 
  summarize(start = min(date),
            end = max(date),
            diff = trunc(difftime(end, start, units="days"))) %>%  
  pull(diff) # This will make the time.diff variable = diff

# Setup a 2x2 plot layout with three panels
layout(matrix(c(1,1,2,3), 2, 2, byrow = FALSE), widths=1, heights=c(1,1))

# Plot the trajectory
plot(WB.sub$x,WB.sub$y,typ="l",xlab="Easting",ylab="Northing",
     main=paste(WB.sub$id[1],"Movement"),frame=FALSE,axes=FALSE,asp=1)
     mtext(paste(format(WB.sub$date[1],"%Y-%m-%d")," to ",format(WB.sub$date[nrow(WB.sub)],"%Y-%m-%d")),cex=0.75) # Just specifying how I want the dates to be reported
     axis(1, labels=TRUE)
     axis(2, labels=TRUE)
     # Color the points
     points(WB.sub$x,WB.sub$y,pch=16,cex=0.5,col="blue")  # All points Blue
     points(WB.sub$x[1],WB.sub$y[1],pch=17,cex=1,col="green") # Starting point Green
     points(WB.sub$x[nrow(WB.sub)],WB.sub$y[nrow(WB.sub)],pch=15,cex=1,col="red") # End point Red

# Plot the movements over time (Velocity)
plot(WB.sub$date, WB.sub$dist/1000, type='l', ylab="Distance moved (km)", xlab="Time", main="Steplengths (3-hour)", frame=FALSE)
    # Calculate the time from release date
    mtext(paste(time.diff,"days"),cex=0.75)
    
# Plot the net displacement per step
plot(WB.sub$date, sqrt(WB.sub$R2n)/1000, type='l', ylab="Distance (km)", xlab="Days Since Release", main="Net Displacement",frame=FALSE)
    mtext(paste(time.diff,"days"),cex=0.75)

# Same graph, but using ggplot    
# ggplot(data = WB.sub,
#        aes(x = date,
#            y = sqrt(R2n)/1000)) +
#   geom_line() +
#   labs(title = "Net Displacement") +
#   xlab("Days Since Release") +
#   ylab("Distance (km)")


## ----Plotting Loop, message=FALSE, warning=FALSE, echo=TRUE, eval=TRUE--------------------------
# Set Output Directory
dir.out <- paste0(getwd(),"/Output/Plots/")

# Create the directory if it doesn't exist
if (!dir.exists(dir.out)){
  dir.create(dir.out)
  print ("Creating Directory!")
} else {
  print("Directory exists!  Check the Output/Plots folder for results.")
}

# Now loop over each individual (by id) and save result
Id.val <- unique(WB.move$id)

# ------------------------------ 
for (i in 1:length(Id.val)){

# Subset the data to our animal of interest and remove NAs from the dataset
# ------------------------------
WB.sub <- WB.move %>% 
  filter(id == Id.val[i],
         !is.na(x))

# Calculate the total days tracked
time.diff <- WB.sub %>% 
  summarize(start = min(date),
            end = max(date),
            diff = trunc(difftime(end, start, units="days"))) %>%
  pull(diff)

# Create the output name of the plot...everything else in the code is the same
# ------------------------------
png(filename = paste0(dir.out,WB.sub$id[i],"_MvmtPlot.png"))
    
# Setup plot layout with three panels
layout(matrix(c(1,1,2,3), 2, 2, byrow = FALSE), widths=1, heights=c(1,1))

# Plot the trajectory
plot(WB.sub$x,WB.sub$y,typ="l",xlab="Easting",ylab="Northing",
     main=paste(WB.sub$id[1],"Movement"),frame=FALSE,axes=FALSE,asp=1)
     mtext(paste(format(WB.sub$date[1],"%Y-%m-%d")," to ",format(WB.sub$date[nrow(WB.sub)],"%Y-%m-%d")),cex=0.75) # Just specifying how I want the dates to be reported
     axis(1, labels=TRUE)
     axis(2, labels=TRUE)
     # Color the points
     points(WB.sub$x,WB.sub$y,pch=16,cex=0.5,col="blue")  # All points Blue
     points(WB.sub$x[1],WB.sub$y[1],pch=17,cex=1,col="green") # Starting point Green
     points(WB.sub$x[nrow(WB.sub)],WB.sub$y[nrow(WB.sub)],pch=15,cex=1,col="red") # End point Red

# Plot the movements over time (Velocity)
plot(WB.sub$date, WB.sub$dist/1000, type='l', ylab="Distance moved (km)", xlab="Time", main="Steplengths", frame=FALSE)
    # Calculate the time from release date
    mtext(paste(time.diff,"days"),cex=0.75)

# Plot the net displacement per step
plot(WB.sub$date, sqrt(WB.sub$R2n)/1000, type='l', ylab="Distance (km)", xlab="Days Since Release", main="Net Displacement",frame=FALSE)
    mtext(paste(time.diff,"days"),cex=0.75)

# Close the plot
# ------------------------------
dev.off()
}


## ----AMT Track, message=FALSE, warning=FALSE, echo=TRUE, eval=TRUE------------------------------
# Step 1: Nest the Track by ID
WB.trk <- make_track(WB.data, 
                     .x = X, 
                     .y = Y, 
                     .t = timestamp,
                     crs = 32737, 
                     # Include any additional information you want to attach to the track or set "all_cols = TRUE"
                     id = animal_id, sex = sex, temp = temp) %>% 
  # Nest the data by id
  nest(data = -'id')

# Look at the data and note the result
head(WB.trk)

# Step 2: Resample and add net squared displacement
# Note the use of the map() function.  Resample to a 3 hour interval
WB.steps <- WB.trk %>% 
  mutate(
    steps = map(data, function(x)
      # First, resample...applying the function to each nested item (id)
      x %>% track_resample( 
        rate = hours(3),
        tolerance = minutes(5)) %>% 
        # Then, calculate the steplengths
        steps_by_burst() %>% 
        # Net Square displacement can be added, along with various other metrics
        add_nsd()
      ))

# Look at the file now
head(WB.steps)

# Step 3: Select columns of interest and unnest
# We'll grab the animal id and then the calculated movements.
WB.move.amt <- WB.steps %>% 
  # Select columns of interest
  select(id, steps) %>% 
  # Unnest
  unnest(cols = steps)

# This result should now be very similar to the result created from adehabitat
# We can graph the result as before, for example
# WB.Sotua <- WB.move.amt %>% 
#   filter(id == 'Sotua')
# 
# ggplot(data = WB.Sotua,
#        aes(x = t1_,
#            y = sqrt(nsd_)/1000)) +
#   geom_line() +
#   labs(title = "Net Displacement") +
#   xlab("Days Since Release") +
#   ylab("Distance (km)")


## ----Save, message=FALSE, warning=FALSE, echo=TRUE----------------------------------------------
save(WB.move, WB.move.amt, file = "Data/wildebeest_3hr_data.rdata")

