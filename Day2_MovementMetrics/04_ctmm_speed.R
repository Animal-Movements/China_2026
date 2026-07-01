#############################
# Speed, distance, diffusion
# https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-019-0177-1
#############################

#! load buffalo dataset from ctmm
data(buffalo)

# north-up projection
projection(buffalo) <- median(buffalo)

# consider first buffalo
DATA <- buffalo[[1]]

# load model fits from ctmm.select
load("Day2_MovementMetrics/Data/cilla.rda")

# units operator
?`%#%`

1 %#% 'day' # day in seconds
1 %#% 'year' # year in seconds

# for time,  will consider the first week of data
DATA <- DATA[DATA$t <= DATA$t[1] + 1%#%'week',]
plot(DATA, col=color(DATA,by='time'), error = FALSE)  # less data so ctmm.select should fit relatively quicker

# fit to first month only
FIT <- ctmm.select(DATA,FIT,trace=3)

# the speed estimate here is RMS Gaussian
summary(FIT)
## contains Gaussian root-mean squared (RMS) speed -- proxy of speed
## mean speed: v^- = E[v] = 1/(tn-t1)*integral of speed over time
## RMS speed: v_rms = root(E[v^2])

# calculate Gaussian (regular mean speed - not RMS)  -- should be w/in an order of magnitude from RMS speed
speed(FIT)
## both speeds assume Gaussian distr of locations and speeds

# non-parametric speed estimation
# "2019 Noonan Fleming Akre ... Calabrese.pdf" in Readings/Continuous_Time folder
speed(DATA,FIT)
## gives mean of mean speed
## data for species w/ vary distinct movement behaviours, speed calcs should be segmented
#### ex. birds stationary/feeding on ground VS flying around
## can also segment day and night; if not, slight underestimation during day, slight overestimation at night
## fairly insensitive to the effect of coarsening data (other methods would estimate animal to be slower for coarser sampled data)

# Impact of coarsening the data
SUB <- DATA
FIT.SUB <- FIT
#########################
# remove every other time
#########################
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]
FIT.SUB <- ctmm.select(SUB,FIT.SUB,trace=3)  # fits more quickly w/ less data
# the speed estimate here is RMS Gaussian
## won't get speed estimate if too coarse (resolution of data not high enough)
summary(FIT.SUB)  # wider CIs
# Gaussian (regular speed - not RMS)
speed(FIT.SUB)  # similar estimates to finer sampled data, but more uncertainty (larger CIs)
# non-parametric speed estimation
speed(SUB,FIT.SUB)  # fits more slowly w/ less data (note lower DoF for speed, effective sample size for speed estimation)
## At some point where data is too coarse, the movement model might change to one that doesn't incorporate a velocity autocorrelation component
## Will get no speed estimate in that case
#########################
# repeat until data become too coarse
## Can look instead at diffusion rate if data too coarse (diffusion rate as a more reliable metric of animal movement activity)
## Can also look at location error

# keep in mind the stationary assumption of the model
# see the appendix of Noonan et al.

###########################
# Population meta-analysis
###########################

help('meta')

#Load in the fitted movement models
load("Day2_MovementMetrics/Data/buffalo.rda")

#Estimate mean spead for each animal
SPEEDS <- list()
for(i in 1:length(buffalo))
{
  SPEEDS[[i]] <- speed(buffalo[[i]],FITS[[i]])
}
names(SPEEDS) <- names(buffalo)
# save(SPEEDS,file="data/buffalo_speeds.rda")
load("Day2_MovementMetrics/Data/buffalo_speeds.rda")  # estimate mean speed of every buffalo


meta(SPEEDS,sort=TRUE)  # mean = pop mean of mean speeds


###########################
# Instantaneous speeds
###########################

# Speed at a particular given time
INST_SPEEDS <- speeds(buffalo[[1]],FITS[[1]])

head(INST_SPEEDS)
