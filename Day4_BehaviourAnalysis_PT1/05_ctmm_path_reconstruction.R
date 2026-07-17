# load ctmm package
library(ctmm)

# load buffalo data from package
data(buffalo)
projection(buffalo) <- median(buffalo)

names(buffalo)

# look at Cilla
DATA <- buffalo$Cilla

# selected autocorrelation model
GUESS <- ctmm.guess(DATA,interactive=FALSE)
FIT <- ctmm.select(DATA,GUESS,trace=3)
# save(FIT,file="Day4_BehaviourAnalysis_PT1/Data/cilla.rda")
# I've already run this
load("Day4_BehaviourAnalysis_PT1/Data/cilla.rda")

#############
# PREDICT
#############

# take the first 10 locations (first 10 hrs)
SUB <- DATA[1:10,]

# plot subset
plot(SUB)

# if working with this amount of data, you might consider alternative ICs
# in particular IC="LOOCV"
help("ctmm.select")
# I'm just sub-setting for purposes of visualization and speed

# convenience function
help("%#%")
1 %#% 'hr'

# make an array of times over the same period, but 5 min apart
SEQ <- seq(from=SUB$t[1],to=SUB$t[10],by=5 %#% 'min')

# predict locations at those times
help('predict.ctmm')  # makes conditional preds based on movement and location data
PRED <- predict(SUB,FIT,t=SEQ)  # predict for every 5 mins
## can allow smoothing for better location estimates (for when data points have greater or lesser uncertainty)

# plot predictions & data
plot(list(PRED,SUB),col=c('blue','red'))
## uncertainty ellipses (more certain near data points, but also depends on how long to next point)

#############
# CONDITIONAL SIMULATION
#############

# 1 minute sequence
SEQ <- seq(from=SUB$t[1],to=SUB$t[10],by=1 %#% 'min')

# simulate locations at those time
help('simulate.ctmm')  # simulate things freely or conditional on data
SIM <- simulate(SUB,FIT,t=SEQ)  # simulate path for every min

# plot conditional simulation & data
plot(list(SIM,SUB),col=c('blue','red'),type=c('l','p'))


SIM2 <- simulate(SUB,FIT,t=SEQ)
# plot conditional simulation & data
plot(list(SIM,SIM2,SUB),col=c('blue','orange','red'),type=c('l','l','p'))
## some variation in simulated trajectories based on the data
## variance would match up with the error ellipses

# that is only trajectory uncertainty
# can also include parameter uncertainty
help('emulate')  # simulating movement model itself from posterior of estimate of movement model (for when model is unknown)
## includes parameter uncertainty

SIM3 <- simulate(SUB,emulate(FIT, fast = T),t=SEQ)  # emulate(FIT) for sample movement model to approximate unknown movement model

plot(list(SIM,SIM2,SIM3,SUB),col=c('blue','orange','black','red'),type=c('l','l','l','p'))

##########################
# Occurrence distributions
##########################

# full dataset
plot(DATA)

OD <- occurrence(DATA,FIT)  # occurrence distribution (average distributions)
plot(OD,col.level=NA)

SIM <- simulate(DATA,FIT,dt=5 %#% 'min')  # simulation of whole dataset (if you know exactly where the animal goes, all sims would be same)
plot(SIM)  # called utilization distribution (but area doesn't correspond to space use, isn't area that you know or estimate the animal to use)

# If you have habitat values and want to know how much (should be primary use of ODs, not for HR estimation)
# time an animal spent you can calculate the weighted average:

# sum(RASTER*OD) = E[RASTER]  # expected value of covariate that animal sampled during data collection found by summing products of raster and OD
# where OD is the exported 'PMF'
help('export')
