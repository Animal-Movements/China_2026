#############
# TELEMETRY ERROR MODELING
# https://www.biorxiv.org/content/10.1101/2020.06.12.130195v2.full
#############

#############
## STEP 0: Do you need to model error?
# How do the scales of error compare to the scales of movement?

#############
## STEP 1: Do you have calibrated data or calibration data?
# Calibration data can be collected or opportunistic.
# Without calibration data, you should supply a prior

#############
## IF YOUR DATA NEEDS CALIBRATION
## STEP 1B: What columns do you have in your data?
# DOP values? (HDOP, VDOP, PDOP, GDOP, ...)
# location classes?
# number of satellites?
# time-to-fix timeout?
# ctmm will try to pick out the best data

# load ctmm library
library(ctmm)

# load turtle data
data(turtle)
# or as.telemetry on the turtle data filename

##############
## IF YOUR DATA NEEDS CALIBRATION
## AND ESPECIALLY IF YOUR DATA HAVE NUMEROUS ERROR-RELATED COLUMNS
## STEP 1C: Error model selection

# turtle datasets
names(turtle)
# first two are calibration data - not turtles

# second look at columns
head(turtle[[1]])
# HDOP: horizontal dilution or precision -- proportional to RMS error (location error w/ location class)
# location class: 3D (or 2D) -- generally doesn't matter (calculated on same scale), but some companies use diff algorithms for 2D vs 3D
## HDOP: want proportionality constant close to 1

## Assuming all of the information is good

help("uere.fit")  # user equivalent range error 

# fit error parameters to calibration data (informs error model)
UERE <- uere.fit(turtle[1:2])
# do not run uere.fit on tracking data (using calibration data, not moving turtles)

# estimated error model parameters
summary(UERE)
## 3D fixes have ~7 at HDOP 1
## 2D fixes have ~ 30 at HDOP 1 (diff scales)

# apply error model to data
uere(turtle) <- UERE

head(turtle$F231)

plot(turtle$F231)  # error circles at each tracked location accounting for calibration data

## If we aren't sure about the error data:
## QUESTION 1: Are the HDOP and location class values informative?
data(turtle)

# make a list to store error models
UERES <- list()

# first attempt: let's use everything
UERES$all <- uere.fit(turtle[1:2])
# do not run uere.fit on tracking data

# summarize error model
summary(UERES$all)

# second attempt: let's drop the location class information
# copy of calibration data
test <- turtle[1:2]
# delete location class column
test[[1]]$class <- NULL
test[[2]]$class <- NULL
uere(test) <- NULL
# store error-model fit (HDOP only)
UERES$HDOP <- uere.fit(test)

# summarize error model
summary(UERES$HDOP)  # removing location class data (only HDOP), ~10.6 m error

# third attempt: let's further drop the HDOP values
# delete HDOP column
test[[1]]$HDOP <- NULL
test[[2]]$HDOP <- NULL
# store error-model fit
UERES$nada <- uere.fit(test)

# summarize error model
summary(UERES$nada)  # removing HDOP as well (large location error estimate)

# compare error-models
summary(UERES)  # AICc selects model w/ all, but all have poor Z^2 values because HDOP values aren't great
# AICc: super-fancy AIC values
# reduced Z-squared statistic (goodness of fit for trends)
# compare to reduced chi-squared statistic (1 = good regression, <1 = suspicious, too good, >1 = bad)

## QUESTION 2: Are these GPS tags identical? (heterogeneity of GPS tags)

# create a list to store individualized error models
indiv <- list()

# calculate individual UEREs
indiv[[1]] <- uere.fit(turtle[[1]])
indiv[[2]] <- uere.fit(turtle[[2]])

# compare calibration parameters
summary(UERES$all) # joint model
## ~7m error for 3D, ~30m for 2D
summary(indiv[[1]]) # but pretty similar if fit separately
summary(indiv[[2]])

# store with joint models
UERES$indiv <- indiv

# compare to joint models
summary(UERES)
# Don't trust AICc here, because it will likely just pick the more complex model (doesn't actually improve much over indiv)


#############
# ERROR CALIBRATION
#############

# calibrate turtle data with best error model
uere(turtle) <- UERES$all  # assign error model to be the error of dataset

# error columns now in data
head(turtle[[1]])
# also now includes a per-time error estimate (error circle)
plot(turtle[[1]]) # see error in indiv turtle movement (most of the variation is explained by error)

#############
# ERROR-MODEL RESIDUALS
#############

# calculate residuals of calibration data w.r.t best error model
RES <- lapply(turtle[1:2],residuals)
# plot residuals
plot(RES)  # residuals in red (st.dev of null model, w/ 95% CI)
## bad error models would give heavy tail distr

# calculate residuals of calibration data w.r.t. worst error model
uere(test) <- UERES$nada  # homoskedastic error model

RES2 <- lapply(test,residuals)
plot(RES2)

#############
# ERROR-INFORMED MOVEMENT ANALYSIS
#############

# turtle data again
names(turtle)

# take female turtle 231
DATA <- turtle$F231

# plot data
plot(DATA)

help('outlie')  # function to help pick out outliers (using error-informed statistics)

# look for outliers
OUT <- outlie(DATA)  # side effect plot (blue segment = highest speed, used to determine outliers)

plot(OUT)
# you may get other useful information here
head(OUT)

# good location estimates
GOOD <- OUT$speed < 0.05 # biological threshold for this species (wood turtle)
# threshold speed for turtle (can remove data above that speed, because those are unreasonable outliers)
# always check errors and outliers just in case because sometimes there's unclear delineation of threshold
## or GPS signals could bounce around, which affects path lengths (gives larger errors than expected)

# take only good location estimates
DATA <- DATA[GOOD,]

# re-check
plot(DATA)
OUT <- outlie(DATA)
# Note now all the points are similarly distanced from median and multiple quickest movement speeds
## data looks more reasonable
plot(OUT)

# create guesstimate interactively
ctmm.guess(DATA)
# * check the error box (turns error on, not on by default because it's slow)

# create guesstimate non-interactively
GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=TRUE),interactive=FALSE)
# new argument CTMM, which can contain extra parameters,
# here error=TRUE

# fit models
FITS <- ctmm.select(DATA,GUESS,verbose=TRUE,trace=3,cores=-1)
# verbose=TRUE returns all candidate models
# I've already run this code for you
# save(FITS,file="Day5_BehaviourAnalysis_PT2/Data/turtle.rda")
load("Day5_BehaviourAnalysis_PT2/Data/turtle.rda")

# look at all models
summary(FITS)
## location error model turned on

# look at best model
summary(FITS[[1]])
## updated error parameter estimates (still similar to what was fed in)

# compare to prior model
summary(uere(DATA))

# compare to movement model without error model 
GUESS <- FITS[[1]]
GUESS$error <- FALSE
FIT.NE <- ctmm.fit(DATA,GUESS,trace=2)  # fits quickly w/out error model turned on

summary(FITS[[1]])$CI  # with error model ( also allows comparison btwn two datasets/studies using diff GPS tags)
summary(FIT.NE)$CI  # without error model (larger HR estimate due to all variation being explained by movement)
## bigger movement rates too, some jittering of data should be explained by location error, 
## but w/out error model, it's all being explained by animal movement

## Smoothing data for other packages (not ctmm)

help('predict', package="ctmm")

SMOOTH <- predict(DATA,FITS[[1]])  # smoothed data to update, removes some error because it feeds in the movement model

plot(DATA)
plot(SMOOTH)  # updated estimated of turtle locations (uses error ellipses, not circles)

SIM <- simulate(DATA,FITS[[1]])  # simulate based on movement + error model
plot(SIM)

## IF YOU DIDN'T HAVE CALIBRATION DATA, SUPPLY A PRIOR

# load un-calibrated datas
data(turtle)

# will need to match the class structure (2D,3D here)
summary(uere(turtle))  # guesses for error (10m)

# supply point estimates (assign numbers to error)
# 20-meter 2D error at HDOP=1
# 10-meter 3D error at HDOP=1
uere(turtle) <- c(20,10)

# extract calibration object
PRIOR <- uere(turtle)
# the default uncertainty when assigning numerical error is zero
summary(PRIOR)
PRIOR$DOF

# set DOF for wide credible intervals
PRIOR$DOF[] <- 2  # Bayesian: error is worth 2 data points
summary(PRIOR)  # data calibrated w/ prior (much more uncertainty due to few data points)

# assign prior to data
uere(turtle) <- PRIOR

# automated guesstimate for calibrated data
GUESS <- ctmm.guess(turtle[[3]],CTMM=ctmm(error=TRUE),interactive=FALSE)
FIT.PRIOR <- ctmm.select(turtle[[3]],GUESS,trace=3,cores=-1)
# this will take a while, but comes out consistent
# save(FIT.PRIOR,file="Day5_BehaviourAnalysis_PT2/Data/turtle-prior.rda")
load("Day5_BehaviourAnalysis_PT2/Data/turtle-prior.rda")

summary(FIT.PRIOR)  # using prior and updating it

# compare update to prior
summary(PRIOR)
## further updated due to larger DOF (bigger sample size)
## but be cautious (better than nothing but not a substitute for actual calibration data)
## may just get same prior back regardless of updates (best you'll get, because this is a last resort to adding an error model)
