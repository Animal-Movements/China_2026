###################
# HELP
###################

# help files
help(package="ctmm")

# vignettes
browseVignettes(package="ctmm")

# FAQ
help("ctmm-FAQ",package="ctmm")

# ctmm user group for any questions or help
browseURL("https://groups.google.com/g/ctmm-user")

# issue reporting
browseURL("https://github.com/ctmm-initiative/ctmm/issues")

# ctmm learning material - where this script is from
browseURL("https://github.com/ctmm-initiative/ctmmlearn")

# ctmm manuscripts
browseURL("https://www.dropbox.com/sh/55ylq4rbm9pl4d9/AAC2WlRCfgQDYrVRpu5pgrfFa?dl=0")

# development branch of ctmm (more recent than CRAN)
remotes::install_github("ctmm-initiative/ctmm")

# what's new in ctmm
news(package="ctmm")

# ctmm point-and-click app - if you know anyone that doesn't user R
# remotes::install_github("ctmm-initiative/ctmmweb")
# ctmmweb::app()

# ctmm MoveApps
browseURL("https://www.moveapps.org/apps/browser?q=ctmm")

###################
# IMPORT AND VISUALIZE
###################

#! load the ctmm package
library(ctmm)

# STEP 1: Get data through MoveBank
# STEP 2: Import data with as.telemetry()
help("as.telemetry")

# loading data from Movebank CSV (which can be compressed)
Buffalo <- as.telemetry('data/Kruger African Buffalo, GPS tracking, South Africa.zip')
# you can also import from a move object, data.frame, etc.
# GPS location error can be an issue if greater than the sampling timesteps

#! load buffalo dataset from ctmm
data(buffalo)
help("buffalo")

# this is a list of buffalo telemetry objects (all objects are telemetry objects)
class(buffalo)

# number of buffalo datasets
length(buffalo)

class(buffalo[[1]])
head(buffalo[[1]])

# names of buffalo
names(buffalo)

# summary of buffalo data
summary(buffalo)
## 1 hour sampling except Pepper's collar malfunctioned (2 hours)

###################
# PLOT TELEMETRY
###################

help("plot.telemetry")

# plot all buffalo
plot(buffalo,main="6 African buffalo") ## telemetry tracking data for all 6 indivs
# but all the same color

# plot buffalo with list-sorted rainbow of colors
COL <- rainbow(length(buffalo))
plot(buffalo,col=COL,main="Rainbow colors") ## rainbow by default
## includes color function, which can color by indiv (spatially closer indivs are more colorly distinct)

# plot buffalo with spatially-separated rainbow of colors
COL <- color(buffalo,by='individual')
plot(buffalo,col=COL,main="Spatial color separation")

# many other built in coloring options for telemetry objects
help("color")
# you can color by sunlight, moonlight, season, time, ...

####################
# PROJECTIONS
####################

# what projection are the buffalo in
projection(buffalo)
## want a projection ideally w/out distortion (tangent to where map is on globe)
## the further out in projection, the more distorted (better at the focal point)
## only local projections are good, unless working w/ remote sensing data (project it to that data)
# 2 point as equidistant object is the safest

# You want a projection that is locally flat over your data (to minimize distortion).
# By default, as.telemetry() will choose a two-point equidistant projection, which is
# safer for migratory species, but does not preserve North=up.
# The algorithm can be found in:
ctmm:::median_longlat
# and automates the estimation of k=2 geometric median (robust) clusters
median(buffalo)
# Object of class "telemetry"
# longitude  latitude x y
# 1  31.82024 -24.73712 0 0

# show north on plot (puts North facing to the side due to foci on horizontal)
compass()

#! center the projection on the geometric median of the data
projection(buffalo) <- median(buffalo)

projection(buffalo)

# now North=up, which is fine for this dataset
plot(buffalo,col=COL,main="Azimuthal-equidistant projection") # changes the method so that north is up
compass()

###################
# VARIOGRAM
###################

# names of buffalo
names(buffalo)

#! select buffalo Cilla
DATA <- buffalo$Cilla

# plot telemetry object
plot(DATA,main="Cilla")

# color by time
COL <- color(DATA,by='time')
plot(DATA,col=COL)
# easier to see migrations/dispersals
# range resident (stays in area for the entire sampling period)

#! calculate a variogram object (named SVF) from the telemetry object
SVF <- variogram(DATA)
plot(SVF,main="Variogram")
# on average how far apart (in distance^2) given a time lag between any two points
## spatial autocorrelation across time (time-lag on x-axis instead of distance)
## can use diff sampling time intervals
## variance is asymptote of variogram
## time to reach asymptopte (time independence)
### Here, we would course the data to 0.5 months (when it asymptotes, no spatial autocorrelation)
### Can also see how long it takes the buffalo to cross its home range


# help file for variogram -- gives unbiased view of autocorrelation
help("variogram")
# there are some options in here if you have very irregular data:
#   fast, dt, res
vignette('variogram')
# Sec. "Irregular Sampling Schedules"

# more accurate CIs, too slow for larger datasets
SVF <- variogram(DATA,CI="Gauss") # (n^2)log(n) algorithm for how long it takes to run (slow for large datasets)

# frequently you want to zoom in to the beginning of the variogram
# plot with zoom slider
zoom(SVF,main="Variogram with good CIs") ## Useful zoom to see nugget effect
# things to look for
# * the asymptote (if any)
# * how long does it take to asymptote
# * initial curvature or initial linear? -- slope of linear portion characterizes the Brownian motion model
### -- DIFFUSION (ask more later) of square distance/time
### -- initial quadratic curvature (if clean, finely sampled data) = mean speed of animal (or square of mean speed of animal)
## possible nugget effect if location errors are not correlated (would give nugget)
## want nugget = 0

## How many square km away at a time-lag of a month did the buffalo get away?
## Asymptotes at about 0.5 month -- autocorrelated data until ~0.5 months
## the longer you wait, the further the animal moves until asymptote (corresponds to home range scale)
## straight line increase corresponds to animal speeds

## Variogram is unbiased estimate

# Show variogram w/ ACF for residuals of IID model
IID <- ctmm.fit(DATA)
RES <- residuals(DATA, IID) # extract residuals
ACF <- correlogram(RES, res = 10) # correlogram more robust than acf function (estimates acf parameter)
## Shows autocorrelation variogram w/ where 0 autocorrelation and 95% CI are
### essentially an upside-down variogram on diff scale
### ideally want residuals to drop down to 0 immediately
## Not unbiased estimator if there's autocorrelation

zoom(ACF) # plot ACF 
# for testing if there's autocorrelation (biased estimator though)
## Fourier transform from time to freq gives correlogram

# Periodogram for autocorrelation
LSP <- periodogram(DATA)
plot(LSP)
## 1/freq for period gives all the data on all the different time scales
## slow moving animal: low variance for large periods????
## white noise for flat line (same energy at every freq)
## can see patterns in their movement periodicity (ex. albatross hunts once/month = spike in activity around 1 month)
## Looking for periodic movement patterns (autocorrelated movement)

# Less common in movement ecology are spectrograms
## heat map of energy (intensity) plotted on freq ~ absolute time scale



###################
# MODEL SELECTION
###################

# model guesstimate function
help("ctmm.guess")
# variogram will be calculated automatically (with default arguments)
# this is interactive mode
ctmm.guess(DATA,variogram=SVF) ## automates guesses for parameters (for optimization)
# notice how much work I spent automating the units of every plot
# for range resident animals (there are other models for non-resident animals, would look like there isn't much asymptote)
## See the zoom gear for parameter estimates: variance (km^2), tau_p (position, day), tau_v (velocity, min)
## variance changes asymptote height
## tau_p time scale for autocorrelation in position: how long it takes animal to cross range (i.e. home range if range resident)
### -- time taken to asymptote
## tau_v time scale for autocorrelation in velocity: how much time is animal going in same direction at same speed (straight line movement time)
### -- changes linearity of initial curvature (i.e. coursely-sampled data would be linear, can't see animal's finite speed)
### -- Brownian motion model is okay for course data (fractal movement), but finite speeds models are better if finer data


# this is noninteractive mode
GUESS <- ctmm.guess(DATA,interactive=FALSE)

# automated model selection
help("ctmm.select")

# fit a bunch of autocorrelation models, tell me what models are being fit, 
## return all models, and use all but one CPU core (parallelizable)
## 10x more data = 10x longer to run (computation time proportional to sample size)
FITS <- ctmm.select(DATA,GUESS,trace=3,verbose=TRUE,cores=-1)
# candidate models: OUF, OUf, OUΩ, IOU, BM, IID, inactive
# I've already run this code for you -- slowest step of analysis
# save(FITS,file="cillas.rda") -- store results
load("data/cillas.rda") # All autocorrelation models stored

# lets look at the results
summary(FITS)
# OUF: position autocorrelation and velocity autocorrelation
# Anisotropic: distribution can be elongated
# Check AICc parameters
## Here, OUF anisotropic is best fit
## AIC increased as features were dropped (nested models)

# IID was not attempted because the nested-model hierarchy is OUF -> OU -> IID
# so let's include the IID models
FITS[["IID anisotropic"]] <- ctmm.fit(DATA) ## assumes no autocorrelation
FITS[["IID"]] <- ctmm.fit(DATA,ctmm(isotropic=TRUE))

# now including IID model
summary(FITS)

# lets look at individual models
# IID  anisotropic model
summary(FITS$`IID anisotropic`)
# DOF = degrees of freedom (effective sample size)
## - here, DOF_area = # data points
# CIs are pretty narrow
# DOP = dilution of precision, gives error estimates for each point

# compare mean and covariance to data
plot(DATA,FITS$`IID anisotropic`,main="IID Gaussian Distribution")

# compare empirical variogram to that of model
zoom(SVF,FITS$`IID anisotropic`,main="IID Variogram")
# Non-overlapping CIs is bad
## IID model poorly fits data

# calculate residuals
RES <- residuals(DATA,FITS$`IID anisotropic`)

# scatter plot of residuals
plot(RES,main="IID Residuals")
# Residuals not normally distributed

# calculate correlogram of residuals
ACF <- correlogram(RES,res=10)
# res=10 is for drifting sampling rate (increased time intervals)
# alternatively, fast=FALSE

zoom(ACF,main='ACF of "IID" Residuals')
## RE: red bands for 95% CI for no autocorrelation

# The first model is the selected model
summary(FITS)
# The selected OUF anisotropic model
summary(FITS[[1]])
# area here is Gaussian area --> estimated # home-range crossings
# speed here is Gaussian RMS (root-mean squared) speed, narrow CIs (quick-and-dirty estimate)
# More parameters in model
# diffusion rate: more abstract than mean speed, but can be estimated at longer timescales/coarser data
## -- alternative measure for how active animal is
# smaller effective sample size (only ~18 data points due to autocorrelation)

# 95% location autocorrelation remaining
exp(-3)
summary(FITS[[1]])$CI[2,] * 3
## position autocorrelation disappears in ~22.5 days

summary(DATA)
## Cilla sampled for ~5 months w/ point estimate at ~1 week
## `ctmm` automatically gives nice round units for timescales
(4.967566 %#% 'months') / (7.505372 %#% 'days')
## = ~20, close to effective sample size (~18)
help("%#%") # converts dimensional quantities to an from SI units
help(sigfig) # gives nice sig figs
sigfig(summary(FITS[[1]])$CI[2,] * 3)

plot(DATA,FITS[[1]],main="Anisotropic Gaussian") # anisotropic
plot(DATA,FITS[[2]],main="Isotropic Gaussian") # isotropic
## ERROR: multiple projections not yet supported

zoom(SVF,FITS[[1]],main="OUF Variogram")
# not perfect, but much better

# residuals
RES2 <- residuals(DATA,FITS[[1]])

plot(RES2,main="OUF Residuals")
# Residuals look better (less clear patterning/structure)

# residual ACF
ACF2 <- correlogram(RES2,res=10)

zoom(ACF2,main='ACF of "OUF" Residuals')
# Most of autocorrelation is explained by model

# you can do well by hand
ctmm.guess(DATA,variogram=SVF)
# RE: this is a stationary model (parameters don't change with periods of time/activity)
## - ideally need one stationary model per behaviour (movement behaviour)
# why is this model fit deflected down?
zoom(SVF,FITS$`OU anisotropic`,main='ACF of "OU" Residuals') 
# this model doesn't fit well at all, wrong type of autocorrelation model
## Brownian for short periods of time, but buffalo has persistence of motion (empirical variogram has initial quadratic shape)
# mismatch in autocorrelation models and data, diffusion rate too low, which propagates to higher time lags
# biases position autocorrelation timescale to be too long (not fitting well)

######################
# Non-resident models
ctmm.guess(DATA,ctmm(range=FALSE)) # `range = FALSE` for non-resident indivs
# fits very well for first few days, but much worse at longer timescales
FITS2 <- ctmm.select(DATA,GUESS,verbose=TRUE,trace=3)

summary(FITS2)
zoom(SVF,FITS2[["IOU anisotropic"]])
zoom(SVF,FITS2[["BM anisotropic"]])
## Generally don't need these models (3 fewer parameters)
### Can be useful if very little data and have 3 less useful parameters
## BUT can't be compared for model selection with AIC/BIC methods

# Likelihoods/AICs cannot be compared
summary(c(FITS,FITS2))
## Can use a specific likelihood model validation method (lead-one-out cross validation, LOOCV)
## Might have to manually judge the model fit

# see help("ctmm.select") IC="LOOCV" argument for tiny tracks

################
# TEASER
################

# simulate data from the selected model with same times (same timescale)
SIM <- simulate(FITS[[1]],t=DATA$t)
## Not conditioned on the data
# simulated utilization distribution estimation

# plot data
plot(SIM,main="Cilla Simulacrum")
# what areas does this individual like/dislike? 
## Generally areas where there are more data points, but RE: spatial autocorrelation in data
## diffusion and speed data more trustworthy
## Guassian model

# SPOILER
plot(SIM,FITS[[1]],level=NA) # did not set seed so it'll be random
