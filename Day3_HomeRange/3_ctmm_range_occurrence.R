###########
# RANGE VERSUS OCCURRENCE DISTRIBUTIONS
# https://doi.org/10.1101/2022.09.29.509951
###########

library(ctmm)
data(buffalo)
projection(buffalo) <- median(buffalo)
DATA <- buffalo$Cilla
load("Day3_HomeRange/Data/cilla.rda")

FITS <- list("OUF anisotropic"=FIT)
# include Brownian motion models
FITS[["BM"]] <- ctmm.fit(DATA,ctmm(tau=Inf,isotropic=TRUE))  # isotropic Brownian motion
# this one is not as commonly used, but let's throw it in
FITS[["BM anisotropic"]] <- ctmm.fit(DATA,ctmm(tau=Inf))  # anisotropic Brownian motion
# square distance moving away proportional to time taken
## fractal/infinite movement path (most appropriate for coarser data)
## doesn't stay w/in HR (diffuses forever), no more appropriate if not necessarily range resident

# you can't compare stationary (IID,OU,OUF) and conditionally stationary (BM,IOU) models with likelihood
summary(FITS)
# but you can compare within
summary(FITS[c("BM","BM anisotropic")])

SVF <- variogram(DATA,CI="Gauss")

# again, the selected model looks okay
zoom(SVF,FITS[[1]])
zoom(SVF,FITS[[1]], frac = 0.01)

# the Brownian motion model looks...
zoom(SVF,FITS$BM)
# why? zoom in (doesn't asymptote, keeps diffusing infinitely, and no initial quadratic (not ballistic))

# range distribution - using the selected model
RD <- akde(DATA,FITS[[1]])  # HR

# occurrence distribution - using the selected model
OD <- occurrence(DATA,FITS[[1]])  # interpolating from distributions of predictions of buffalo occurrence (average distributions tog to get occurrence distribution)
# predicts well for finely sampled data (negatively biased, biases too small for HR)
# still better than the BM model

# conventional (non-dynamic) Brownian bridge (not the selected model)
BB <- occurrence(DATA,FITS$BM)

# plot them
EXT <- extent(list(DATA,OD,RD))  # make extent the largest of the diff distributions
plot(RD,col.level=NA,col.grid=NA,ext=EXT)  # fuzzy and large due to small effective sample size
title("OUF AKDE")
# plot OUF occurrence distribution
plot(OD,col.level=NA,ext=EXT)  # would estimate buffalo to be somewhere along the track (especially for finely sampled data)
title("OUF Krige")
# plot BM occurrence distribution (BB)
plot(BB,col.level=NA,ext=EXT)  # assumes buffalo moves randomly btwn any 2 points (biases small, only good for super coarse data)
title("BM Krige (BB)")

# Q: What is the occurrence distribution?
# A: Given a random time *in the sampling period*, where was the animal

# Q: What is the range distribution?
# A: At some time in the future/past *under the same behaviors* where will the animal be
# A: Long-term space use *for continuing behaviors*

# Impact of COARSENING the data
SUB <- DATA

#########################
# remove every other time
#########################
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]  # 2-hr sampling, 4-hr, etc.: as it coarsens, they will begin to look similar and closer to normal distr. (but they aren't the same)
par(mfrow=c(1,2))
RD <- akde(SUB,FITS[[1]])
OD <- occurrence(SUB,FITS[[1]])
plot(RD,col.level=NA,col.grid=NA,ext=EXT)  # range distr doesn't really change as data coarsens
title("Range distribution")
plot(OD,col.level=NA,ext=EXT)  # occurrence distr. becomes increasingly uncertain as data coarsens (sampled at larger time intervals)
title("Occurrence distribution")
#########################

# repeat the above until they look similar
# how much data when they look similar?
nrow(DATA)
nrow(SUB)

# Impact of truncating the data
SUB <- DATA

####################################
# remove the second half of the data
####################################
SUB <- SUB[1:round(nrow(SUB)/2),]  # cutting data in half
par(mfrow=c(1,2))
RD <- akde(SUB,FITS[[1]])
OD <- occurrence(SUB,FITS[[1]])
plot(RD,col.level=NA,col.grid=NA,ext=EXT)  # range distr doesn't change much but will become less detailed (until closer to Gaussian w/ high uncertainty)
title("Range distribution")
plot(OD,col.level=NA,ext=EXT)  # will lose more and more of the distribution until it's a single point
title("Occurrence distribution")
####################################

# repeat the above
par(mfrow=c(1,1))

# range area = predicted space use, given the same behaviors (biological)
# occurrence area = uncertainty (sampling dependent and limited to the sampling period)
# neither estimate the amount of space used during the sampling period!!!
