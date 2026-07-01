################
# Resource Selection Functions (Habitat suitability modeling)
# © Christen Fleming & Björn Reineking
# Alston & Fleming et al., Methods in Ecology and Evolution 4:2 643-654 (2023)
################

# RSFs: parametric estimation of animal resource use (why does animal live in a certain place)
## what drives where the animal is located
## Specify a probability distribution (log link) for a model of habitat selection 
### where (+) coeff means attraction to resource and (-) coeff means repulsion from resource 
### (normalize the function, by dividing by Monte Carlo integration for a probability distribution likelihood function)
## traditional methods can be used as an "approximation" of the more rigorous model
## read up on "spatial point processes"

library(ctmm)
load("data/tapir.rda")
# E.P. Medici, Data from: Study "Lowland tapirs, Tapirus terrestris, in Southern Brazil", Movebank Data Repository (2023)
# tree cover data from the Hansen forest map based on Landsat 7

# plot one tapir with treecover raster, to make sure we have appropriate environmental data & projection
i <- 1
DATA <- tapir[[i]]
projection(DATA) <- median(DATA)
plot(DATA,error=2,R=treecover,main="Lowland tapir under tree cover")
## green = tree cover, white = mostly grassland (possibly wetlands), red = tapirs

# select an autocorrelation model
# for the moment rsf.fit only uses isotropic models
GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=TRUE,isotropic=TRUE),interactive=FALSE)
FIT <- ctmm.select(DATA,GUESS,trace=3)
# save(FIT,file="data/tapir-iso.rda")
load("data/tapir-iso.rda")

# raster covariates must be in a named list
R <- list(tree=treecover)
# see raster::as.factor() for categorical variables

# AKDE (no RSF)
AKDE <- akde(DATA,FIT,weights=TRUE)
plot(DATA,error=2,UD=AKDE,R=treecover,col.grid=NA,main="AKDE")
## independent of covariates (not considering preference for/against tree cover)

# fit IID model for comparison
IID <- ctmm.fit(DATA,CTMM=ctmm(isotropic=TRUE))
KDE <- akde(DATA,IID)

help("rsf.fit")

# assigned weight without autocorrelation
plot(DATA$timestamp,mean(KDE$DOF.area) * KDE$weights,xlab='time',ylab="weight", ylim = c(0,1.2))  # subtracted 1 from the mean
# How many points do you need for an IID RSF estimate?
# iRSF without autocorrelation: iterates until the default 1% error threshold
RSF.IID <- rsf.fit(DATA,KDE,R=R)
## assuming independently sampled data (sampling "available" points)
### each point's impact on likelihood is heavy due to assumption of independence
## verbose shows change in log likelihood and betas

# assigned weight with autocorrelation
plot(DATA$timestamp,mean(AKDE$DOF.area) * AKDE$weights,xlab='time',ylab="weight")
# How many points do you need for a autocorrelation-weighted RSF estimate?
## points near gaps have higher weights
# iRSF with autocorrelation: iterates until the default 1% error threshold
RSF <- rsf.fit(DATA,AKDE,R=R)  # post-hoc adjustment to fix assumption of independent sampling 
# if you don't have a time-dependent model, integrator="Riemann" is much faster
RSF <- rsf.fit(DATA,AKDE,R=R,integrator="Riemann")
## no reason to use Monte-Carlo integration (only if you want to integrate once per point in time)
## just integrate as you would from Calc2 (assume same integration for each time point)
## model is slightly different when using Riemann integration, but not significantly diff (unless the data is bad)

summary(RSF)
## effective sample size of ~11

## "Availability model" is some underlying Gaussian distribution that is fit at the same time as the RSF
### propagates uncertainty from one estimator to the other
### estimators correspond to two diff orders of selection (Gaussian for home range selection, RSF for tree cover)

# Advantages of rsf.fit() iRSFs over regular RSFs:
# * log-likelihood is down-weighted to account for autocorrelation and irregular sampling
# * available points are randomly sampled until numerical convergence
# * available area is estimated - uncertainty is propagated (iRSF)

# iRSF or iSSF, which to choose? (for iSSFs, see the 'amt' R package)
# * RSFs requires range residence, SSFs do not
# * SSFs can model fine scale selection, and may have larger DOFs for fine-scale data
#     DOF[RSF] ~ DOF[area]
#     DOF[SSF] ~ DOF[diffusion]
# * SSFs are discrete time and cannot handle irregular data
# * RSFs directly output utilization-distribution (UD) information (resource utilization and space utilization),
#     SSF selection parameters have a different meaning, and their UD is non-trivial

## rsf.select() can do model selection on multiple predictors
RSFS <- rsf.select(DATA,AKDE,R=R,formula=~I(sqrt(tree))+tree+I(tree^2),integrator="Riemann",verbose=TRUE,trace=TRUE)
summary(RSFS)

# selected model
RSF <- RSFS[[1]]
summary(RSF)  # ~3.5 selection strength for tree cover

treecover # 0-1 valued
# relative selection of tree cover versus no tree cover
exp( summary(RSF)$CI[1,] * (sqrt(1)-sqrt(0)) )
## btwn 3 and 363 preference for tree cover

# if you had more individuals and more significance (and transferable models)
help("mean.ctmm")

# The iRSF distribution that was fit
help("agde")

AGDE <- agde(DATA,RSF,R=R)
# note the finite available area that was estimated
plot(DATA,AGDE,main='iRSF')
## actual model fitted

# suitability maps
help("suitability")

SUIT <- suitability(DATA,CTMM=RSF,R=R,grid=AKDE)
names(SUIT) # brick with 3 layers (lower, point estimate, upper)
plot(DATA,error=2,R=SUIT[['est']],col.grid=NA,main="suitability")

# RSF-informed AKDE
help('akde')

RAKDE <- akde(DATA,RSF,R=R,weights=TRUE)  # kernel density dependence
plot(DATA,error=2,UD=RAKDE,col.grid=NA,main="iRSF-AKDE")

# you can also add boundaries at the kernel level