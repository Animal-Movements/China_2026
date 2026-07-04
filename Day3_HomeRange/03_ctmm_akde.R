##########
# AKDE
##########

library(ctmm)

help("akde") # main function and new `pkde()` function for population-wide data (tracked multiple indivs)
## kernel density methods (non-parametric), best for if model isn't known
## debiasing: conditioned on autocorrelation model, not IID
### default kde puts equal weights for all points, but akde is weighted nonparametric method (weights are optimized
### -- less weight on over-sampled points and more weight on under-sampled points (# weights (optimization parameters) = # datapoints)
help("bandwidth")
## bandwidth = spread of kernels
## kernels will spillover into areas where animal can't go, can set boundaries if kernels are smaller than the polygon
# uses Gaussian optimization function

# load buffalo data
data(buffalo)
projection(buffalo) <- median(buffalo)  

names(buffalo)

# here we will work with Pepper
DATA <- buffalo$Pepper

COL <- color(DATA,by="time")
plot(DATA,col=COL,main="Pepper")

# this dataset has problems
dt.plot(DATA)
## time intervals in data sorted by size, most of the sampling intervals are 2 hours, some greater, some around 1 hour
## collar malfunctioned a lot (ideally it would be a flat line at the desired interval)

# selected autocorrelation model
GUESS <- ctmm.guess(DATA,interactive=FALSE)
FIT <- ctmm.select(DATA,GUESS,trace=3)
# save(FIT,file="pepper.rda")
# I've already run this
load("Day3_HomeRange/Data/pepper.rda")

summary(FIT)
## velocity autocorrelation ~ 30-40 mins
## position autocorrelation ~ 7-24 days

# analogous IID model
IID <- ctmm.fit(DATA)

summary(IID)

# regular KDE
KDE <- akde(DATA,IID) ## with IID model
## anisotropic kernel
## wouldn't want symmetric kernels here (want elongated kernels)

# default AKDE
AKDE <- akde(DATA,FIT) ## with autocorrelation model

# optimally weighted AKDE
wAKDE <- akde(DATA,FIT,weights=TRUE)
# you only need this with irregular sampling (i.e. Pepper's collar malfunction for sampling times) - can be slow
# unweighted AKDE places too much density on oversampled times

# Pepper's optimal weights
plot(DATA$timestamp,wAKDE$weights,xlab="time",ylab="weight",main="Optimal Weights")

plot(DATA$timestamp,wAKDE$weights,xlab="time",ylab="weight",main="Optimal Weights",ylim=c(0,0.005))
## more weight ascribed to 2 hour data
## if there are large gaps in the weights of data, the higher weighted/more unique datapoints are more valuable

# matching extent for plotting
EXT <- extent(list(KDE,AKDE,wAKDE))  # useful to determine tiles for sensing data

plot(DATA,KDE,ext=EXT,main="KDE")
# note CIs, grid, etc...
summary(KDE) ## RE: IID model
## can't see the CIs due to high assumed certainty from high effective sample size (area in DOF)

plot(DATA,AKDE,ext=EXT,main="AKDE")
summary(AKDE)
# Larger point estimates and can see grid much more (scale + orientation of bandwidth)
# interpret like a histogram
## larger CIs due to lower effective sample size

plot(DATA,wAKDE,ext=EXT,main="optimally weighted AKDE")
summary(wAKDE)
## Note diff shape for akde: 1-hour data further up (less weight), more weight on the 2-hour sampling lower down
## more representative visualization of movement data instead of sampling data

# Over-smoothing bias
osAKDE <- akde(DATA,FIT,weights=TRUE,debias=FALSE) 
## no over-smoothing bias correction (Gaussian reference function akde over-smooths)

plot(DATA,osAKDE,ext=EXT,main="uncorrected wAKDE")
## makes CIs too big

###########################
# Home-range meta-analysis
###########################

help("meta")
## meta-analysis: hierarchical model that propagates indiv parameters into population-wide and conducts model selection

FITS <- list()
for(i in 1:length(buffalo))
{
  GUESS <- ctmm.guess(buffalo[[i]],interactive=FALSE)
  FITS[[i]] <- ctmm.select(buffalo[[i]],GUESS,trace=3)
}
names(FITS) <- names(buffalo)
# save(FITS,file="data/buffalo.rda")
load("Day3_HomeRange/Data/buffalo.rda")

# calculate AKDES on a consistent grid
AKDES <- akde(buffalo,FITS,weights=TRUE)
# save(AKDES,file="data/buffalo_akdes.rda")
load("Day3_HomeRange/Data/buffalo_akdes.rda")

# color to be spatially distinct
COL <- color(AKDES,by='individual')
## indivs closer together spatially, have more distinct colours

# plot AKDEs
plot(AKDES,col.UD=COL,col.level=COL,col.grid=NA,level=NA,main="African buffalo AKDEs")

# Mean buffalo HR "the old way"
AREA <- vector("numeric", length = length(AKDES))
for(i in 1:length(AKDES))
{ AREA[i] <- summary(AKDES[[i]], units = FALSE)$CI[2] } # turn off units when making tables (units will be diff)
AREA
mean(AREA) # mean
sqrt(var(AREA)/length(AREA)) # SE


help('meta',package="ctmm")

# meta-analysis of buffalo home-range areas
meta(AKDES,col=c(COL,'black'),sort=TRUE)
# model selection: Dirac-delta > inverse-Gaussian for pop-level parameters
### -- here Dirac-delta singular (no variance), can't estimate st.dev/mean (can if turning of model selection, but not selected feature)
## Forest plot
## indiv HR estimates (a lot of uncertainty), will overestimate variance (can't distinguish sampling error from statistical error)
## hierarchical meta-analysis model that better estimates mean (standard is normal mean, normal variance, but here we don't use that)

# force inverse-Gaussian population distribution
meta(AKDES,plot=FALSE,IC=NA)
# since CoV isn't a selected feature, its underestimated here

# comparing sub-groups (north vs south don't look significantly different)
BUFFALO <- list(South=AKDES[1:3],North=AKDES[4:6])
META <- meta(BUFFALO)

META
META['South/','/North',]  # ratio of mean southern HR to mean northern HR
# not significantly diff (CIs overlap)

# more general meta-analytic regressions
help("Log") ## log-transform to make estimates more normal for `metafor`
# then you can use the 'metafor' R package
Log(FITS, variable = "speed")


#########################
# Population density
#########################

# this is a straight mean of the individual densities that doesn't model population variance
help("mean.UD")
# note the 'sample' argument for correct CIs
# Ex. want to average summer and winter ranges or have indiv that switches btwn nests (estimate separately then average)

# straight mean - for a population of 6 buffalo
MEAN <- mean(AKDES,sample=FALSE)

plot(buffalo,MEAN,col=COL,main="Mean African buffalo AKDE")

# this is a population kernel density estimate (paper coming)
help("pkde")  # population KDE: bandwidth optimization after choosing hierarchical model

PKDE <- pkde(buffalo,AKDES)  # runs mean on fitted movement models (meta-analysis hierarchical model)
## does model selection across many parameters due to all the parameters for each indiv
## tests which correlations btwn parameters can be supported (outputs delta AIC)

plot(buffalo,PKDE,col=COL,main="African buffalo PKDE")
## much larger and much more uncertain due to low sampling (but at least doesn't bias too small)
## other methods tend to estimate too small or look at saturation curves of crude pop range estimate



