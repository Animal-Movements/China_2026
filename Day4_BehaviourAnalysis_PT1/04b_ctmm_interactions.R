# This script details methods that can be used for studying
# interactions between individuals. This includes:
# - Home-range overlap
# - Encounter location distributions (CDE)
# - Pairwise distances
# - Proximity ratios
# - Encounter rates


# These analyses are conditional on fitted movement models and HR estimates
# (see: https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_akde.R)
library(ctmm)
data("buffalo")
projection(buffalo) <- median(buffalo)
load("Day4_BehaviourAnalysis_PT1/Data/buffalo.rda") # Fitted movement models; object called 'FITS'
load("Day4_BehaviourAnalysis_PT1/Data/buffalo_akdes.rda") # Estimated HR areas; object called 'AKDES'

#-----------------------------------------------------
# Home-range overlap
#-----------------------------------------------------

# Do individuals share the same space?
# Relevant paper: https://doi.org/10.1111/2041-210X.13027
help("overlap")  ## overlap of distributions (measure of similarities btwn distributions as % overlap of PMF)

#Estimate HR overlap for all pairs
#Note: these all must have compatible resolutions and alignments
OVER <- overlap(AKDES)  ## statistical measure of overlap
## (as compared to geometric overlap that depends on the % range chosen, usually 95%, and doesn't consider freq of space use)
OVER  ## has bias correction (things tend to look more dissimilar than they are, so overlap estimates are lower w/out bias correction)

# This will generate an error because of incompatible grids (due to pixel by pixel calculation)
overlap(list(akde(buffalo$Pepper, FITS$Pepper),
             akde(buffalo$Queen, FITS$Pepper)))

# But this works because HRs are estimated simultaneously (and consistently)
overlap(akde(list(buffalo$Pepper,buffalo$Queen),
             list(FITS$Pepper, FITS$Queen)))


# look at everything
OVER

# pairwise CIs 
OVER$CI["Pepper","Toni",]
OVER$CI["Queen","Toni",]

# point estimates
OVER$CI[,,"est"]


#-----------------------------------------------------
# Encounter location distributions (CDE)
#-----------------------------------------------------

# where encounters are expected to take place (assumption that indivs are moving independently when not encountering)

# Relevant paper: https://doi.org/10.1111/2041-210X.13597
help("cde")  # estimated area of where animals are likely to encounter each other


#Plot the data and HR estimates
plot(buffalo[c("Pepper", "Queen")],
     UD=AKDES[c("Pepper", "Queen")],
     col = c("#e76f51", "#264653"),
     col.UD=c("#f4a261", "#2a9d8f"),
     col.grid = NA)


#Estimate the home range overlap
overlap(AKDES[c("Pepper", "Queen")])


#Estimate the CDE (conditional distribution of encounters, where we expect majority of encounter to takep place)
CDE <- cde(AKDES[c("Pepper", "Queen")])  ## can weight indivs separately

#Visualise the CDE
plot(buffalo[c("Pepper", "Queen")],
     col=c("#e76f51", "#264653"),
     UD=CDE,
     col.UD="#046C9A",
     col.grid = NA)


#-----------------------------------------------------
# Pairwise proximity and distance metrics
#-----------------------------------------------------

# metrics that takes time into account (paper coming)
help("proximity")

#Pairwise separation distances
DISTS <- distances(buffalo[c("Cilla","Mvubu")],
                   FITS[c("Cilla","Mvubu")])
## predicts distance btwn 2 indivs at given time, might have location


#Visualise the separation distances
plot(DISTS$est ~ DISTS$timestamp,
     type = "l",
     col = "#5e548e")


# Internal plotting function (work in progress)
ctmm:::ts.plot(DISTS)

# what would totally independent motion look like?
cilla_sim <- simulate(FITS$Cilla, t = buffalo$Cilla$t)
mvubu_sim <- simulate(FITS$Mvubu, t = buffalo$Mvubu$t)

sim_dists <- distances(list(cilla_sim, mvubu_sim),
                       FITS[c("Cilla","Mvubu")])

#Plot the data
par(mfrow = c(2,2))
plot(buffalo[c("Cilla", "Mvubu")],
     col = c("#e76f51", "#264653"),
     main = "Empirical data")

plot(list(cilla_sim, mvubu_sim),
     col = c("#e76f51", "#264653"),
     main = "Simulated data")

plot(DISTS$est ~ DISTS$timestamp,
     type = "l",
     col = "#5e548e",
     main = "Empirical distances",
     ylab = "Distance (m)",
     xlab = "Time",
     ylim = c(0,max(sim_dists$est)))

plot(sim_dists$est ~ sim_dists$timestamp,
     type = "l",
     col = "#5e548e",
     main = "Simulated distances",
     ylab = "Distance (m)",
     xlab = "Time",
     ylim = c(0,max(sim_dists$est)))


# Proximity ratio (note: can be slow)
help('proximity')

PROXIMITY <- proximity(buffalo[c("Cilla","Mvubu")],
                       FITS[c("Cilla","Mvubu")])
load("Day4_BehaviourAnalysis_PT1/Data/buffalo_proximity.rda")  # ond statistic only works if they are moving tog or actually avoiding each
PROXIMITY  ## >1 = farther apart than each other <1 closer together than expected

# Proximity ratio for simulated animals
SIM_PROXIMITY <- proximity(list(cilla_sim, mvubu_sim),
                           FITS[c("Cilla","Mvubu")])
load("Day4_BehaviourAnalysis_PT1/Data/simulated_proximity.rda")
SIM_PROXIMITY

#-----------------------------------------------------
# Encounters
#-----------------------------------------------------

help("encounter")  ## encounters, encounter rates/freqs
# Relevant paper: https://doi.org/10.1101/2023.06.07.544097
## old measure is prox, which is very sensitive (looks at whether they're w/in a certain threshold distance of each other)
## but the issue is that this misses a lot of potential encounters if data is too coarse.

#Empirical encounters
DISTS$encounter <- ifelse(DISTS$est <= 100, 1, 0)

#Visualize the results
par(mfrow = c(1,1))
plot(DISTS$encounter ~ DISTS$timestamp)
cdplot(as.factor(DISTS$encounter) ~ DISTS$timestamp)

#Empirical Encounter rate (n/day)
n <- sum(DISTS$encounter)
t <- "day" %#% (DISTS$t[nrow(DISTS)] - DISTS$t[1])
n/t


#If you do this, run a sensitivity analysis
enc_rad <- 1:1000
N <- vector("numeric", 1000)
for(i in 1:length(enc_rad)){
  N[i] <- sum(ifelse(DISTS$est <= enc_rad[i], 1, 0))
}

#visualise the results
plot(N ~ enc_rad,
     ylab = "Encounters",
     xlab = "Encounter radius",
     type = "l",
     col = "#5e548e")


#Estimate relative encounter rates
RATES <- encounter(AKDES, method = "PDF")
RATES$CI["Cilla","Mvubu",] * 1000^2 # good for small distances (spend about 1.2% of their time tog)
tanh(sqrt(RATES$CI["Cilla","Mvubu",])*1000)^2 # more reliable
## strong assumption that when they're not together, they're avoiding each other
