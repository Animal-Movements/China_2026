# extract R code
library(knitr)
file.exists("2_HMMs.Rmd")
purl("2_HMMs.Rmd")

file.exists("3_HMM_accelerometer.Rmd")
purl("3_HMM_accelerometer.Rmd")