# extract R code
library(knitr)
file.exists("01_environmental_annotation.Rmd")
purl("01_environmental_annotation.Rmd")

file.exists("2_HMMs.Rmd")
purl("2_HMMs.Rmd")

file.exists("3_HMM_accelerometer.Rmd")
purl("3_HMM_accelerometer.Rmd")

file.exists("3_HMM_accelerometer_internal.Rmd")
purl("3_HMM_accelerometer_internal.Rmd")
