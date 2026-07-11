# extract R code
library(knitr)
file.exists("1_DataImport.Rmd")
purl("1_DataImport.Rmd")

file.exists("2_DataCleaning.Rmd")
purl("2_DataCleaning.Rmd")

file.exists("3_TrajectoryVisualization.Rmd")
purl("3_TrajectoryVisualization.Rmd")