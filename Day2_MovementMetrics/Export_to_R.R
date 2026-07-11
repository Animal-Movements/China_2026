# extract R code
library(knitr)
file.exists("1_discrete_step_metrics.Rmd")
purl("1_discrete_step_metrics.Rmd")

file.exists("2_summary_movement_traits.Rmd")
purl("2_summary_movement_traits.Rmd")

# file.exists("3_ctmm_intro.Rmd")
# purl("3_ctmm_intro.Rmd")
# 
# file.exists("4_ctmm_speed.Rmd")
# purl("4_ctmm_speed.Rmd")