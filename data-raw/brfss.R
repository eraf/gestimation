## code to prepare `brfss` dataset goes here
load('dataset/brfss.RData')
usethis::use_data(brfss, overwrite = TRUE)
