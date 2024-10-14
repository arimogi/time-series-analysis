library(tibble)
library(readr)

data_dir <- "./jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read.csv(fname)
