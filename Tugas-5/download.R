setwd("D:/Projects/R/time-series-analysis/Tugas-5")
dir.create("./jena_climate", recursive = TRUE)

download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "./jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "./jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "./jena_climate"
)
