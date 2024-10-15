library(tibble)
library(readr)
library(ggplot2)
library(tseries)
library(keras)
library(tensorflow)

setwd("D:/Projects/R/time-series-analysis/Tugas-5")
data_dir <- "./jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
wrName <- file.path(data_dir, "jena_climate_5000.csv")
data <- read.csv(fname)

glimpse(data)

dataTmp <- (data[1:5000, ])
write.csv(dataTmp, wrName)


ggplot(data)

ggplot( data[1:100, ], 
        aes( 
             x = 1:100, 
             y = 'T..degC.'
            )
      ) + geom_line()

#plot(x=data$Date.Time, y=data$T..degC.)
#ggplot(data[1:1440,], aes(x = 1:1440, y = 'T (degC)')) + geom_line()

