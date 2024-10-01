# Load library, install if not found
library(forecast)
library(tidyverse)
library(fpp2)

# Contoh data time series
data <- ts(c(100, 102, 103, 105, 106, 107, 110, 112, 113), frequency = 1)

# Load excel data
retaildata <- readxl::read_excel("d:/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))

# buat grafik dan hasil
autoplot(myts)
summary(myts)

# ses model
ses_model_1 <- forecast::holt(myts)
ses_model_2 <- forecast::holt(myts, alpha = 0.2, beta = 0.3, initial = "simple")

summary(ses_model_1)
summary(ses_model_2)

plot(ses_model_1)


