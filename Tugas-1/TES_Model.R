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

# tes model
#function (y, h = 2 * frequency(x), seasonal = c("additive", 
#          "multiplicative"), damped = FALSE, level = c(80, 95), fan = FALSE, 
#          initial = c("optimal", "simple"), exponential = FALSE, alpha = NULL, 
#          beta = NULL, gamma = NULL, phi = NULL, lambda = NULL, biasadj = FALSE, 
#          x = y, ...) 
  
ses_model_1 <- forecast::hw(myts)
ses_model_2 <- forecast::hw(myts, alpha = 0.2, beta = 0.3, initial = c("simple"))

summary(ses_model_1)
summary(ses_model_2)

plot(ses_model_1)
plot(ses_model_2)


