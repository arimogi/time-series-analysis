# Load library, install if not found
library(forecast)
library(tidyverse)
library(fpp2)

# Load excel data
retaildata <- readxl::read_excel("d:/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))

# buat grafik dan hasil
autoplot(myts)
summary(myts)

data_decomp <-decompose(myts, type = c("multiplicative"))

summary(data_decomp)
plot(data_decomp)



