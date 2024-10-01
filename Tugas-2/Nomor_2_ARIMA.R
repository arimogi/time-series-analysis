library(forecast)
library(ggplot2)
library(tidyverse)
library(fpp2)

dataExcel <-  readxl::read_excel("D:/Dataset/Tugas_1/Data Soal Tugas 1.xlsx", sheet="Soal 7B",  skip=0)

myTS <- ts(dataExcel[,"Production"], start=c('1'), frequency=1)
autoplot(myTS)+ggtitle("Production")+xlab("Year")+ylab("Production")+theme_light()

myArima <- auto.arima(myTS)
summary(myArima)
print(myArima)

myForecast <- forecast(myArima, h=4)
print(myForecast)
autoplot(myForecast, main="Forecast Production 4 years", ylab="Production", xlab="Year")+theme_light()

####################
# 1983 dengan 34.6 #
####################
dataExcel <-  readxl::read_excel("D:/Dataset/Tugas_1/Data Soal Tugas 1.xlsx", sheet="Soal 7C",  skip=0)

myTS <- ts(dataExcel[,"Production"], start=c('1'), frequency=1)
autoplot(myTS)+ggtitle("Production")+xlab("Year")+ylab("Production")+theme_light()

myArima <- auto.arima(myTS)
summary(myArima)

myForecast <- forecast(myArima, h=4)
print(myForecast)
autoplot(myForecast, main="Forecast Production", ylab="Production", xlab="Year")+theme_light()
residuals(myForecast)
