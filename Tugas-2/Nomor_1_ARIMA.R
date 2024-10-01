library(forecast)
library(tidyverse)
library(ggplot2)
library(fpp2)
library(tseries)

dataExcel <-  readxl::read_excel("D:/Dataset/Tugas_1/Data Soal Tugas 1.xlsx", sheet="Soal 6",  skip=0)

#### Soal A
myTS_1 <- ts(dataExcel[,"Soal A"], frequency=1, start=c('1'))
summarise()

autoplot(myTS_1, main="Soal A", ylab="Value", xlab="time")+theme_light()
#plot(myTS_1, main="Soal A", ylab="Value", xlab="time", asp=1.0)
myAdf <- adf.test(myTS_1)
summary(myAdf)
diff_1 <- diff(myTS_1)
autoplot(diff_1)+theme_light()

#cek ACF dan PACF
acf_1 <- acf(myTS_1, main="ACF Soal A", lag.max = 40)
pacf_1 <- pacf(myTS_1, main="PACF Soal A", lag.max = 40)

#print(myTS_1)
myArima_1 <- auto.arima(myTS_1)
summary(myArima_1)
checkresiduals(myArima_1)


########################################
#### Soal B
myTS_2 <- ts(dataExcel[,"Soal B"], frequency=1, start=c('1'))
print(myTS_2)
#autoplot(myTS_2)
autoplot(myTS_2, main="Soal B", ylab="Value", xlab="Time")+theme_light()
adf.test(myTS_2)
diff_2 <- diff(myTS_2)
autoplot(diff_2, main="Differenced", xlab="Time", ylab="Diff")+theme_light()

#cek ACF dan PACF
acf_2 <- acf(myTS_2, main="ACF Soal B")
pacf_2 <- pacf(myTS_2, main="PACF Soal B")

myArima_2 <- auto.arima(myTS_2)
summary(myArima_2)
#autoplot(myArima_2)
checkresiduals(myArima_2)

