library(forecast)
library(tidyverse)
library(ggplot2)
library(fpp2)
library(swfscMisc)

#dataExcel <-  readxl::read_excel("D:/Dataset/Tugas_1/Data Soal Tugas 1.xlsx", sheet="Soal 8",  skip=0)
#summarise(dataExcel)
#myTS <- ts(dataExcel[,"Jan."], frequency=13 )
#print(myTS)

myDataValues <- c(580, 514, 555, 563, 627, 596, 632, 639, 577, 611, 639, 875,
                  650, 594, 650, 668, 712, 731, 779, 712, 708, 738, 758, 1073,
                  669, 652, 743, 709, 751, 774, 803, 760, 749, 757, 779, 1066,
                  734, 707, 785, 762, 838, 876, 878, 871, 807, 834, 877, 1236,
                  789, 744, 827, 831, 895, 889, 955, 983, 976, 929, 989, 1294,
                  860, 799, 899, 866, 1016, 978, 1042, 1026, 944, 1002, 1009, 1368,
                  908, 849, 916, 958, 1008, 1033, 1129, 1019, 984, 1045, 1049, 1459,
                  910, 927, 981, 1011, 1041, 1080, 1138, 1072, 1033, 1072, 1111, 1591,
                  950, 932, 1049, 1021, 1097, 1151, 1194, 1174, 1160, 1135, 1209, 1692,
                  1071, 1044, 1158, 1122, 1209, 1334, 1360, 1368, 1297, 1283, 1375, 1974,
                  1294, 1258, 1301, 1297, 1425, 1378, 1429, 1452, 1305, 1377, 1439, 1958)

myTS <- ts(myDataValues, start = c(1970, 1), frequency = 12)
print(myTS)
autoplot(myTS, main="US Liquor Sales", xlab="Year", ylab="million dollars")+ theme_light()
adf.test(myTS, k = 12)

myDiff <- diff(myTS, differences = 1)
print(myDiff)
autoplot(myDiff)

acf(myTS)
pacf(myTS)

### Manual 
myArima_1 <- arima(myTS, order = c(0, 1, 1), seasonal = list(order=c(1,1,0), period=12))
myForecast <- forecast(myArima_1, h = 12)
autoplot(myForecast, main="Forecast next 12 month", xlab="Year", ylab="million dollars")+theme_light()
myForecast <- forecast(myArima_1, h = 24)
autoplot(myForecast, main="Forecast next 24 month", xlab="Year", ylab="million dollars")+theme_light()

### Auto ARIMA
mySArimaModel <- auto.arima(myTS, seasonal = TRUE)
print(mySArimaModel)
myForecast <- forecast(mySArimaModel, h = 12)
print(myForecast)
autoplot(myForecast, main="Forecast next 12 month", xlab="Year", ylab="million dollars")+theme_light()


myForecast <- forecast(mySArimaModel, h = 24)
print(myForecast)
autoplot(myForecast, main="Forecast next 24 month", xlab="Year", ylab="million dollars")+theme_light()
