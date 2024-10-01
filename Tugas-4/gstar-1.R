library("gstar")

myFolder <- getwd()
print(myFolder)
setwd("D:/R/time-series-analysis/Tugas-4/")
myFolder <- getwd()
print(myFolder)

#Data <- read.table("oil-production.txt", header = TRUE)
Data <- read.csv("Rainfall3Lokasi-GSTAR.txt", header = TRUE)
#is.data.frame(Data)
ts.plot(Data$Juanda)
ts.plot(Data$Perak1)
ts.plot(Data$Perak2)

tt_ratio = round(nrow(Data) * 0.8)
Data_train = Data[1:s, ]
Data_test  = Data[-c(1:s), ]

W <- matrix( c(0,   0.5, 0.5, 
               0.5, 0,   0.5, 
               0.5, 0.5, 0), 
             3, 3)
W = W/(ncol(Data)-1)

fit <- gstar(Data, weight = W, 1, 1, est="OLS")
summary(fit)
performance(fit, Data_test)
predict(fit, n=2)

plot(fit)
plot(fit, Data_test)

