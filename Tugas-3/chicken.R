library("forecast")
library("dplyr")
library("neuralnet")
library("rnn")

myFolder <- getwd()
print(myFolder)
setwd("D:/Documents/TD Analisis Data Deret Waktu/")
myFolder <- getwd()
print(myFolder)

data <- read.table("chicken.txt")
