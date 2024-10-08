library(tscount)
library(forecast)
library(dplyr)
library(neuralnet)
library(tseries)
library(zoo)
library(drat)
library(nnet)
library(quantmod)
library(rnn)

data("chicken", package = "astsa")
head(chicken)
tail(chicken)
ts.plot(chicken, xlab="Date", ylab = "Price (cents)", col="blue")
data <- chicken

pacf(data, lag.max = 46)
require(quantmod)
data <- as.zoo(data)
x1 <- Lag(data, k=1)
x2 <- Lag(data, k=2)
x3 <- Lag(data, k=3)
x4 <- Lag(data, k=4)
x <-cbind(x1,x2,x3,x4,data)
x <- log(x)
head(round(x,2))

x <- x[-(1:4) ,]
head(round(x,2))

x <- data.matrix(x)
range_data <- function(x){(x-min(x))/(max(x)-min(x))}
min_data <- min(x)
max_data <- max(x)
x <- range_data(x)

max(x)
min(x)

x1 <- as.matrix(x[,1])
x2 <- as.matrix(x[,2])
x3 <- as.matrix(x[,3])
x4 <- as.matrix(x[,4])
y <- as.matrix(x[,5])

n_train <-170
y_train <- as.matrix(y[1:n_train])
x1_train <- as.matrix(t(x1[1:n_train ,]))
x2_train <- as.matrix(t(x2[1:n_train ,]))
x3_train <- as.matrix(t(x3[1:n_train ,]))
x4_train <- as.matrix(t(x4[1:n_train ,]))

x_train_array <- array(c(x1_train, x2_train, x3_train, x4_train), 
                 dim = c(dim(x_train)))

dim(x_train_array)
print(x_train_array)
print(y_train)

require(rnn)
set.seed(2018)
rnn <- trainr(Y=t(y_train),
                 X= x_train_array,
                 learningrate = 0.05,
                 hidden_dim = 3,
                 numepochs = 500,
                 network_type = "rnn",
                 sigmoid = "logistic")

str(rnn)
error_1 <- (rnn$error)
str(rnn$error)

predictionrnn <- t(predictr(rnn, x_train))
round(cor(y_train, predictionrnn) ,5)

##model 2
set.seed(2018)
rnn2 <-trainr(Y=t(y_train),
              X= x_train_array,
              learningrate = 0.05,
              hidden_dim = c(3,2),
              numepochs = 500,
              network_type = "rnn",
              sigmoid = "logistic")
prediction2 <- t(predictr(rnn2, x_train))
round(cor(y_train, prediction2) ,5)

#test set
x1_test <- as.matrix(t(x1[(n_train+1):nrow(x1),]))
x2_test <- as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test <- as.matrix(t(x3[(n_train+1):nrow(x3),]))
x4_test <- as.matrix(t(x4[(n_train+1):nrow(x4),]))
y_test <- as.matrix(y[(n_train+1):nrow(x4)])

print(y_test)

x_test_array <- array(c(x1_test, x2_test, x3_test, x4_test), 
                       dim = c(dim(x1_test), 4))

dim(x_test_array)
pred1_test <- t(predictr(rnn, x_test_array))
pred2_test <- t(predictr(rnn2, x_test_array))

#unscalling data
unscale_data <- function(x, max_x, min_x)
{x*(max_x - min_x)+min_x}

predictionrnn <- unscale_data(pred1_test, max_data, min_data)
predictionrnn <- exp(predictionrnn)
predictionrnn <- ts (matrix(predictionrnn), end = c(2016, 7), frequency = 12)
plot(predictionrnn, type="l", col="red", xlab="time", ylab="harga", main="nilai prediksi model 1")

prediction2 <- unscale_data(pred2_test, max_data, min_data)
prediction2 <- exp(prediction2)
prediction2 <- ts (matrix(prediction2), end = c(2016, 7), frequency = 12)
plot(prediction2, type="l", col="blue", xlab="time", ylab="harga", main="nilai prediksi model 2")

y_actual <- unscale_data(y_test, max_data, min_data)
y_actual <- exp(y_actual)
y_actual <- ts(matrix(y_actual), end = c(2016, 7), frequency = 12)

result_all <- cbind (y_actual, round(predictionrnn, 2), round(prediction2, 2))
colnames(result_all) <- c("actual", "Model 1", "Model 2")
result_all

# 8. Menghitung Mean Squared Error (MSE) model 2
mse <- mean((y_actual - prediction2)^2)
print(paste("Mean Squared Error:", mse))
rmse <- sqrt(mse)
print(paste("RMSE :", rmse))
mean_actualtesting <- mean(y_actual)
tss <- sum((y_actual - mean_actualtesting)^2)
rss <- sum((y_actual - prediction2)^2)
r_squaredTesting <- 1 - (rss/tss)
print(paste("R-squared:", r_squaredTesting))

# 8. Menghitung Mean Squared Error (MSE) model 1
mse <- mean((y_actual - predictionrnn)^2)
print(paste("Mean Squared Error:", mse))
rmse <- sqrt(mse)
print(paste("RMSE :", rmse))
mean_actualtesting <- mean(y_actual)
tss <- sum((y_actual - mean_actualtesting)^2)
rss <- sum((y_actual - predictionrnn)^2)
r_squaredTesting <- 1 - (rss/tss)
print(paste("R-squared:", r_squaredTesting))


