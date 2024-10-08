library(tscount)
library(forecast)
library(dplyr)
library(neuralnet)
library(tseries)
library(zoo)
library(drat)
library(nnet)
library(quantmod)
data("ecoli", package = "tscount")
head(ecoli)

data <- ecoli$cases
data <- as.numeric(unlist(ecoli[3]))
data <- ts(matrix(data), start = c(2001,1), end = c(2013,20), frequency = 52)

#visualisasi data
ts.plot(data, xlab="Date", ylab="Number of cases")
pacf(data, lag.max = 46)

# 1. Menyiapkan data dengan lag 1, 2, 3, dan 4
x1 <- coredata(Lag(data, k = 1))
x2 <- coredata(Lag(data, k = 2))
x3 <- coredata(Lag(data, k = 3))
x4 <- coredata(Lag(data, k = 4))

# Gabungkan data lag dengan target (nilai aslinya)
data_lagged <- cbind(x1, x2, x3, x4, data = coredata(data))
print(data_lagged)
# Hapus NA (karena lag menghasilkan NA di beberapa awal pengamatan)
data_lagged <- na.omit(data_lagged)
data_lagged <- as.data.frame(data_lagged)

# 2. Normalisasi data
maxs <- apply(data_lagged, 2, max)
mins <- apply(data_lagged, 2, min)
normalized_data <- as.data.frame(scale(data_lagged, center = mins, scale = maxs - mins))

# 3. Membagi data menjadi training dan test set
n <- nrow(normalized_data)
train_size <- round(n * 0.8)
train <- normalized_data[1:train_size, ]
test <- normalized_data[(train_size + 1):n, ]

# 4. Membuat model neural network dengan input lag 1, 2, 3, dan 4
formula_nn <- as.formula("data ~ x1 + x2 + x3 + x4")
set.seed(2018)
nn <- neuralnet(formula_nn, data = train, hidden = c(5), linear.output = FALSE, learningrate = 0.07)

# Visualisasi model
plot(nn)

pred1_train = predict(nn, train)
ts.plot(pred1_train)

#mengembalikan skala 
predicted_values1 <- pred1_train * (max(data_lagged$data) - min(data_lagged$data)) + min(data_lagged$data)
actual_values1 <- train$data * (max(data_lagged$data) - min(data_lagged$data)) + min(data_lagged$data)

plot(predicted_values1, type = "l", col = "red", xlab = "Time", ylab = "Nilai", main = "Actual vs Predicted on Data Training")
lines(actual_values1, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1)

# Menghitung Mean Squared Error (MSE)
mse <- mean((actual_values1 - predicted_values1)^2)
print(paste("Mean Squared Error:", mse))
rmse <- sqrt(mse)
print(paste("RMSE :", rmse))
mean_actual <- mean(actual_values1)
tss <- sum((actual_values1 - mean_actual)^2)
rss <- sum((actual_values1 - predicted_values1)^2)
r_squared <- 1 - (rss/tss)
print(paste("R-squared:", r_squared))

# 5. Melakukan prediksi pada test set
test_input <- subset(test, select = -data)
nn_predictions <- compute(nn, test_input)$net.result

# 6. Mengembalikan skala hasil prediksi ke skala asli
predicted_values <- nn_predictions * (max(data_lagged$data) - min(data_lagged$data)) + min(data_lagged$data)
actual_values <- test$data * (max(data_lagged$data) - min(data_lagged$data)) + min(data_lagged$data)

# 7. Visualisasi hasil prediksi vs data aktual
plot(actual_values, type = "l", col = "red", xlab = "Time", ylab = "Nilai", main = "Actual vs Predicted on Data Testing")
lines(predicted_values, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1)

# 8. Menghitung Mean Squared Error (MSE)
mse <- mean((actual_values - predicted_values)^2)
print(paste("Mean Squared Error:", mse))
rmse <- sqrt(mse)
print(paste("RMSE :", rmse))
mean_actualtesting <- mean(actual_values)
tss <- sum((actual_values - mean_actualtesting)^2)
rss <- sum((actual_values - predicted_values)^2)
r_squaredTesting <- 1 - (rss/tss)
print(paste("R-squared:", r_squaredTesting))


