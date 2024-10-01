#install.packages("tscount")
#install.packages("forecast")
#install.packages("dplyr")
#install.packages("neuralnet")
#install.packages("rnn")

library("tscount")
library("forecast")
library("dplyr")
library("neuralnet")

data("ecoli")
# data <- as.numeric(unlist(ecoli[3]))
data <- ts(matrix(data), start = c(2001,1), end = c(2013,20), frequency = 52)

plot(data, xlab="Date", ylab="Number of cases", col="darkred")

acf(data)
pacf(data)

ecoli_data <- data.frame(
  lag1 = lag(ecoli, 1),  # Lag 1
  lag2 = lag(ecoli, 2),  # Lag 2
  current = ecoli  # Nilai E. coli saat ini
)
ecoli_data <- ecoli_data %>% na.omit()

# Split data menjadi training dan testing
set.seed(123)  # seed untuk pertama
index <- sample(1:nrow(ecoli_data), size = 0.8 * nrow(ecoli_data))
train_data <- ecoli_data[index, ]
test_data <- ecoli_data[-index, ]

set.seed(123)
ffnn_model <- neuralnet(
  current ~ lag1 + lag2,  
  data = train_data,
  hidden = c(5, 3),  
  linear.output = TRUE
)

plot(ffnn_model)

ffnn_predictions <- compute(ffnn_model, test_data[, c("lag1", "lag2")])
predicted_values <- ffnn_predictions$net.result

result <- data.frame(
  Actual = test_data$current,
  Predicted = predicted_values
)

print(result)

mse <- mean((result$Actual - result$Predicted)^2)
cat("Mean Squared Error (MSE):", mse)
