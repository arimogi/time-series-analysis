#install.packages("jsonlite")
#install.packages("tscount")
#install.packages("quantmod")
#install.packages("httpgd")
#install.packages("tscount")
#install.packages("forecast")
#install.packages("dplyr")
#install.packages("neuralnet")
#install.packages("rnn")

# Load necessary libraries
library(tscount)
library(forecast)
library(dplyr)
library(neuralnet)

# Load the E. coli data from tscount package
data("ecoli", package = "tscount")

# Check the structure of the data
head(ecoli)

# Convert the 'cases' column to a time series object
data <- ts(ecoli$cases, start=c(2001,1), frequency=52)

# Plot the time series data
plot(data, xlab="Date", ylab="Number of Cases", col="darkblue")

# ACF and PACF plots
acf(data)
pacf(data)

# Normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Create lag features and normalize the data
ecoli_data <- ecoli %>%
  mutate(
    lag1 = lag(cases, 1),
    lag2 = lag(cases, 2),
    current = cases  # Keep the original cases as current
  ) %>%
  na.omit()  # Remove rows with NA values due to lagging

# Normalize the lag features and current values
ecoli_data <- ecoli_data %>%
  mutate(
    lag1 = normalize(lag1),
    lag2 = normalize(lag2),
    current = normalize(current)
  )

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(seq_len(nrow(ecoli_data)), size = 0.8 * nrow(ecoli_data))
train_data <- ecoli_data[train_indices, ]
test_data <- ecoli_data[-train_indices, ]

# Build and train the Feed-Forward Neural Network model
ffnn_model <- neuralnet(
  current ~ lag1 + lag2,  # Predict 'current' using lag features
  data = train_data,
  hidden = c(5, 3),  # Two hidden layers with 5 and 3 neurons
  linear.output = TRUE
)

# Plot the neural network model
plot(ffnn_model)

# Make predictions on the test set
predictions <- compute(ffnn_model, test_data[, c("lag1", "lag2")])$net.result

# Evaluate model performance
actual <- test_data$current
predicted <- predictions

# Calculate Mean Squared Error (MSE)
mse <- mean((actual - predicted)^2)
print(paste("Mean Squared Error:", mse))
