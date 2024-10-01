# Load necessary libraries
library(keras)

# Load the data (make sure chicken data is loaded correctly)
load("C:/Users/amr/Documents/KULIAH/S3/Semester 1/Deret Waktu/R Code/chicken.rda")

# Create a time series object
chicken_ts <- ts(chicken, start=c(2001, 8), frequency=12)

# Plot the time series
plot(chicken_ts, main="Whole Bird Spot Price (US Cents per Pound)", ylab="Price", xlab="Year")

# Split the data into training and testing sets
train_length <- round(length(chicken_ts) * 0.8)  # 80% training data
train_data <- window(chicken_ts, end=c(2001 + (train_length-1)/12))
test_data <- window(chicken_ts, start=c(2001 + train_length/12))

plot(train_data, main="Training Data", ylab="Price", xlab="Year")

# Prepare the data for RNN input (create lagged sequences)
lag_transform <- function(x, lag=1){
  lagged <- c(rep(NA, lag), x[1:(length(x) - lag)])
  return(lagged)
}

# Create lagged input features for RNN
train_data_lagged <- lag_transform(train_data, lag=1)
train_input <- train_data_lagged[-1]  # Remove first NA
train_output <- train_data[-1]  # Remove first value to align with input

# Reshape the data for RNN input (3D array for RNN input)
X_train <- array(train_input, dim = c(length(train_input), 1, 1))  # (samples, timesteps, features)
y_train <- array(train_output, dim = c(length(train_output), 1))  # (samples, output)

# Use the Input layer explicitly in the Sequential model
model <- keras_model_sequential() %>%
  layer_input(shape = c(1, 1)) %>%  # Only `shape` is defined, no conflict with `batch_shape`
  layer_simple_rnn(units = 50) %>%  # The RNN layer
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

# Train the model
history <- model %>% fit(
  X_train, y_train,  # Use X_train and y_train with proper shapes
  epochs = 100,
  batch_size = 1,
  verbose = 1
)

# Prepare test data for prediction
test_data_lagged <- lag_transform(test_data, lag=1)[-1]
X_test <- array(test_data_lagged, dim = c(length(test_data_lagged), 1, 1))

# Make predictions
predictions <- model %>% predict(X_test)

# Plot actual vs predicted values
plot(test_data, main="Actual vs Predicted", col='blue', type='l')
lines(c(rep(NA, length(train_data)), predictions), col='red', type='l')
legend("topleft", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1)
