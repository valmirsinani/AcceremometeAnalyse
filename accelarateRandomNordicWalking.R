# Step 1: Import the accelerometer data
accel_data <-  read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')

# Step 3: Set threshold parameters
threshold_high <- 1.2  # Upper threshold for step detection
threshold_low <- 0.8   # Lower threshold for step detection
window_size <- 10      # Size of the sliding window

num_points <- nrow(accel_data)
step_indices <- c()

for (i in 1:num_points) {
  start <- max(1, i - window_size + 1)
  end <- min(num_points, i + window_size - 1)
  
  current_window <- accel_data[start:end, "y"]
  
  if (max(current_window) > threshold_high && min(current_window) < threshold_low) {
    step_indices <- c(step_indices, i)
  }
}


# Step 5: Count the number of steps
num_steps <- length(step_indices)
cat("Number of Nordic walking steps detected:", num_steps)
library('lubridate')
op <- options(digits.secs=0)
datetime_dt <- as.POSIXct(accel_data$dt, format = "%Y-%m-%dT%H:%M:%OS")

timestamps <- datetime_dt  # Convert timestamp to POSIXct format


# Step 6: Visualize and analyze the results
plot(timestamps, accel_data$y, type = "l", xlab = "timestamp", ylab = "Acceleration Y")
points(timestamps[step_indices], accel_data$y[step_indices], col = "red")


