#Windowed Peak Detection
library(dplyr)

accel_data <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')

#accel_data %>% as.numeric(accel_data)


#nrow(filtered_data)
accel_data <- na.omit(accel_data)

filtered_data <- accel_data[complete.cases(accel_data), ]

#accel_data <- data.frame(subset(accel_data, is.na(accel_data$x) > FALSE & is.na(accel_data$y) > FALSE))

# Step 3: Convert data to numeric
#accel_data <- data.frame(lapply(accel_data$x, as.numeric))

#accel_data <- data.frame(lapply(accel_data$y, as.numeric))

#accel_data <- data.frame(lapply(accel_data$z, as.numeric))


accel_data <- accel_data[!is.na(accel_data), ]

nrow(accel_data)

# Step 3: Set window parameters
window_size <- 10  # Size of the sliding window
threshold <- 0.5  # Minimum peak height threshold

# Step 4: Perform windowed peak detection for each axis
num_points <- nrow(accel_data)
peak_indices <- c()

for (i in 1:num_points) {
  start <- max(1, i - window_size + 1)
  end <- min(num_points, i + window_size - 1)
  
  current_window_x <- accel_data[start:end, "x"]
  current_window_y <- accel_data[start:end, "y"]
  current_window_z <- accel_data[start:end, "z"]
  
  current_peak_x <- accel_data[i, "x"]
  current_peak_y <- accel_data[i, "y"]
  current_peak_z <- accel_data[i, "z"]
  
  if (
    current_peak_x == max(current_window_x) && current_peak_x >= threshold &&
    current_peak_y == max(current_window_y) && current_peak_y >= threshold &&
    current_peak_z == max(current_window_z) && current_peak_z >= threshold
  ) {
    peak_indices <- c(peak_indices, i)
  }
}
#
library('lubridate')
op <- options(digits.secs=0)
datetime_dt <- as.POSIXct(accel_data$dt, format = "%Y-%m-%dT%H:%M:%OS")
accel_data$dt <-  datetime_dt_format <-   paste(day(datetime_dt),hour(datetime_dt),minute(datetime_dt),sep="-") 
# Step 5: Count the number of peaks
num_peaks <- length(peak_indices)
cat("Number of peaks detected:", num_peaks)
#

 par(mfrow = c(1, 3)) 

# Step 6: Visualize and analyze the results
plot(accel_data$dt, accel_data$x, type = "l", xlab = "Timestamp", ylab = "Acceleration x")
points(accel_data$dt[peak_indices], accel_data$x[peak_indices], col = "red")

plot(accel_data$dt, accel_data$y, type = "l", xlab = "Timestamp (m)", ylab = "Acceleration y")
points(accel_data$dt[peak_indices], accel_data$y[peak_indices], col = "red")

plot(accel_data$dt, accel_data$z, type = "l", xlab = "Timestamp (m)", ylab = "Acceleration z")
points(accel_data$dt[peak_indices], accel_data$z[peak_indices], col = "red")
