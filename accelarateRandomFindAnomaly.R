
accelerometer_data <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT10000.csv", sep = ',')

#

 

library('lubridate')
op <- options(digits.secs=0)
datetime_dt <- as.POSIXct(accelerometer_data$dt, format = "%Y-%m-%dT%H:%M:%OS")
seconddt <-  toString( second(datetime_dt))
accelerometer_data$dt <- day(datetime_dt)*24*60 + hour(datetime_dt)*60 + minute(datetime_dt)

#
par(mfrow = c(1, 2)) # Divide the plot area into a 2x3 gri
plot(accelerometer_data$dt, accelerometer_data$z, type = "l", xlab = "Timestamp", ylab = "Acceleration")


detect_anomalies <- function(data, threshold = 1.2) {
  z_scores <- scale(data)  # Compute z-scores of the data
  anomalies <- abs(z_scores) > threshold  # Identify anomalies based on z-score threshold
  return(anomalies)
}

anomaliesx <- detect_anomalies(accelerometer_data$x)

anomaliesy <- detect_anomalies(accelerometer_data$y)
anomaliesz <- detect_anomalies(accelerometer_data$z)


anomaly_threshold <- 1.2

plot(accelerometer_data$dt , accelerometer_data$y, type = "l", xlab = "Timestamp", ylab = "Acceleration 2")
points(accelerometer_data$x[anomaliesx], accelerometer_data$y[anomaliesx], col = "red")

points(accelerometer_data$y[anomaliesy], accelerometer_data$z[anomaliesy], col = "red")

points(accelerometer_data$x[anomaliesz], accelerometer_data$z[anomaliesz], col = "red")


 

