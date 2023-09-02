library(rgl)

 
accel_data <-read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatas.csv", sep = ',')
 
#plot3d(accel_data$x, accel_data$y, accel_data$z, type = "l", xlab = "Acceleration X", ylab = "Acceleration y", zlab = "Acceleration z")
 

# Step 5: Define anomaly detection algorithm (using z-score in this example)
detect_anomalies <- function(data, threshold = 3) {
  z_scores <- scale(data)  # Compute z-scores of the data
  anomalies <- abs(z_scores) > threshold  # Identify anomalies based on z-score threshold
  return(anomalies)
}
 
anomalies <- detect_anomalies(accel_data[, c("y", "x")])

anomaliesy <- detect_anomalies(accel_data[, c("z", "y")])
anomaliesz <- detect_anomalies(accel_data[, c("x", "y")])


num_anomalies <- sum(anomalies)
cat("Number of anomalies detected:", num_anomalies)
 

 
plot3d(accel_data$x, accel_data$y, accel_data$z, type = "l", xlab = "Acceleration X", ylab = "Acceleration z", zlab = "Acceleration z")
points3d(accel_data$x[anomalies], accel_data$y[anomalies], accel_data$z[anomalies], col = "red", size = 3)
points3d(accel_data$x[anomalies], accel_data$y[anomaliesy], accel_data$z[anomaliesz], col = "red", size = 3)