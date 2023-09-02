# Step 1: Import the accelerometer data
accel_data <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT10000.csv", sep = ',')

# Step 2: Preprocess the data (e.g., normalize, handle missing values, etc.)

# Step 3: Extract the accelerometer readings as features
accel_features <- accel_data[, c("x", "y", "z","dt")]
#par(mfrow = c(1, 2)) 

library('lubridate')
op <- options(digits.secs=0)
datetime_dt <- as.POSIXct(accel_features$dt, format = "%Y-%m-%dT%H:%M:%OS")
#accel_features$dt <- datetime_dt# paste(substr(second(datetime_dt),5,8),ep="") 

accel_features$dt <- day(datetime_dt)*24*60 + hour(datetime_dt)*60 + minute(datetime_dt)
# Step 6: Visualize the clusters (assuming 2D data)
# Step 4: Apply the k-means algorithm
library(stats)
set.seed(123)  # Set a seed for reproducibility
num_clusters <- 4  # Set the desired number of clusters
kmeans_model <- kmeans(accel_features, centers = num_clusters)

# Step 5: Access the cluster assignments and centroids
cluster_labels <- kmeans_model$cluster
centroids <- kmeans_model$centers
 par(mfrow = c(1, 3)) 

plot(accel_features[, 4], accel_features[, 1], col = cluster_labels,
     main = "K-means Clustering of AD",
     xlab = "Time (m)", ylab = "Acceleration X")
points(centroids[, 4], centroids[, 1], col = "red", pch = 8, cex = 2)


plot(accel_features[, 4], accel_features[, 2], col = cluster_labels,
     main = "K-means Clustering of AD",
     xlab = "Time (m)", ylab = "Acceleration Y")
points(centroids[, 4], centroids[, 2], col = "red", pch = 8, cex = 2)


plot(accel_features[, 4], accel_features[, 3], col = cluster_labels,
     main = "K-means Clustering of AD",
     xlab = "Time (m)", ylab = "Acceleration Z")
points(centroids[, 4], centroids[, 3], col = "red", pch = 8, cex = 2)


# Step 7 Visualize the clusters in a 3D scatter plot
library(rgl)
plot3d(accel_features[, 1], accel_features[, 2], accel_features[, 3],
       col = cluster_labels, size = 3, xlab = "Acceleration X", ylab = "Acceleration Y", zlab = "Acceleration Z")
points3d(centroids[, 1], centroids[, 2], centroids[, 3],
         col = "blue", size = 6, add = TRUE)


