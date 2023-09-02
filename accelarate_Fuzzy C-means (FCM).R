# Load required libraries
install.packages("factoextra")   # Install factoextra package if not already installed
library(factoextra)             # Load factoextra package

library(e1071)   # For fuzzy c-means algorithm
library(cluster) # For silhouette coefficient

# Generate sample accelerometer data (replace with your own data)
accelerometer_data <-accel_data <- read.csv("<dir>/magnetomererdatasWithDidDT.csv", sep = ',')


accelerometer_data$x=as.double(accelerometer_data$x)
accelerometer_data$y=as.double(accelerometer_data$y)
accelerometer_data$z=as.double(accelerometer_data$z)
# Set the number of clusters
num_clusters <- 3

accelerometer_data <- accelerometer_data[, c("x", "y", "z")]
accelerometer_data <- na.omit(accelerometer_data)
# Fuzzy C-means clustering
fcm_result <- cmeans(accelerometer_data, num_clusters)

# Get cluster membership values for each data point
membership <- fcm_result$membership

# Assign data points to the cluster with highest membership value
predicted_labels <- apply(membership, 1,which.max)

 

# Evaluate clustering performance using silhouette coefficient
silhouette_score <- silhouette(predicted_labels, dist(accelerometer_data)) 
plot(silhouette_score)

sil <- silhouette(predicted_labels, dist(accelerometer_data))
fviz_cluster(list(data = accelerometer_data, cluster = predicted_labels))





