# Step 1: Import the accelerometer data
accel_data <-  read.csv("<dir>/magnetomererdatasWithDidDT.csv", sep = ',')

# Step 2: Preprocess the data (e.g., normalize, handle missing values, etc.)

# Step 3: Extract the accelerometer readings as features
accel_features <- accel_data[, c("x", "y", "z")]

# Step 4: Perform hierarchical clustering
distance_matrix <- dist(accel_features)  # Calculate the pairwise distance matrix
hca_model <- hclust(distance_matrix)  # Perform hierarchical clustering

# Step 5: Visualize the dendrogram
#plot(hca_model, hang = -1, main = "Hierarchical Clustering Dendrogram",     xlab = "Data Point", ylab = "Distance")

# Step 6: Cut the dendrogram to obtain clusters
num_clusters <- 3  # Set the desired number of clusters
clusters <- cutree(hca_model, k = num_clusters)

# Step 7: Visualize the clusters in a 3D scatter plot (assuming 3D data)
library(rgl)
plot3d(accel_features[, 1], accel_features[, 2], accel_features[, 3],
       col = clusters, size = 3,
       xlab = "Acceleration X", ylab = "Acceleration Y", zlab = "Acceleration Z")
