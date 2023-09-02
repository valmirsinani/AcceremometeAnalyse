# Step 1: Import the accelerometer data
accel_data <-  read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')

# Step 2: Preprocess the data (e.g., normalize, handle missing values, etc.)

# Step 3: Extract the accelerometer readings as features
accel_features <- accel_data[, c("x", "y", "z")]

# Step 4: Apply the DBSCAN algorithm
library(dbscan)
dbscan_model <- dbscan(accel_features, eps = 0.5, minPts = 5)

# Step 5: Access the cluster assignments
cluster_labels <- dbscan_model$cluster



pairs(accel_features, col = cluster_labels + 1L)
#OPTICS
par(mfrow = c(2, 3)) # Divide the plot area into a 2x3 gri
opt <- optics(accel_features, eps = 1, minPts = 4)
opt <- extractDBSCAN(opt, eps_cl = 0.4)
plot(opt)

#HDBSCAN

hdb <- hdbscan(accel_features, minPts = 4)
plot(hdb, show_flat = TRUE)

#knn k = 5
dbscan::kNNdistplot(accel_features, k =  5)
abline(h = 0.15, lty = 2)

#kNNdistplot
set.seed(1234)
db = dbscan(accel_features, 0.4, 4)
hullplot(accel_features, db$cluster)


#fpc
library("fpc")

cs = cluster.stats(dist(accel_features[1:3]), db$cluster)


plot(accel_features[, 1], accel_features[, 2],
     col = cluster_labels, pch = 16,
     xlab = "x", ylab = "y")



# Step 6: Visualize the clusters in a 3D scatter plot (assuming 3D data)
library(rgl)
plot3d(accel_features[, 1], accel_features[, 2], accel_features[, 3],
       col = cluster_labels, size = 3,
       xlab = "x", ylab = "x", zlab = "x")



