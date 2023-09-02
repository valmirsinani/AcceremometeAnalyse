library(randomForest)
library(party)

# Step 1: Import the accelerometer data
accel_data <- read.csv("<dir>/magnetomererdatasTop1000.csv", sep = ',')

str(accel_data)


# Step 2: Preprocess the data (assuming no preprocessing needed in this example)

# Step 3: Feature extraction (assuming no additional features needed in this example)

# Step 4: Train the random forest model
model <- randomForest(y ~ ., data = accel_data, ntree=500)
plot(model)

# View the forest results.
print(model) 

# Importance of each predictor.
print(importance(y,type = 2)) 

#Step 5: Calculate the anomaly scores using the out-of-bag (OOB) observations
anomaly_scores <- predict(model, type = "vote", newdata = accel_data)

# Step 6: Set a threshold to identify anomalies
anomalies <- anomaly_scores > threshold  # Adjust threshold as needed

# Step 7: Count the number of anomalies
num_anomalies <- sum(anomalies)
cat("Number of anomalies detected:", num_anomalies)

# Step 8: Visualize and analyze the results
plot3d(accel_data$timestamp, accel_data$acceleration_x, accel_data$acceleration_y, type = "l", xlab = "Timestamp", ylab = "Acceleration X", zlab = "Acceleration Y")
points3d(accel_data$timestamp[anomalies], accel_data$acceleration_x[anomalies], accel_data$acceleration_y[anomalies], col = "red", size = 3)

