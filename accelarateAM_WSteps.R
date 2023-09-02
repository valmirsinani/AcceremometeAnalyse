
accel_data <-  read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',' ,nrows=2000)


features <- cbind(acceleration_x = accel_data$x,
                  acceleration_y = accel_data$y,
                  acceleration_z = accel_data$z)
 
labels <- c(0, 0, 1, 0, 1, 1, 0, 0, 1, 1)  # Example labels, replace with your actual labels


 
train_indices <- sample(1:nrow(features), nrow(features) * 0.7)
train_data <- features[train_indices, ]
train_labels <- labels[train_indices]
test_data <- features[-train_indices, ]
test_labels <- labels[-train_indices]

complete_cases <- !is.na(train_labels)
train_data <- train_data[complete_cases, ]
train_labels <- train_labels[complete_cases]


library(randomForest)
model <- randomForest(train_data, train_labels)

plot(model)

# Step 7: Evaluate the model
predicted_labels <- predict(model, test_data)
accuracy <- sum(predicted_labels == test_labels) / length(test_labels)
cat("Accuracy:", accuracy, "\n")
