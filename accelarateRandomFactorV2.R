# Step 1: Load the required library
library(randomForest)
accel_data<-read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')

# Step 2: Load the dataset
#data(accel_data)  # Load the iris dataset (a popular example in classification)

# Step 3: Split the dataset into training and testing sets
set.seed(123)  # Set a seed for reproducibility
train_indices <- sample(1:nrow(accel_data), 0.7*nrow(accel_data))  # Select 70% of the data for training
train_data <- accel_data[train_indices, ]  # Training data
test_data <- accel_data[-train_indices, ]  # Testing data

# Step 4: Train the Random Forest model
model <- randomForest(accel_data$z ~ ., data = train_data, ntree = 100)

# Step 5: Make predictions on the testing data
predictions <- predict(model, newdata = test_data)

# Step 6: Evaluate the model
accuracy <- sum(predictions == test_data$Species) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")


########################################

 
 
# Step 1: Load the required library
library(randomForest)

# Step 2: Load the dataset
data(iris)  # Load the iris dataset (a popular example in classification)

# Step 3: Split the dataset into training and testing sets
set.seed(123)  # Set a seed for reproducibility
train_indices <- sample(1:nrow(iris), 0.7*nrow(iris))  # Select 70% of the data for training
train_data <- iris[train_indices, ]  # Training data
test_data <- iris[-train_indices, ]  # Testing data

# Step 4: Train the Random Forest model
model <- randomForest(Species ~ ., data = train_data, ntree = 100)

# Step 5: Make predictions on the testing data
predictions <- predict(model, newdata = test_data)
plot(predictions)

# Step 6: Evaluate the model
accuracy <- sum(predictions == test_data$Species) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

########################################


# Step 1: Import the accelerometer data and labels
accel_data<-read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')
labels <- accel_data$x  # Assuming activity_label is the column containing the class labels

# Step 2: Preprocess the data (e.g., normalize, handle missing values, etc.)

# Step 3: Split the data into training and testing sets
library(caret)
set.seed(123)  # Set a seed for reproducibility
train_indices <- createDataPartition(labels, p = 0.7, list = FALSE)  # Split data into 70% training and 30% testing
train_data <- accel_data[train_indices, ]
test_data <- accel_data[-train_indices, ]
train_labels <- labels[train_indices]
test_labels <- labels[-train_indices]

# Step 4: Train a classification model
# Choose an appropriate classification algorithm based on your requirements and the characteristics of your data
# For example, let's use a random forest classifier
library(randomForest)
model <- randomForest(x ~ ., data = train_data)

# Step 5: Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Step 6: Evaluate the model performance
library(caret)
confusionMatrix(predictions, test_labels)




