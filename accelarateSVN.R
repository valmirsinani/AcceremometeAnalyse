# Install the required package if not already installed
install.packages("e1071")
library(e1071)


data <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')

#plit the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(data), nrow(data) * 0.8)  # 80% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Convert labels to factors (if not already)
train_data$x <- as.factor(train_data$x)
test_data$x <- as.factor(test_data$x)

# Train the SVM model
svm_model <- svm(x ~ ., data = train_data, kernel = "radial")

# Make predictions on the test set
predictions <- predict(svm_model, newdata = test_data)

# Evaluate the model
accuracy <- sum(predictions == test_data$x) / length(test_data$x)
print(paste("Accuracy:", accuracy))