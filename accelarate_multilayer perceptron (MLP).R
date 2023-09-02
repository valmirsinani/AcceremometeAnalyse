

acceleration_data <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasTop1000.csv", sep = ',',nrows=3000)
 
install.packages("neuralnet")  # Install the neuralnet package
library(neuralnet)            

normalized_data <- as.data.frame(scale(acceleration_data))

set.seed(123)
formula <- as.formula("walking  ~ x + y + z")
 
train_indices <- sample(1:nrow(acceleration_data), 0.7 * nrow(acceleration_data))
train_data <- acceleration_data[train_indices, ]
test_data <- acceleration_data[-train_indices, ]

mlp_model <- neuralnet(formula, data = train_data, hidden = 10)

mlp_predictions <- compute(mlp_model, test_features)$net.result
mlp_predictions <- ifelse(mlp_predictions > 0.5, 1, 0)

test_labels <- test_data$y

accuracy <- sum(mlp_predictions == test_labels) / length(test_labels)
library(cluster)      # For clustering algorithms
library(factoextra)   # For visualizing clustering results

# Assuming your features are in columns 1 to n
features <- acceleration_data[, 1:ncol(acceleration_data)]
scaled_features <- scale(features)

k <- 2
kmeans_model <- kmeans(scaled_features, centers = k)

walking_label <- ifelse(kmeans_model$centers[1, 1] > kmeans_model$centers[2, 1], 1, 2)

target <- rep(walking_label, nrow(acceleration_data))



