datas <- read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasTop1000.csv", sep = ',')

library(randomForest)
set.seed(123)  # For reproducibility
train_idx <- sample(1:nrow(datas), 0.7 * nrow(datas))  # 70% for training
train_data <- datas[train_idx, ]
test_data <- datas[-train_idx, ]

rf_model <- randomForest(train_data$x ~ ., data = train_data)

predictions <- predict(rf_model, newdata = test_data)
confusion_matrix <- table(predictions, test_data$y)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


library(ggplot2)
library(reshape2)
library(tidyr)

# Convert the confusion matrix to a data frame
confusion_df <- as.data.frame.matrix(confusion_matrix)
confusion_df <- melt(confusion_df)
accuracy_df <- data.frame(Method = "Random Forest", Accuracy = accuracy)



ggplot(accuracy_df, aes(x = Method, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Model Accuracy",
       x = "Method",
       y = "Accuracy")
