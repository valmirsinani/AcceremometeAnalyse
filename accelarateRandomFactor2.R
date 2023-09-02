datas <- read.csv("<dir>/magnetomererdatasTop1000.csv", sep = ',')

library(randomForest)
set.seed(123)  # For reproducibility
train_idx <- sample(1:nrow(datas), 0.7 * nrow(datas))  # 70% for training
train_data <- datas[train_idx, ]
test_data <- datas[-train_idx, ]

rf_model <- randomForest(train_data$x ~ ., data = train_data)
plot(rf_model)

predictions <- predict(rf_model, newdata = test_data)
confusion_matrix <- table(predictions, test_data$y)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


plot(confusion_matrix)

