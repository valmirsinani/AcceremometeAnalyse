library(glmnet)  # Load the glmnet package for logistic regression


dataset <- read.csv("<dir>/magnetomererdatasTop1000.csv", sep = ',')#,nrows=3000)

# R program to illustrate
# Multi Layered Neural Networks

# Use the set.seed() function
# To generate random numbers
set.seed(500)

# Import required library
library(MASS)

# Working on the Boston dataset
data <- Boston
apply(data, 2, function(x) sum(is.na(x)))
index <- sample(1 : nrow(data),
                round(0.75 * nrow(data)))
train <- data[index, ]
test <- data[-index, ]
lm.fit <- glm(medv~., data = train)
summary(lm.fit)
pr.lm <- predict(lm.fit, test)
MSE.lm <- sum((pr.lm - test$medv)^2) / nrow(test)
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data,
                              center = mins,
                              scale = maxs - mins))
train_ <- scaled[index, ]
test_ <- scaled[-index, ]

# Applying Neural network concepts
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~",
                      paste(n[!n %in% "medv"],
                            collapse = " + ")))
nn <- neuralnet(f, data = train_,
                hidden = c(4, 2),
                linear.output = T)

# Plotting the graph
plot(nn)
