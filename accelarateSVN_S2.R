install.packages("tidyr")
install.packages("lubridate")
library(tidyverse)

library(hms)


dataset = read.csv("C:/Users/valmirs/Desktop/phdk/sem II IoT/magnetomererdatasWithDidDT.csv", sep = ',')


dataset =head(dataset,300) 



dataset$dt = as_hms(dataset$dt)


install.packages('caTools')
library(caTools)

set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)


# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)

classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

y_pred = predict(classifier, newdata = test_set[-3])

cm = table(test_set[, 3], y_pred)



