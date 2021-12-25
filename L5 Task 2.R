# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear Environment
rm(list = ls())

# Clear console
cat("\014")  # ctrl+L

#import libraries
library("caret")
library("e1071")
library("rpart")
library("rpart.plot")

#import iris dataset
library(datasets)

#holdout validation (1 train test split)
#y is the outcome variable
#p is the percentage of data assigned to the training date
train_rows <- createDataPartition(y = iris$Species, p =0.7, list = FALSE)

#Create the training data
iris_train <- iris[train_rows,]

#Create the test data, by taking all the rows that were not included in iris_train
iris_test <- iris[-train_rows, ]

#train the data with recursive partitioning
fitDT <- train(data = iris_train, method = "rpart", Species~.)

#To see a description of the tree 
fitDT$finalModel

#See the decision rules
rpart.rules(fitDT$finalModel)

#Get the plot of the tree
rpart.plot(fitDT$finalModel)

#get the predictions from the tree
DT_predictions <- predict(fitDT$finalModel, newdata = iris_test, type = "class")
DT_predictions

#Create the confusionMatrix to evaluate the performance of the model
confusionMatrix(DT_predictions, iris_test$Species)
