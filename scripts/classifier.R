#[1.]------Importing essentail information------
#Package for handling the data
library(caTools)
#Random Forest package
library(randomForest)
#Classification and Regression package
library(caret)

#Importing dataset
dataset <- read.csv('system_logs_dummy.csv')
#Dropping irrelevant features w.r.t results from 'feature_ranking.R' script
dataset <- dataset[, c(-2, -3, -4, -5, -6, -7)]

#[2.]------Data Preprocessing------
#Lopping through all the columns to handle missing values (If present)
for (i in seq(1, ncol(dataset)-1)){
  #Replacing the missing values with the mean of all the columns and rounding them at the end
  dataset[, i] <- ifelse(is.na(dataset[, i]), 
                         ave(dataset[, i], FUN = function(x) round(mean(x, na.rm = T))),
                         dataset[, i])
}

#Handling categorical data and labeling them (In this case the target variable risk_factor)
dataset$risk_factor <- factor(dataset$risk_factor,
                              levels = c('low', 'medium', 'high'),
                              labels = c(1, 2, 3))

#Splitting dataset into training and test set with 80:20 % ratio
split.fac <- sample.split(dataset$risk_factor, SplitRatio = 0.8)
training.set <- subset(dataset, split.fac == TRUE)
test.set <- subset(dataset, split.fac == FALSE)

#Feature scaling
training.set[, -6] <- scale(training.set[, -6])
test.set[, -6] <- scale(test.set[, -6])

#[3.]------Building the Model------
#Building classifier object using 500 trees
classifier <- randomForest(x = training.set[, -6],
                          y = training.set$risk_factor,
                          ntrees = 500)

#[4.]------Predicting the Results------
#Predicting the values
predicted.val <- predict(classifier, newdata = test.set[, -6])

#Building a confusion matrix
cm <- table(test.set[, 6], predicted.val)

#[5.]------Implementing K-Fold Cross Validation------
#Creating 10 folds w.r.t the dependant variable
folds <- createFolds(training.set$risk_factor, k = 10)
#Applying the classifier function to all the folds
cv <- lapply(folds, function(x){
  training.fold <- training.set[-x, ]
  test.fold <- training.set[x, ]
  #Random forest classifier object using 500 trees
  classifier <- randomForest(x = training.fold[, -6],
                             y = training.fold$risk_factor,
                             ntrees = 500)
  #Predicting values
  predicted.val <- predict(classifier, newdata = test.fold[, -6])
  #Building a confusion matrix of single fold
  cm <- table(test.fold[, 6], predicted.val)
  #Testing the accuracy out of the confusion matrix
  accuracy <- (cm[1, 1] + cm[2, 2] + cm[3, 3]) / sum(cm)
  #Returing the accuracy
  return(accuracy)
})

#[6.]------Getting final results------
#Pilot accuracy test result
print(paste('Pilot accuracy test result:', round(((cm[1, 1] + cm[2, 2] + cm[3, 3]) / sum(cm)) * 100, 3), '%'))
#Printing the maximum accuracy from the cross validation set result
print(paste('Maximum accuracy from cross validation set:', round(max(as.numeric(cv)) * 100, 3), '%'))
#Printing the average accuracy of our model
print(paste('Estimated average accuracy for our model after cross validating:', round(mean(as.numeric(cv)) * 100, 3), '%'))
