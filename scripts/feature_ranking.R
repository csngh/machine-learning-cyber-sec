#[1.]-----Importing Libraries-----
#Machine Learning Benchmarking package
library(mlbench)
#Classification and Regression package
library(caret)

#[2.]-----Importing dataset-----
dataset <- read.csv('system_logs_dummy.csv')

#[3.]-----Creating the trainging model-----
#Creating multiple folds to train the data on
tControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#Trainging the model w.r.t the dependant variable, using Learning Vector Quantization
tModel <- train(risk_factor~., data = dataset, method = "lvq", preProcess = "scale", trControl = tControl)

#[4.]-----Getting results-----
#Calculating variable importance
importance <- varImp(tModel, scale = FALSE)
#Visualizing variable importance
plot(importance)