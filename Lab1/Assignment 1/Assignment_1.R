#1.1
setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab1/Assignment1")
library("kknn")

#Loads data and divides it into train and test sets
Dataframe=read.csv2("spambase.csv")
n=dim(Dataframe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,] 

#1.2
#Classify is a function that divides the predictions according to the exercise
classify = function(pred, const){
  classified = c()
  for(i in 1:length(pred)){
    classified[i] = if(pred[i] > const) 1 else 0
  }
  return(classified)
}
#Fit the logistic regression model
glm.fit = glm(Spam~., data=train, family=binomial)

#Prediction with test data
preds_test = predict(glm.fit, test, type="response")

#Classifies according to exercise 1.2
classified5_test = classify(preds_test, 0.5)

#Confusion matrix test data:
conf5_test = table(Truth = test$Spam, classified5_test)

#Missclassification function
missclass = function(preds, truth){
  return(mean(preds != truth))
}

#Missclassification rate test data
missclass5_test = missclass(classified5_test, test$Spam)

#Prediction for train data
preds_train = predict(glm.fit, train, type="response")

#Classified train data
classified5_train = classify(preds_train, 0.5)

#Confusion matrix train data
conf5_train = table(Truth = train$Spam, Prediction = classified5_train)

#Missclassification rate train data
missclass5_train = missclass(classified5_train, train$Spam)

#Printout for confusion matrixes and missclassification rates
print(conf5_test)
print(missclass5_test)
print(conf5_train)
print(missclass5_train)

#1.3
#Confusion matrixes and missclassification rates for test and train data with classification
#according to exercise 1.3
classified9_test = classify(preds_test, 0.9)
conf9_test = table(Truth = test$Spam, Prediction = classified9_test)
missclass9_test = missclass(classified9_test, test$Spam)
classified9_train = classify(preds_train, 0.9)
conf9_train = table(Truth = train$Spam, Prediction = classified9_train)
missclass9_train = missclass(classified9_train, train$Spam)

#Printout
print(conf9_test)
print(conf9_train)
print(missclass9_train)
print(missclass9_test)

#1.4
#Fits model with package kknn() and K=30
fit.kknn30 = train.kknn(Spam~., train, 30)

#Make predictions with test data
preds30_test = predict(fit.kknn30, test)

#Classifies data as in 1.2 
classified_test_kknn30 = classify(preds30_test, 0.5)
#Confusion matrix and missclassification rate for test data
conf_test_kknn30 = table(classified_test_kknn30, test$Spam)
missclass_test_kknn30 = missclass(classified_test_kknn30, test$Spam)

#Same steps as previously but for training data
preds30_train = predict(fit.kknn30, train)
classified_train_kknn30 = classify(preds30_train, 0.5)
conf_train_kknn30 = table(classified_train_kknn30, train$Spam)
missclass_train_kknn30 = missclass(classified_train_kknn30, train$Spam)

#Printout for confusion matrixes and missclassification rates
print(conf_test_kknn30)
print(conf_train_kknn30)
print(missclass_test_kknn30)
print(missclass_train_kknn30)

#1.5
#Same exercise as 1.4 but with K=1
fit.kknn1 = train.kknn(Spam~., train, 1)

preds1_test = predict(fit.kknn1, test)

classified_test_kknn1 = classify(preds1_test, 0.5)

conf_test_kknn1 = table(classified_test_kknn1, test$Spam)

missclass_test_kknn1 = missclass(classified_test_kknn1, test$Spam)
preds_train_kknn1 = predict(fit.kknn1, train)
classified_train_kknn1 = round(preds_train_kknn1)
conf_train_kknn1 = table(classified_train_kknn1, train$Spam)
missclass_train_kknn1 = missclass(classified_train_kknn1, train$Spam)
print(conf_test_kknn1)
print(conf_train_kknn1)
print(missclass_test_kknn1)
print(missclass_train_kknn1)




