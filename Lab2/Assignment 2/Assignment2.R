setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab2/Assignment 2")
library(e1071)
library(tree)
library(MASS)
#Misclassification function
misclass = function(preds, truth) {
  return(mean(preds != truth))
}
#Used to create a matrix with probabilities in 2.5
probMat = function(pi, input) {
  m = length(pi)
  probMat = matrix(nrow = dim(input)[1], ncol = length(pi))
  for (i in 1:dim(input)[1])
    for (j in 1:m) {
      probMat[i, j] = ifelse(input[i, 2] > pi[j], 1, 0)
    }
  return (probMat)
}
#ROC function used in 2.5
ROC = function(p, input) {
  m = length(p)
  TPR = numeric(m)
  FPR = numeric(m)
  
  for (i in 1:m) {
    t = table(test$good_bad, as.numeric(input[, i]))
    if (dim(t)[2] == 1 & colnames(t) == "1") {
      TP = t[2, 1]
      FP = t[1, 1]
      nPlus = TP
      nMinus = FP
      TPR[i] = TP / nPlus
      FPR[i] = FP / nMinus
    } else if (dim(t)[2] == 1 & colnames(t) == "0") {
      nPlus = t[2, 1]
      nMinus = t[1, 1]
      TP = 0
      FP = 0
      TPR[i] = TP / nPlus
      FPR[i] = FP / nMinus
    } else{
      TP = t[2, 2]
      FP = t[1, 2]
      nPlus = TP + t[2, 1]
      nMinus = FP + t[1, 1]
      TPR[i] = TP / nPlus
      FPR[i] = FP / nMinus
    }
  }
  return(list(TPR = TPR, FPR = FPR))
}
#2.1
#Load data and create train, validation and test sets
data = read.csv2("creditscoring.csv")
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = data[id, ]
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n * 0.25))
valid = data[id2, ]
id3 = setdiff(id1, id2)
test = data[id3, ]
#2.2a
#Fit and predict with tree and deviance measure
fit_devTree = tree(good_bad ~ ., data = train, split =  c("deviance"))
plot(fit_devTree)
text(fit_devTree)
print(summary(fit_devTree))
pred_test = predict(fit_devTree, newdata = test, type = "class")
misclass_devTest = misclass(pred_test, test$good_bad)
print(misclass_devTest)
#2.2b
#Fit and predict with tree and gini measure
fit_ginTree = tree(good_bad ~ ., train, split =  c("gini"))
plot(fit_ginTree)
text(fit_ginTree)
print(summary(fit_ginTree))
pred_test = predict(fit_ginTree, newdata = test, type = "class")
misclass_ginTest = misclass(pred_test, test$good_bad)
print(misclass_ginTest)
#2.3
#Finds optimal tree depth
trainScore = rep(0, 15)
testScore = rep(0, 15)
for (i in 2:15) {
  prunedTree = prune.tree(fit_devTree, best = i)
  pred = predict(prunedTree, newdata = valid,
                 type = "tree")
  trainScore[i] = deviance(prunedTree)
  testScore[i] = deviance(pred)
}
plot(
  2:15,
  trainScore[2:15],
  type = "b",
  col = "red",
  ylim = c(280, 600)
)
points(2:15, testScore[2:15], type = "b", col = "blue")
legend(
  "topright",
  lty = c(1, 1, 1),
  col = c("red", "blue"),
  legend = c("Train", "Validation")
)
best_Tree = prune.tree(fit_devTree, best = 4)
pred_best = predict(best_Tree, newdata = valid, type = "class")
plot(best_Tree)
text(best_Tree)
misclass_best = misclass(pred_best, test$good_bad)
print(misclass_best)
#2.4
#Fitting and prediction with Naive Bayes
fit_bayes = naiveBayes(good_bad ~ ., data = train, type = c("class"))
pred_bayesTrain = predict(fit_bayes, newdata = train)
conf_bayesTrain = table(Prediction = pred_bayesTrain, Truth = train$good_bad)
misclass_bayesTrain = misclass(pred_bayesTrain, train$good_bad)
print(conf_bayesTrain)
print(misclass_bayesTrain)
pred_bayesTest = predict(fit_bayes, newdata = test)
conf_bayesTest = table(Prediction = pred_bayesTest, Truth = test$good_bad)
misclass_bayesTest = misclass(pred_bayesTest, test$good_bad)
print(conf_bayesTest)
print(misclass_bayesTest)
#2.5
#Create ROC curves with Naive Bayes and tree
fit_bayes = naiveBayes(good_bad ~ ., data = train)
pred_bayesTest = predict(fit_bayes, newdata = test, type = "raw")
fit_devTree = tree(good_bad ~ ., data = train, split =  c("deviance"))
best_Tree = prune.tree(fit_devTree, best = 4)
pred_best = predict(best_Tree, newdata = test, class = "raw")
pi = seq(from = 0.05, to = 0.95, by = 0.05)
probMatBayes = probMat(pi, pred_bayesTest)
rocBayes = ROC(pi, probMatBayes)
probMatTree = probMat(pi, pred_best)
rocTree = ROC(pi, probMatTree)
plot(
  rocBayes$FPR,
  rocBayes$TPR,
  xlab = "FPR",
  ylab = "TPR",
  type = "l",
  col = "blue",
  xlim = c(0, 1),
  ylim = c(0, 1)
)
points(rocTree$FPR, rocTree$TPR, col = "red", type = "l")
legend(
  "topleft",
  lty = c(1, 1),
  legend = c("Naive Bayes", "Optimal Tree"),
  col = c("blue", "red")
)
#2.6
#Create confusion matrix and calculate misclassification rate with Naive Bayes and loss matrix
loss = matrix(c(0, 1, 10, 0), 2, 2)
fit_bayes = naiveBayes(good_bad ~ ., data = train)
pred_lossTrain = predict(fit_bayes, train, type = "raw")
classify_lossTrain = c()
classify_lossTrain = ifelse(pred_lossTrain[, 1] / pred_lossTrain[, 2] > loss[2, 1] / loss[1, 2], "bad", "good")
misclass_lossTrain = misclass(classify_lossTrain, train$good_bad)
conf_lossTrain = table(Truth = train$good_bad, Prediction = classify_lossTrain)
print(misclass_lossTrain)
print(conf_lossTrain)
pred_lossTest = predict(fit_bayes, test, type = "raw")
classify_lossTest = c()
classify_lossTest = ifelse(pred_lossTest[, 1] / pred_lossTest[, 2] > loss[2, 1] / loss[1, 2], "bad", "good")
misclass_lossTest = misclass(classify_lossTest, test$good_bad)
conf_lossTest = table(Truth = test$good_bad, Prediction = classify_lossTest)
print(misclass_lossTest)
print(conf_lossTest)
