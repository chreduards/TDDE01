setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab2/Assignment 1")
library("MASS")

#Misclassification function
misclass = function(preds, truth){
  return(mean(preds != truth))
}
#Classification function used in 1.4
classify = function(pred, const){
  classified = c()
  for(i in 1:length(pred)){
    classified[i] = if(pred[i] > const) "Male" else "Female"
  }
  return(classified)
}

#1.1
#Load data and set color
data=read.csv("australian-crabs.csv")
data$Color = "black"
data$Color[data$sex == "Male"] = "red"

#Plot CL vs RW
plot(data$RW, data$CL, xlab = "Rear Width", ylab = "Carapace Length", col=data$Color, main="Carapace Length vs Rear Width")
legend("topleft", pch=c(1,1), legend = c("Female", "Male"), col = c("black", "red"))

#1.2
#Fit LDA model to data and perform prediction
fit_lda = lda(data$sex~ data$CL + data$RW, data = data)
pred_proportional = predict(fit_lda, data)
#Plot the LDA classification
plot(data$RW, data$CL, xlab = "Rear Width", ylab = "Carapace Length", col=as.numeric(pred_proportional$class), main="Prediction with proportional prior")
legend("topleft", pch=c(1,1), legend = c("Female", "Male"), col = c("black", "red"))
#Calculate misclasification rate
misclass_lda = misclass(pred_proportional$class, data$sex)
print(misclass_lda)

#1.3
#LDA model fitting and prediction with new priors
fit0.9_lda = lda(data$sex~ data$CL + data$RW, data = data, prior = c(0.1, 0.9))
pred0.9 = predict(fit0.9_lda, data)
plot(data$RW, data$CL, xlab = "Rear Width", ylab = "Carapace Length", col=as.numeric(pred0.9$class), main="Prediction with different prior")
legend("topleft", pch=c(1,1), legend = c("Female", "Male"), col = c("black", "red"))
misclass_lda0.9 = misclass(pred0.9$class, data$sex)
print(misclass_lda0.9)

#1.4
#Logistic regression model fitting and prediction
fit_glm = glm(data$sex~ data$CL + data$RW, data=data, family=binomial)
pred_glm = predict(fit_glm, data, type="response")
classified = round(pred_glm)
plot(data$RW, data$CL, xlab = "Rear Width", ylab = "Carapace Length", col=as.numeric(classified)+1, main="Prediction with logistic regression")
legend("topleft", pch=c(1,1), legend = c("Female", "Male"), col = c("black", "red"))
classify_sex = (classify(pred_glm, 0.5))
misclass_glm = misclass(classify_sex, data$sex)
print(misclass_glm)
#Coefficients for decision boundary
glmCof = fit_glm$coefficients
abline(-glmCof[1]/glmCof[2], glmCof[3]/-glmCof[2], col = "blue")
legend("topleft", lty=c(1,1,1), col=c("black", "red", "blue"), legend=c("Female", "Male", "Decision Boundary"))
