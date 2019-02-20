setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab1/Assignment4")
data=read.csv2("tecator.csv")
data_matrix = as.matrix(data)
library("glmnet")
library("MASS")

n=dim(data_matrix)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
validation=data[-id,]

#4.1
#Plot moisture vs protein
moisture=data_matrix[,ncol(data)]
protein=data_matrix[,ncol(data)-1]
plot(protein,moisture)

#4.3
#Create six different models each for training and validation data sets.
mse_train = numeric(6)
mse_validation = numeric(6)
for (i in 1:6) {
  lm_model = lm(Moisture ~ poly(Protein, i), data = train)
  predict_train = predict.lm(lm_model, train)
  predict_validation = predict.lm(lm_model, validation)
  mse_train[i] = mean((train$Moisture - predict_train) ^ 2)
  mse_validation[i] = mean((validation$Moisture - predict_validation) ^ 2)
}
x = seq(1, 6)
#Plot the MSE vs pol. degree
plot(
  x,
  mse_train,
  ylim = c(30, 35),
  col = "blue",
  ylab = "Mean Squared Error",
  xlab = "Pol. Degree",
  type = "l"
)
points(x, mse_validation, col = "red", type = "l")

#4.4
#Perform the stepAIC variable selection
lm_step = lm(data$Fat~., data[,2:102] )
step = stepAIC(lm_step)

#4.5
#Ridge regression
predictors = scale(data[, 2:101])
response = scale(data[, 102])
ridge = glmnet(as.matrix(predictors),
               response,
               alpha = 0,
               family = "gaussian")
plot(ridge, xvar = "lambda", label = TRUE)

#4.6
#Lasso regression
lasso = glmnet(as.matrix(predictors),
               response,
               alpha = 1,
               family = "gaussian")
plot(lasso, xvar = "lambda", label = TRUE)

#4.7
#Perform cross-validation to find optimal lasso-model on lambda
cross_validation = cv.glmnet(as.matrix(predictors),
                             response,
                             alpha = 1,
                             family = "gaussian")

cross_validation$lambda.min
plot(cross_validation)
coef(cross_validation, s = "lambda.min")
print(cross_validation$lambda.min)