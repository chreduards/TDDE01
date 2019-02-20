#### Import and divide data ####
setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab2/Assignment 1")
library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin = sin(Var))
tr <- trva[1:25, ] #Training
va <- trva[26:50, ] #Validation
#### Main ####
#Randomly generating the initial weights
winit = runif(31, min = -1, max = 1)
MSE = c() #Vector for the MSE
#Training nn different i
for (i in 1:10) {
  nn = neuralnet(
    Sin ~ Var,
    data = tr,
    hidden = 10,
    threshold = i / 1000,
    startweights = winit
  )
  #Prediction for validation data
  pred = compute(nn, va$Var)$net.result
  #Plotting iterations for nn training
  plot(
    va$Var,
    pred,
    main = paste("Iteration", i),
    ylab = "Sin(x)",
    xlab = "x"
  )
  #Plot actual data
  points(va, col = "red")
  #Mean squared error vs iteration
  MSE[i] = mean((va$Sin - pred) ^ 2)
}
#Show mean squared error for different i
plot(MSE, xlab = "Index of i in Threshold(i/1000)", main = "MSE")
#Train optimal nn
nn = neuralnet(
  Sin ~ Var,
  data = tr,
  hidden = 10,
  threshold = which.min(MSE) / 1000,
  startweights = winit
)
plot(nn)#Picture of the final nn 