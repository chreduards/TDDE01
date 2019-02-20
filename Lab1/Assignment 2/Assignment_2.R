#2.1
setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab1/Assignment2")
lifetimes=read.csv2("machines.csv")
thetas <- seq(0.0, 5.0, 0.01)
set.seed(12345)
#2.2
#The loglikelihood function which is used in the assignment
loglikelihood <- function(x, theta) {
  p = log(theta*exp(-theta*x))
  return(sum(p))
}

#Predict values with the loglikelihood function, and inputs lifetimes and thetas 
predsLog = sapply(thetas, loglikelihood, x=lifetimes)


#Plots the log-likelihood function on theta
plot(thetas, predsLog, xlab="theta",
     ylab = "log-likelihood",
     xlim=c(0,5), ylim=c(-160,-40), col="blue", type="l")

#Finds the maximum theta
max_predsLog = thetas[which(predsLog==max(predsLog))]
print(max_predsLog)

#2.3
#Chose only the first six values in the dataset then do the same as in 2.2
lifetimes_first_six = lifetimes[1:6,]
predsLog_firstSix = sapply(thetas, loglikelihood, x=lifetimes_first_six)
plot(thetas, predsLog, xlab="theta",
     ylab = "log-likelihood",
     xlim=c(0,5), ylim=c(-80,0), col="blue", type="l")
points(thetas, predsLog_firstSix, col="red", type="l")
max_predsLog_firstSix = thetas[which(predsLog_firstSix==max(predsLog_firstSix))]
print(max_predsLog_firstSix)

#2.4
#The bayeshian model used in the assignment
bayesian_model = function(x, theta){
  p = prod(theta*exp(-theta*x))
  prior = 10*exp(-10*theta)
  l = log(p*prior)
  return(l)
}

predsBay = sapply(thetas, bayesian_model, x=lifetimes)
#Plots the predictions from 2.4 and 2.2
plot(thetas, predsBay, xlab="theta",
     ylab = "log-likelihood",
     xlim=c(0,5), ylim=c(-250,-40), col="blue", type="l")
points(thetas, predsLog, col="red", type="l")
#Theta that maximises the function
max_predsBay = thetas[which(predsBay==max(predsBay))]
print(max_predsBay)

#2.5
#Theta from 2.2
theta = max_predsLog
#Creates a histogram of the original data
hist(as.matrix(lifetimes),15, main= "Histogram of original data", xlab="Lifetime")
#Creates a histogram of the new random generated data
hist(rexp(50,theta),15, main= "Histogram of new data", xlab="Lifetime")