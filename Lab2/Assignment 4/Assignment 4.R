setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab2/Assignment 4")
library(fastICA)
data=read.csv2("NIRSpectra.csv")
set.seed(12345)
data1=data
data1$Viscosity=c()
#4.1
#Conduct a standard PCA
lambda=res$sdev^2
res=prcomp(data1)
#eigenvalues
lambda
#proportion of variation
sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(res)
plot(res$x[,1], res$x[,2], xlab="PC1", ylab="PC2")
vars=apply(data1,2,var)
plot(vars)
#4.2
#Trace plots of loadings of the components from 4.1
U = res$rotation
plot(U[,1], main = "Traceplot, PC1") 
plot(U[,2], main = "Traceplot, PC2", ylim = c(-0.1,0.5))
#4.3
#Independent component analysis with fastICA
ica = fastICA(data1, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
              method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)
Wp = ica$K %*% ica$W 
plot(Wp[,1], main = "Traceplot Wp1")
plot(Wp[,2], main = "Traceplot Wp2")
plot(ica$S[,1], ica$S[,2], main = "Score plot ICA")