#### Import and divide data ####
setwd("C:/Users/Christoffer Eduards/OneDrive/R/Lab 3/Assignment 1")
set.seed(1234567890)
library(geosphere)
stations = read.csv("stations.csv")
temps = read.csv("temps50k.csv")
st = merge(stations, temps, by = "station_number")
#### Constants ####
h_distance = 100000 #Smoothing coefficient [meters]
h_date = 15 #Smoothing coefficient [days]
h_time = 2.5 #Smoothing coefficient [hours]
#POI (Linköping)
a = 58.401 
b = 15.577
date = "2013-08-01" #The date to predict (one out of two used)
times =
  c(
    "04:00:00",
    "06:00:00",
    "08:00:00",
    "10:00:00",
    "12:00:00",
    "14:00:00",
    "16:00:00",
    "18:00:00",
    "20:00:00",
    "22:00:00",
    "00:00:00"
  ) #The times to predict
tempPredSum = vector(length = length(times)) #Used for predicted temperatures with sum method
tempPredProd = vector(length = length(times)) #Used for predicted temperatures with prod method
st = st[(difftime(st$date, date)) < 0, ] #Filter out posterior data
stPositions = cbind(c(st[, 4]), c(st[, 5])) #Get latitudes and longitudes from data
##### Functions ####
#All methods implement this general Gaussian method
gaussianKernel = function(u, h) {
  return(exp(-abs(u / h) ^ 2))
}
#Measures the distance from the stations to a POI
stationToPoi = function(poi) {
  return(gaussianKernel(distHaversine(stPositions, poi), h_distance))
}
#Measures the days from the measurments to a Day of interest
dateToDoi = function(doi) {
  daysDiff = vector(length = length(st$date))
  mod = vector(length = length(st$date))
  u = vector(length = length(st$date))
  #Number of days difference
  for (i in 1:length(st$date)) {
    daysDiff[i] = as.numeric(as.Date(doi) - as.Date(st$date[i]))
  }
  #Modulus to get rid of the years
  for (i in 1:length(mod)) {
    mod[i] = daysDiff[i] %% 365
  }
  #Maximum half a year
  for (i in 1:length(daysDiff)) {
    u[i] = min(mod[i], 365 - mod[i])
  }
  return(gaussianKernel(u, h_date))
}
#Measures the time of day from the measurments to an hour of interest
hourToHoi = function(hoi) {
  hourDistance = as.numeric(abs(difftime(
    strptime(hoi, "%H:%M:%S"), strptime(st$time, "%H:%M:%S")
  ))) / 3600
  ifelse(hourDistance > 12, 24 - hourDistance, hourDistance) #Maximum 12 hours
  return(gaussianKernel(hourDistance, h_time))
}
#### Main ####
#### Temperature predictions ####
#Lastly all methods are utilized as described in the report and the predictions are calculated using the three "sub-kernels"
stDist = stationToPoi(c(a, b))
daDist = dateToDoi(date)
for (i in 1:length(times)) {
  tiDist = hourToHoi(times[i])
  kernelSum = stDist + daDist + tiDist
  kernelProd = stDist * daDist * tiDist
  tempPredSum[i] = (kernelSum %*% st$air_temperature) / sum(kernelSum)
  tempPredProd[i] = (kernelProd %*% st$air_temperature) / sum(kernelProd)
}
#Plot the kernel utilizing a summation (boundaries for y-values need to be changed for different dates)
plot(
  seq(4, 24, 2),
  tempPredSum,
  type = "o",
  ylim = c(6, 8.5),
  xlab = "Time of Day",
  ylab = "Temperature",
  main = "Sum of Kernels"
)
#Plot the kernel utilizing a product (boundaries for y-values need to be changed for different dates)
plot(
  seq(4, 24, 2),
  tempPredProd,
  type = "o",
  ylim = c(13,21),
  xlab = "Time of Day",
  ylab = "Temperature",
  main = "Prod of Kernels"
)