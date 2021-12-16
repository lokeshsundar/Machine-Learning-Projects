getwd()
setwd("E:\\MBA\\Project\\Mansur\\Forecasting model")

##Essential libraries
library(tseries)
library(forecast)
library(ggplot2)

## DAta documentation
help(AirPassengers)

## Exploring the data EDA
View(AirPassengers)
class(AirPassengers)
head(AirPassengers)
str(AirPassengers)
tail(AirPassengers)

## When data starts and Ends 
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)

##Summary statistics
summary(AirPassengers)

##visual of data
plot(AirPassengers)

## visualising trend in the dataset
abline(reg=lm(AirPassengers~time(AirPassengers)))

cycle(AirPassengers)

##aggregate plot
plot(aggregate(AirPassengers, FUN = mean))

## box plot
boxplot(AirPassengers~ cycle(AirPassengers))

##More plots to analyse the data
seasonplot(AirPassengers)
ggseasonplot(AirPassengers)
ggseasonplot(AirPassengers, polar=TRUE)
gglagplot(AirPassengers)
ggsubseriesplot(AirPassengers)
tsdisplay(AirPassengers)




## XTS Plot
require(xts)
plot(as.xts(AirPassengers,), major.format = "%mm-%YYYY", cex=0.6)

## Step 2: Data decomposition
?stl
Air_decompose <- stl(AirPassengers, s.window = 7)
Air_decompose
plot(Air_decompose)

Air_decompose1 <- stl(AirPassengers, s.window = 9)
plot(Air_decompose1)

Air_decompose2 <- stl(AirPassengers, s.window = "periodic")
plot(Air_decompose2)

##Step 3: deseasonlaising the data
deseasoned_air <- seasadj(Air_decompose2)
plot(deseasoned_air)
abline(lm(deseasoned_air~ time(deseasoned_air)), col = "blue")
deseasoned_air

## Is the time series stationary?? Run Augmented dickey fuller test
?adf.test
adf.test(diff(log(AirPassengers)),alternative = "stationary", k = 0) ## this aslo includes data differencing

##ACF plot
acf(diff(log(AirPassengers))) ##Or without differecning
ggAcf(AirPassengers)

## Pacf plot
pacf(diff(log(AirPassengers))) ##Or without differencing
ggPacf(AirPassengers)

## Step 4: Data modelling > Split the Dateset into Training Set and Test Set

Air_Train <- window(deseasoned_air, start=1949, end = c(1958,12))
Air_Test <- window(deseasoned_air, start = 1959)
str(Air_Train)
str(Air_Test)
Air_Train
Air_Test

## Step 5: Building the ARIMA model with determied values

Airfit = arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)) # (p,d,q)=(PACF, ACF, diff)

Airfit

tsdisplay(residuals(Airfit), lag.max = 15, main = "Model Residuals")


## Forecasting with the ARIMA model

fcast_Air = forecast(Airfit, h=12)
fcast_Air
plot(fcast_Air)



## compute accuracy of the forecast - on the test data

accuracy(fcast_Air)

accuracy(fcast_Air,Air_Test)


