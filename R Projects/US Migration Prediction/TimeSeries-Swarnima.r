#importing packages for Time series analysis and forecasting
library(readxl)
library(ggplot2)
library(forecast)
library(xts)
library(fpp2)
library(TTR)
library(dplyr)
library(hrbrthemes)
library(tseries)
library(StatMeasures)
#Remove the first record from the excel file (skip = 1)  
demand <- read_excel("E:\\dataset\\revexx\\UK Outward Passengers Movement.xls", skip = 1)

str(demand)
names(demand)[3] <- c("Ireland")
names(demand)[7] <- c("Total")
str(demand)
summary(demand[,3:7])

#Plottig Time Series for Ireland for quarterly data from year 1996 to 2006 
dem_Ireland <- ts(demand[,3], start=c(1996,1), end=c(2006,4), frequency=4) 
plot(dem_Ireland)
#Plottig Time Series for Total for quarterly data from year 1996 to 2006 
dem_Total <- ts(demand[,7], start=c(1996,1), end=c(2006,4), frequency=4) 
plot(dem_Total)
#Plotting the Time Series across Ireland and Total 
ts.plot(dem_Ireland, dem_Total, gpars = list(col = c("black", "red")),xlab="year", ylab="demand") 
legend("topleft", colnames(demand[3:7]), col=1:ncol(demand), lty=1.9, cex=.45)
# this shows that our attributes in the dataset are independent of each other and hence we
#can perform Univariate time series forecating on it

#working on Total data quarterly
#firstly removing Null values
tail(dem_Total)
sum(is.na(dem_Total))
dem_Total<-na.omit(dem_Total)
sum(is.na(dem_Total))
#making the data stationary
adf.test(dem_Total) #augmented-dickey fuller test
#The p-value turns out be 0.96. 
#We thus fail to reject our Ho and conclude that the data is not stationary.

##using method of differencing to make our data stationary.
data_Total <- diff(dem_Total, differences = 2)
adf.test(data_Total)
#p-value is now smaller than printed p-value.. that means data is stationary now.
plot(data_Total)

#decomposition of time series data set - Seasonal, Trend and Random
monthplot(data_Total)
boxplot (data_Total ~cycle(data_Total))
#Decomposing the time series using STL (Seasonal and Trend decomposition using Loss).
Total_Sea <- stl(data_Total, s.window="p") #constant seasonality 
plot(Total_Sea)
#on original data ,i.e., non-stationary
Total_Sea_UnS <- stl(dem_Total, s.window="p") #constant seasonality 
plot(Total_Sea_UnS)
#according to seasonality there was increase in overall movement from 1996 to around 2002 then there was slight fall near 2003 and then it increased again till 2006
#to forecast for next quarter we have to take into account both trend and seasonality
D_names <- c('Deseasoned', 'Actual')
Deseason_Total <- (Total_Sea_UnS$time.series[,2]+Total_Sea_UnS$time.series[,3]) 
ts.plot(dem_Total, Deseason_Total, col=c("red", "blue"), main="Total Movement vs Deseasoned Movement")
#show Movement in Red and de-seasoned movement in Blue
#we can see that there is increasing trend of movement.

#dividing data into test and train
Total_Train <- window(dem_Total, start=c(1996,1), end=c(2004,4), frequency=4) 
Total_Test <- window(dem_Total, start=c(2005,1), frequency=4) 
Total_Test
#Convert into seasonal, trend and irregular component using STL
Total_Trn <- stl(Total_Train, s.window="p") 

#model building
##Random walk with drift model - Forecast on train data
fcst.Total.stl <- forecast(Total_Trn, method="rwdrift", h=4) 
Vec_Total<- cbind(Total_Test,fcst.Total.stl$mean) 
ts.plot(Vec_Total, col=c("blue", "red"),xlab="year", ylab="Total", main="Quarterly Movement Total: Actual vs Forecast")

#using acf and pacf to check the lags
acf(data_Total)
pacf(data_Total)
#based on acf and pacf, using arima(1,2,3)
fit1 <- arima(dem_Total, c(1,2,3))
fit1
#Forecasting
forecast1 <- forecast(fit1, h = 4) #h=4 for next 4 quarters
forecast1
plot(forecast1)
#the last Total point forecast will be 11429

#Mean absolute percentage error (MAPE)
## It calculates the mean absolute percentage error (Deviation) function for the forecast and the eventual outcomes.
#removing NA in Vec_Total
tail(Vec_Total)
sum(is.na(Vec_Total))
Vec_Total<-na.omit(Vec_Total)
sum(is.na(Vec_Total))
MAPEA <- mean(abs(Vec_Total[,1]-Vec_Total[,2])/Vec_Total[,1]) 
MAPEA
#From the above MAPE results we can see the 3.3 % less accuracy in model.
#Forecasting using Arima model
Total.arima.fit <- auto.arima(data_Total, seasonal=TRUE) 
Tforecast <- forecast(Total.arima.fit, h=4) 

plot(Tforecast)
Tforecast
#the point forecast will decrease in the later part of the quarter


     