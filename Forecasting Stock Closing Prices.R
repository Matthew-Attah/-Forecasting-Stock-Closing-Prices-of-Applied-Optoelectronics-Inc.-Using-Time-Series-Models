# Load and install necessary packages and libraries
library(tidyverse)
library(lubridate)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)


# Load the dataset
data <- read.csv("AAON.csv")

#View first 5 rows of the dataset
head(data)

# Correct the Date column
data$Date <- ymd(data$Date)

# Confirm the range of dates
summary(data$Date)

#Filter the data to show the starting date and last date of the dataset
data <- data %>%
  filter(Date >= as.Date("1992-12-16") & Date <= as.Date("2020-04-01"))

# Select the Close prices for plotting
company_data <- data %>% select(Date, Close)


#Convert to Time Series
ts_data <- ts(company_data$Close, start = c(1992, 350), frequency = 365)

#plot time series data
plot(ts_data, main="AAON Close Price", xlab="Date", ylab="Close Price", col="red")

#log and plot the time series
logts_data <- log(ts_data) 
plot.ts(logts_data) 

#Decompose data
ts_decomposed<-decompose(logts_data, type="additive")

# Get the estimated values of the seasonal component
ts_decomposed$seasonal
plot(ts_decomposed)

#plot the ts_decomposedseasonallyadjusted
ts_decomposedseasonallyadjusted <- ts_data  - ts_decomposed$seasonal
plot(ts_decomposedseasonallyadjusted)


# Holt-Winters Model
hw_model <- HoltWinters(logts_data)
hw_model
hw_model$SSE

plot(hw_model)

# Forecast using Holt-Winters
hw_forecast <- forecast(hw_model, h=365) # Forecast one year ahead
plot(hw_forecast)

acf(hw_forecast$residuals, lag.max=20, na.action = na.pass) 
# Perform the Ljung-Box test on the residuals
Box.test(hw_forecast$residuals, lag = 20, type = "Ljung-Box")

# make time series plot
plot.ts(hw_forecast$residuals) 
hw_forecast$residuals <- hw_forecast$residuals[!is.na(hw_forecast$residuals)] 

# Plot histogram of residuals
hist(hw_forecast$residuals, main = "Histogram of Holt-Winters Residuals",
     xlab = "Residuals", col = "red", border = "black", freq = FALSE)

# Add a normal curve based on the residuals
curve(dnorm(x, mean = mean(hw_forecast$residuals), sd = sd(hw_forecast$residuals)), 
      col = "blue", lwd = 2, add = TRUE)

#Testing for Stationarity Using ADF. 
#if the p-value > 0.05 then it is not stationary 
ts_adf<-adf.test(ts_data)
ts_adf

#since our data is not stationary we will difference to obtain stationarity 
diff1_data<-diff(ts_data, differences = 1)
diff1_data

#plot the difference data
plot(diff1_data)

#re-confirm the stationarity using adf
confirm_adf<-adf.test(diff1_data)
confirm_adf

#plot the correlogram
acf(diff1_data, lag.max=20) 
# get the autocorrelation values
acf(diff1_data, lag.max=20, plot=FALSE)

# plot a partial correlogram
pacf(diff1_data, lag.max=20) 
# get the partial autocorrelation values
pacf(diff1_data, lag.max=20, plot=FALSE) 


# Fit an ARIMA model (Automatic model selection)
arima_model <- auto.arima(diff1_data)

# Print ARIMA model summary
arima_model

# Forecast the next 365 days
forecasted_values <- forecast(arima_model, h = 365)
forecasted_values

# Plot the forecast
autoplot(forecasted_values) +
  labs(title = "ARIMA Forecast of Share Price (Close) for the Company",
       x = "Date", y = "Forecasted Close Price") +
  theme_minimal()


#Check for Normality, Constant Variance and Autocorrelation
acf(forecasted_values$residuals, lag.max=20) 
Box.test(forecasted_values$residuals, lag=20, type="Ljung-Box")

plot.ts(forecasted_values$residuals)

# Plot histogram of residuals
hist(forecasted_values$residuals, main = "Residuals",
     xlab = "Residuals", col = "red", border = "black", freq = FALSE)

# Add a normal curve based on the residuals
curve(dnorm(x, mean = mean(forecasted_values$residuals), sd = sd(forecasted_values$residuals)), 
      col = "blue", lwd = 2, add = TRUE)


