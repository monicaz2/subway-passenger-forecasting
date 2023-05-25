library('forecast')
library(zoo)
library(xts)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tseries)
library(xts)

# Read in the data
data <- read.csv("/Users/monicazhang/Documents/Spring 2023/Time Series Analysis and Forecasting/CTA_-_Ridership_-_Daily_Boarding_Totals.csv", header = TRUE)
head(data)

#W = Weekday, A = Saturday, U = Sunday/Holiday
# * How people are counted on the 'L' *
#On the rail system, a customer is counted as an "entry" each time he or she passes through a turnstile to enter a station.  
#Customers are not counted as "entries" when they make a "cross-platform" transfer from one rail line to another, since they don't pass through a turnstile.

#passenger number is in 10k
data$rail_boardings<- data$rail_boardings/10000

ts <- xts(data$rail_boardings, order.by = as.Date(data$service_date, format = "%m/%d/%Y"), frequency = 365)

plot(ts, main = "Time Series Plot - CTA", ylab = "Total Passengers")

data$service_date <- as.Date(data$service_date, format = "%m/%d/%Y")

filtered_data <- data[data$service_date >= "2015-01-01" & data$service_date <= "2018-12-31", ]
filtered_data_2018 <- data[data$service_date >= "2018-01-01" & data$service_date <=  "2018-12-31", ]
testing_data <- data[data$service_date >= "2019-01-01" & data$service_date <= "2019-12-31", ]

#check for null values
sum(is.na(filtered_data$rail_boardings))
sum(is.na(filtered_data$testing_data))
sum(is.na(filtered_data$service_date))
sum(is.na(filtered_data$service_date))

# Create the time series object
ts_cta <- xts(filtered_data$rail_boardings, order.by = as.Date(filtered_data$service_date, format = "%m/%d/%Y"), frequency = 365.25)
ts_cta_2018 <- xts(filtered_data_2018$rail_boardings, order.by = as.Date(filtered_data_2018$service_date, format = "%m/%d/%Y"), frequency=365.25)

ts_test <- xts(testing_data$rail_boardings, order.by = as.Date(testing_data$service_date, format = "%m/%d/%Y"), frequency=365.25)

plot(ts_cta, main = "Chicago Rail Boardings", ylab = "Total Passengers (in 10,000)")
plot(ts_cta_2018, main = "Chicago Subway Boardings in 2018", ylab = "Total Passengers (in 10,000)")
plot(ts_test, main = "Chicago Rail Boardings", ylab = "Total Passengers")

acf(ts_cta) #ts looks non-stationary

pacf(ts_cta)

adf_test <- adf.test(ts_cta)
print(adf_test) #adf test - ts is stationary

kpss.test(ts_cta)  #ts is non-stationary

filtered_data <- filtered_data %>% 
  mutate(month = format(service_date, "%b"),
         day_of_week = format(service_date, "%A"))

# Set the order of months and weekdays
filtered_data$month <- factor(filtered_data$month, levels = month.abb)
filtered_data$day_of_week <- factor(filtered_data$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Boxplot by Month with Color
ggplot(filtered_data, aes(x = month, y = rail_boardings, fill = month)) +
  geom_boxplot() +
  xlab("Month") +
  ylab("Rail Boardings") +
  ggtitle("Distribution of Rail Boardings by Month") +
  scale_fill_discrete(name = "Month")
#we notice monthly seasonality

# Boxplot by Day of the Week with Color
ggplot(filtered_data, aes(x = day_of_week, y = rail_boardings, fill = day_of_week)) +
  geom_boxplot() +
  xlab("Day of the Week") +
  ylab("Rail Boardings") +
  ggtitle("Distribution of Rail Boardings by Day of the Week") +
  scale_fill_discrete(name = "Day of the Week")
#we daily  seasonality

ggplot(filtered_data, aes(x = day_type, y = rail_boardings, fill = day_type)) +
  geom_boxplot() +
  xlab("Day Type") +
  ylab("Total Rides") +
  ggtitle("Ridership Distribution by Day Type") +
  scale_fill_discrete(
    name = "Day Type",
    labels = c("A" = "Saturday", "U" = "Sunday/Holiday", "W" = "Weekday")
  )

ts_data_daily <- ts(filtered_data$rail_boardings, frequency = 365.25)
plot(ts_data_daily)

stl_model <- stl(ts_data_daily, s.window='periodic')
autoplot(stl_model)

decomposed <- decompose(ts_data_daily)
# Plot the components
plot(decomposed)

adf_test <- adf.test(ts_data_daily)
print(adf_test) #adf test - ts is stationary

kpss.test(ts_data_daily)  #ts is non-stationary

# Assuming you have a time series object called 'ts_data'
periodogram <- spec.pgram(ts_cta)
plot(periodogram, main = "Periodogram of Time Series")

ts_data_weekly <- ts(filtered_data$rail_boardings, frequency = 7)
plot(ts_data_weekly)

stl_model_weekly <- stl(ts_data_weekly, s.window='periodic')
autoplot(stl_model_weekly)

ts_data_monthly <- ts(filtered_data$rail_boardings, frequency = 30)
plot(ts_data_monthly)

stl_model_monthly <- stl(ts_data_monthly, s.window='periodic')
autoplot(stl_model_monthly)


baseline <- auto.arima(ts_cta)
summary(baseline)

#SARIMA - daily
sarima_daily <- auto.arima(ts_data_daily, seasonal = TRUE, D=1)
summary(sarima_daily)
checkresiduals(sarima_daily) #there is correlation in the residuals
forecast_daily <- forecast(sarima_daily, h = 365)
plot(forecast_daily)
accuracy(forecast_daily, ts_test) 

sarima_weekly <- auto.arima(ts_data_weekly, seasonal = TRUE, D=1)
summary(sarima_weekly)
forecast_weekly <- forecast(sarima_weekly, h = 52)
plot(forecast_weekly)
accuracy(forecast_weekly, ts_test)

sarima_monthly <- auto.arima(ts_data_monthly, seasonal = TRUE, D=1)
summary(sarima_monthly)
forecast_monthly <- forecast(sarima_monthly, h = 12)
plot(forecast_monthly)

# Convert 'day_type' to numeric codes
filtered_data$day_type_code <- as.numeric(factor(filtered_data$day_type, levels = c("A", "U", "W"), labels = c(1, 2, 3)))
testing_data$day_type_code <- as.numeric(factor(testing_data$day_type, levels = c("A", "U", "W"), labels = c(1, 2, 3)))

xreg <- ts(filtered_data$day_type_code, frequency = 365)
xreg_test <- ts(testing_data$day_type_code, frequency = 365)

# ARIMAX
arima <- auto.arima(ts_cta, xreg = xreg)
summary(arima)
forecast_arima <- forecast(arima, h = 365, xreg=xreg_test)
plot(forecast_arima)
accuracy(forecast_arima, ts_test)

#TBATS
ts_week_day <- msts(filtered_data$rail_boardings, seasonal.periods = c(7, 365.25))
tbats_week_day <- tbats(ts_week_day)
summary(tbats_week_day)

forecast_week_day <- forecast(tbats_week_day, h = 365)
plot(forecast_week_day)
accuracy(forecast_week_day, ts_test)

