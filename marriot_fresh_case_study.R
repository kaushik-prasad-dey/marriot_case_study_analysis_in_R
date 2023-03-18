#libs declaration
library(readr)
library(ggplot2)
library(forecast)
library(TTR)
library(dplyr)
library(readxl)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(tseries)
#get current directory
getwd() 
#read csv file
library(readxl)
file_path <- paste(getwd(), "mariot_data.xlsx", sep ="/")
marriot_data_set<-read_excel(file_path)
#view csv file
View(marriot_data_set)
#summary of the dataset
summary(marriot_data_set)
#calculate the saturday demand 
Sat_Demand_forcasting <- marriot_data_set %>% 
  filter(`DOW INDICATOR1`==1 & `DEMAND` != 'NA') %>% 
  select(`DOW INDICATOR1`,`DEMAND`)
#view the saturday demand caulative dataset
view(Sat_Demand_forcasting)
library (plyr)

#find out the critical ratio
Cost_of_shortage <- 180
Cost_of_coverge <- 90
Critical_ratio <- Cost_of_shortage/(Cost_of_coverge+Cost_of_shortage)
Critical_ratio
#find out the saturday demand calculations
Sat_demand_Size <-length(Sat_Demand_forcasting$DEMAND)
Sat_demand_Size
Sat_demand_Mean_value <- mean(Sat_Demand_forcasting$DEMAND)
Sat_demand_Mean_value
Sat_demand_Sd <- sd(Sat_Demand_forcasting$DEMAND)
Sat_demand_Sd
#find out the forecast value for Saturay based on demand
Sat_forecasting_value <- qnorm(
  p = Critical_ratio, 
  mean = Sat_demand_Mean_value, 
  sd = Sat_demand_Sd
)
Sat_forecasting_value
paste0('Saturday demand calculations can be predicted' ," : ",Sat_forecasting_value)

#now we have to find out the calculations as time series dataset.
Predicted_TimeSeries <- marriot_data_set %>% 
  filter(`PICKUP RATIO` != 'NA') %>% 
  select(Dates,`PICKUP RATIO`)

Predicted_TimeSeries$Dates =ymd(Predicted_TimeSeries$Dates)
Predicted_TimeSeries
#convert predicted time series data into TS actual Conversion with seasonality
TimeSeries_ts = ts(Predicted_TimeSeries[,2],start = c(1987,05,23),frequency=7)
plot(TimeSeries_ts, xlab='Years', ylab='pickup Ratio')

#decompostion time series model(multiplicative)
TimeSeries_ts_decompose_multiple = decompose(TimeSeries_ts, type=c("multiplicative"))
summary(TimeSeries_ts_decompose_multiple)
plot(TimeSeries_ts_decompose_multiple)

#decompostion time series model(additive)
TimeSeries_ts_decompose_additive = decompose(TimeSeries_ts, type=c("additive"))
summary(TimeSeries_ts_decompose_additive)
plot(TimeSeries_ts_decompose_additive)


#plot the pickup ratio data
title_bar <- "Historical Pick up Ratio Data Plotting..."
x_bar <- "Time Series Date"
y_bar <- "Pick Ratio Analysis"
graph_plotting<-ggplot(Predicted_TimeSeries) +
  aes(x = Dates, y = `PICKUP RATIO`) +
  geom_line(size = 0.5, colour = "red") +
  labs(x = x_bar, y = y_bar,title = title_bar) +
  theme_gray()
ggplotly(graph_plotting)

#HoltWinter Exponential Smoothing method
marriot_holt_expo <- HoltWinters(TimeSeries_ts, gamma = FALSE, beta = FALSE)
summary(marriot_holt_expo)
plot(marriot_holt_expo)
checkresiduals(marriot_holt_expo)

#HoltWinter's Multiplecative model
marriot_HoltWinter_multi <- hw(TimeSeries_ts,seasonal= "multiplicative",damped = TRUE)
summary(marriot_HoltWinter_multi)
plot(marriot_HoltWinter_multi)
checkresiduals(marriot_HoltWinter_multi)

#HoltWinter's additive model
marriot_HoltWinter_additive <- hw(TimeSeries_ts,seasonal= "additive",damped = TRUE)
summary(marriot_HoltWinter_additive)
plot(marriot_HoltWinter_additive)
summary(TimeSeries_HoltWinter)
checkresiduals(marriot_HoltWinter_additive)

#arima Model analysis
marriot_Arima <- auto.arima(TimeSeries_ts)
summary(marriot_Arima)
plot(marriot_Arima)
checkresiduals(marriot_Arima)

#Simple moving average model
marriot_hotel_SMA <- SMA(TimeSeries_ts,n=3)
plot.ts(marriot_hotel_SMA)
summary(marriot_hotel_SMA)

#Simple moving average model of mulitple MA
autoplot(TimeSeries_ts, series = "Data") + 
  autolayer(ma(TimeSeries_ts, 3), series = "3 month Moving Average") +
  autolayer(ma(TimeSeries_ts, 5), series = "5 month Moving Average") +
  autolayer(ma(TimeSeries_ts, 10), series = "10 month Moving Average") +
  xlab("Year") +  ylab("Pickup Ratio") +
  theme_gray()

#3 months moving average
marriot_SMA3 <- SMA(TimeSeries_ts, n=3)
summary(marriot_SMA3)
plot.ts(marriot_SMA3)

#Mean method
marriot_MA3 <- meanf(TimeSeries_ts, h=3)
autoplot(marriot_MA3,xlab = "Year", ylab = "pickup Ratio")
summary(marriot_MA3)
checkresiduals(marriot_MA3)

#5 months moving average
marriot_SMA5 <- SMA(TimeSeries_ts, n=5)
summary(marriot_SMA5)
plot.ts(marriot_SMA5)
#Mean method
marriot_MA5 <- meanf(TimeSeries_ts, h=5)
autoplot(marriot_MA5,xlab = "Year", ylab = "pickup Ratio")
summary(marriot_MA5)
checkresiduals(marriot_MA5)

#Naive method for 3 months
marriot_hotel_Naive_3 <- naive(TimeSeries_ts, h=3)
autoplot(marriot_hotel_Naive_3,xlab = "Year", ylab = "pickup Ratio")
summary(marriot_hotel_Naive_3)
checkresiduals(marriot_hotel_Naive_3)

#Naive method for 5 months
marriot_hotel_Naive_5 <- naive(TimeSeries_ts, h=5)
autoplot(marriot_hotel_Naive_5,xlab = "Year", ylab = "pickup Ratio")
summary(marriot_hotel_Naive_5)
checkresiduals(marriot_hotel_Naive_5)

#Additive decomposition of time series
marriot_decompose_additive = decompose(TimeSeries_ts, type=c("additive"))
plot(marriot_decompose_additive)
summary(marriot_decompose_additive)

#multiplicative decomposition of time series
marriot_decompose_multiplicative = decompose(TimeSeries_ts, type=c("multiplicative"))
plot(marriot_decompose_multiplicative)
summary(marriot_decompose_multiplicative)

#Augmented Dickey-Fuller Test
adf_result<-adf.test(TimeSeries_ts,k = 7)
adf_result