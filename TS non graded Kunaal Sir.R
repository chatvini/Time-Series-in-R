#1/ Import libraries
# plotting data
library(ggplot2)

# handling time series data
library(tseries)
library(forecast)
library(timeSeries)

# handling data
library(dplyr)

# handling dates
library(plyr)

#multi plot
library(fpp)

library(prophet)

# 2/ Import data
train <- read.csv('/Users/macmojave/Downloads/u-j-time-series-forecasting-case-study/train.csv')
test <- read.csv('/Users/macmojave/Downloads/u-j-time-series-forecasting-case-study/test.csv')
sample <- read.csv('/Users/macmojave/Downloads/u-j-time-series-forecasting-case-study/sample_submission.csv')
hl <- read.csv('/Users/macmojave/Downloads/u-j-time-series-forecasting-case-study/holidays.csv')

# 3/ Check data
head(train)
head(test)
head(sample)
head(hl)

# 4/ Basic data Sanity
# Convert date in proper format
train$date<-as.Date(train$date, format="%d-%m-%Y")
head(train$date)
colSums(is.na(train))
# Counting number of days in each year
TotalDays = data.frame(train$date, year = strftime(train$date, "%Y"))
count(TotalDays, "year")
# Finding missing days in train dataset
date_range <- seq(min(train$date), max(train$date), by = 1) 
MissingDays<-date_range[!date_range %in% train$date] 
MissingDays
# Create data frame of missing values with 0
MissingDaysDF <- data.frame(MissingDays,as.integer(0))
colnames(MissingDaysDF) <- c("date", "transactions")
MissingDaysDF
# Find average of 1 Jan from previous years
Find_1JanValues <- train %>% filter(train$date == c('2013-01-01','2014-01-01','2015-01-01'))
mean(Find_1JanValues$transactions)
# Update value for 2016-01-01 in MissingDaysDF
MissingDaysDF$transactions[MissingDaysDF$date == '2016-01-01'] <- 3395
MissingDaysDF
# Find average of 3 Jan from previous years
Jan3_2013 <-train %>% filter(train$date == '2013-01-03')
Jan3_2014 <-train %>% filter(train$date == '2014-01-03')
Jan3_2015 <-train %>% filter(train$date == '2015-01-03')
round((Jan3_2013$transactions+Jan3_2014$transactions+Jan3_2015$transactions)/3,0)

# Update value for 2016-01-03 in MissingDaysDF
MissingDaysDF$transactions[MissingDaysDF$date == '2016-01-03'] <- 88407.5
MissingDaysDF

# Create new train dataset with missing days
MissingDaysDF$transactions<-as.integer(MissingDaysDF$transactions)
train1<-rbind(train, MissingDaysDF)
train1<-train1[order(train1$date, decreasing = FALSE), ]
head(train1,n=10)
tail(train1,n=10)

boxplot(train1$transactions)


# Clean time series data for outliers in time series data
train1$transactions<-tsclean(train1$transactions, replace.missing = TRUE, iterate = 2, lambda = "auto")
boxplot(train1$transactions)

# Convert date in proper format for holidays list
hl$Date<-as.Date(hl$Date, format="%d-%m-%Y")
colnames(hl) <- c("date", "Holidays")
head(hl)

# Create new variable 'Holidays' to identify days with holidays for all rows in train
train2 <- left_join(train1,hl,by='date')
train2$Holidays[is.na(train2$Holidays)] <- 0
head(train2)

# Extract 'Holidays' variable separately
Holidays <- train2$Holidays
length(Holidays)

# 5/ EDA - Plot Data
plot(train1$date,train1$transactions, type = "s")

# 5.1
# Decomposition
# Convert transactions to time series
ts_train <- ts(train1$transactions, frequency = 365, start = c(2013,1))
plot.ts(ts_train)

# Check STL of transactions
ts_train_stl<-stl(ts_train,s.window = "periodic")
plot(ts_train_stl)

# Check decompose of transactions
ts_train_deco<-decompose(ts_train,type="additive")
plot(ts_train_deco)

#Desire to fit ARIMA model

# Check if time series is stationary or not
adf.test(ts_train) # P-value is low , TS in stationary

fitlnstock <- auto.arima(ts_train)
fitlnstock

#finalmodel
fitlnstock1 <- arima(ts_train, order = c(5,0,2))
fitlnstock1

# Let's do forecasting on the outcome of autoarima
fv <- forecast(fitlnstock1, 227)
fv
length(fv$x)
fv$x  # Point forecast
plot(fv)

fv

fit1<- meanf(ts_train,h=227)
fit2<- naive(ts_train,h=227)
fit3<- snaive(ts_train,h=227)
fit3

plot(fit1, plot.conf= FALSE, main= 'Forecast for transactions') 
lines(fit2$mean,col=2)
lines(fit3$mean,col=3)
legend("bottomright", lty=1, col= c(4,2,3),
       legend = c("Mean method", "Naive method", "Seasonal Naive method"))

finalforecastvalue <- as.numeric(fit3$x)
finalforecastvalue 
length(finalforecastvalue)
length(final)
length(fv$x)
length(test$id)
length(fit3$mean)


# 8/ Check Performance
# Diagnosis
# ARIMA Model
accuracy(fv$fitted,train1$transactions)

length(fit3$fitted)
length(train1$transactions)

percentage_error <- abs(((train1$transactions - fv$fitted))/train1$transactions)
percentage_error

#MAPE
mean(percentage_error)

# RMSE
sq_error<- (train1$transactions - fv$fitted)^2
MSE <- mean(sq_error)
RMSE <- sqrt(MSE)
RMSE

# 10/ Export results
df <- data.frame(test$id, fit3$mean)
View(df)

colnames(df) <- c("id", "transactions")

write.csv(df,"/Users/macmojave/Downloads/submission6.csv", row.names = FALSE, na = '0')


#Using Prophet package
# For Prophet package - colnames should be "ds" and "y". 
colnames(train1) <- c("ds", "y")
head(train1)

#Submission 2 <- with first obs 1433

# model building 
m <- prophet(train1)
m  # Please closely check various objects of this "m". 

# Create a future dataset 
future <- make_future_dataframe(m, periods = 227)

future11 <- predict(m, future)
View(future11)
# Visualize 
plot(m, future11)

# Compare forecasted values against actual values 
final <- cbind( test$id, future11$yhat[1462:1688])
View(final)

# %error in each prediction 
# overall mean of %error for each prediction 

prophet_plot_components(m, future11)

colnames(final) <- c("id", "transactions")

write.csv(final,"/Users/macmojave/Downloads/submission3.csv", row.names = FALSE, na = '0')


#Submission 4 considering seasonality
# model building 
m <- prophet(train1, daily.seasonality=TRUE)
m  # Please closely check various objects of this "m". 

# Create a future dataset 
future <- make_future_dataframe(m, periods = 227)

future11 <- predict(m, future)
View(future11)
# Visualize 
plot(m, future11)

# Compare forecasted values against actual values 
final <- cbind( test$id, future11$yhat[1462:1688])
View(final)

# %error in each prediction 
# overall mean of %error for each prediction 

prophet_plot_components(m, future11)

colnames(final) <- c("id", "transactions")

#write.csv(final,"/Users/macmojave/Downloads/submission4.csv", row.names = FALSE, na = '0')
#write.csv(final,"/Users/macmojave/Downloads/submission8.csv", row.names = FALSE, na = '0')
write.csv(final,"/Users/macmojave/Downloads/submission11.csv", row.names = FALSE, na = '0')


# Improved Submission 4 by changing obs 1 with 1433
# Improved Submission 3 by changing obs 1 with 1433
