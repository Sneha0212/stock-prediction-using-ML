##################### Linear, Polynomial & Support Vector Regression ############

install.packages("ggplot2")
install.packages("e1071")
install.packages("cowplot")
install.packages("gridExtra")
library(ggplot2)
library(e1071)
library(caret)
library(cowplot)
library(gridExtra)

#Function for calculating model performance
modelPerformance <- function(actual, predicted){
  MAE <- mean(abs(actual - predicted), na.rm = T)
  MSE <- mean((actual - predicted)^2, na.rm = T)
  RMSE <- sqrt(mean((actual - predicted)^2, na.rm = T))
  MAPE <- mean(abs(actual - predicted)/actual, na.rm = T)
  
  return(data.frame(MAE, MSE, RMSE, MAPE))
}


# Data loading and preprossesing
#1. MSFT data
msft_data <- read.csv(file = "MSFT.csv")
#Change format of Date column to Date 
msft_data$Date <- as.Date(msft_data$Date)
ggplot(data = msft_data, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("Microsoft Stock Prices") +
  geom_smooth(method = "lm")

msft2 <- msft_data

#Adding a feature t to represent Date in numeric format
msft2$t <- as.numeric(c(1:1259))

#Split the data into train and test - use data before year 2019 as train data, and 2019 onwards as test data
msft2_split <-  split(msft2, msft2$Date < as.Date("2019-02-02"))
msft2_split$`FALSE`

msft2_train <- as.data.frame(msft2_split$`TRUE`)
msft2_test <-  as.data.frame(msft2_split$`FALSE`)

#2. AMZN data 
amzn_data <- read.csv(file = "AMZN.csv")
amzn_data$Date <- as.Date(amzn_data$Date)

ggplot(data = amzn_data, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("Amazon Stock Prices") +
  geom_smooth(method = "lm", se=FALSE)

amzn2 <- amzn_data
amzn2$t <- as.numeric(c(1:1259))

#Split the data into train and test - use data before year 2019 as train data, and 2019 onwards as test data
amzn2_split <-  split(amzn2, amzn2$Date < as.Date("2019-02-02"))
amzn2_split$`FALSE`

amzn2_train <- as.data.frame(amzn2_split$`TRUE`)
amzn2_test <-  as.data.frame(amzn2_split$`FALSE`)

#3. FB data
fb_data <- read.csv(file = "FB.csv")
fb_data$Date <- as.Date(fb_data$Date)

ggplot(data = fb_data, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("Facebook Stock Prices") +
  geom_smooth(method = "lm")

fb2 <- fb_data
fb2$t <- as.numeric(c(1:1259))

#Split the data into train and test - use data before year 2019 as train data, and 2019 onwards as test data
fb2_split <-  split(fb2, fb2$Date < as.Date("2019-02-02"))
fb2_split$`FALSE`

fb2_train <- as.data.frame(fb2_split$`TRUE`)
fb2_test <-  as.data.frame(fb2_split$`FALSE`)


#1. Linear Regression Model
#1.1 MSFT
#Train model
msft_lm <- lm(msft2_train$Adj.Close ~ Date, data = msft2_train)
summary(msft_lm)
#Predict on test data
msft_lm_pred <- predict(msft_lm, msft2_test)

#Performance of linear model
msft_lm_perf <- modelPerformance(msft2_test$Adj.Close, msft_lm_pred)


#Plotting predicted Vs Actual Values
ggplot(data = msft2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("MSFT Linear Model") +
  geom_line(aes(Date, msft_lm_pred),color='red', data = msft2_test)

#1.2. AMZN
#Train model
amzn_lm <- lm(amzn2_train$Adj.Close ~ Date , data = amzn2_train)
summary(amzn_lm)
#Predict on test data
amzn_lm_pred <- predict(amzn_lm, amzn2_test)

#Performance of linear model
amzn_lm_perf <- modelPerformance(amzn2_test$Adj.Close, amzn_lm_pred)

#Plotting predicted Vs Actual Values
 ggplot(data = amzn2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("AMZN Linear Model") +
  geom_line(aes(Date, amzn_lm_pred),color='red', data = amzn2_test)

#1.3. FB
#Train model
fb_lm <- lm(fb2_train$Adj.Close ~ Date , data = fb2_train)
summary(fb_lm)
#Predict on test data
fb_lm_pred <- predict(fb_lm, fb2_test)

#Performance of linear model
fb_lm_perf <- modelPerformance(fb2_test$Adj.Close, fb_lm_pred)

#Plotting predicted Vs Actual Values
ggplot(data = fb2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("FB Linear Model") +
  geom_line(aes(Date, fb_lm_pred),color='red', data = fb2_test)

#Plot Polynomial model prediction for all three datasets
grid.arrange(msft_lm_plot, amzn_lm_plot, fb_lm_plot, ncol = 3)


#2. Polynomial Regression Model
#2.1 MSFT
#Train model
msft_quadratic_regression = lm(Adj.Close ~ t + I(t^2), data = msft2_train) # t^2 is used to fit a curved line
summary(msft_quadratic_regression)
#Predict on test data
msft_qm_pred <- predict(msft_quadratic_regression, msft2_test)

#Performance of curvilinear model
msft_qm_perf <- modelPerformance(msft2_test$Adj.Close, msft_qm_pred)

#Plotting predicted Vs Actual Values
ggplot(data = msft2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("MSFT Polynomial Model") +
  geom_line(aes(Date, msft_qm_pred), color='red', data = msft2_test)

#2.2 AMZN
#Train model
amzn_quadratic_regression = lm(amzn2_train$Adj.Close ~ t + I(t^2), data = amzn2_train) # t^2 is used to fit a curved line
summary(amzn_quadratic_regression)
#Predict on test data
amzn_qm_pred <- predict(amzn_quadratic_regression, amzn2_test)

#Performance of curvilinear model
amzn_qm_perf <- modelPerformance(amzn2_test$Adj.Close, amzn_qm_pred)

#Plotting predicted Vs Actual Values
ggplot(data = amzn2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("AMZN Polynomial Model") +
  geom_line(aes(Date, amzn_qm_pred),color='red', data = amzn2_test)

#2.3 FB
#Train model
fb_quadratic_regression = lm(fb2_train$Adj.Close ~ t + I(t^2), data = fb2_train) # t^2 is used to fit a curved line
summary(fb_quadratic_regression)
#Predict on test data
fb_qm_pred <- predict(fb_quadratic_regression, fb2_test)

#Performance of curvilinear model
fb_qm_perf <- modelPerformance(fb2_test$Adj.Close, fb_qm_pred)

#Plotting predicted Vs Actual Values
ggplot(data = fb2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("FB Polynomial Model") +
  geom_line(aes(Date, fb_qm_pred),color='red', data = fb2_test)

#Plot Polynomial model prediction for all three datasets
grid.arrange(msft_qm_plot, amzn_qm_plot, fb_qm_plot, ncol = 3)


#3. Support Vector Regression 
#3.1 MSFT
msft_svm <- svm(Adj.Close ~ Date + Volume , data = msft2_train, kernel = "linear")
summary(msft_svm)
#predict
msft_svm_pred <- predict(msft_svm, msft2_test)

#Performance of SVM model
msft_svm_perf <- modelPerformance(msft2_test$Adj.Close, msft_svm_pred)

#plot
ggplot(data = msft2_test, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") +
  geom_line(aes(Date, msft_svm_pred), color='red', data = msft2_test)


#Tune the svm model to find the best parameters
msft_svm_tune <- tune(svm, Adj.Close ~ Date + Volume, data = msft2_train,
                      ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100), epsilon = c(0,0.1,0.5,1)), 
                      kernel = "linear")

#Print optimum value of parameters
print(msft_svm_tune)

msft_svm_best <- msft_svm_tune$best.model
summary(msft_svm_best)

#predict
msft_svm_pred2 <- predict(msft_svm_best, msft2_test)

#Performance of tuned SVM model
msft_svm_perf2 <- modelPerformance(msft2_test$Adj.Close, msft_svm_pred2)


#Plotting predicted Vs Actual Values
ggplot(data = msft2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("MSFT Support Vector Regression") +
  geom_line(aes(Date, msft_svm_pred2), color='red', data = msft2_test)


#3.2 Amazon 
#Train model
amzn_svm <- svm(Adj.Close ~ Date + Volume, data = amzn2_train, kernel = "linear")
#predict
amzn_svm_pred <- predict(amzn_svm, amzn2_test)

#Performance of SVM model
amzn_svm_perf <- modelPerformance(amzn2_test$Adj.Close, amzn_svm_pred)

#plot
ggplot(data = amzn2_test, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") +
  geom_line(aes(Date, amzn_svm_pred), color='red', data = amzn2_test)


#Tune the svm model to find the best parameters
amzn_svm_tune <- tune(svm, Adj.Close ~ Date + Volume, data = amzn2_train,
                      ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100), epsilon = c(0,0.1,0.5,1)), 
                      kernel = "linear")

#Print optimum value of parameters
print(amzn_svm_tune)

amzn_svm_best <- amzn_svm_tune$best.model
summary(amzn_svm_best)

#predict
amzn_svm_pred2 <- predict(amzn_svm_best, amzn2_test)

#Performance of tuned SVM model
amzn_svm_perf2 <- modelPerformance(amzn2_test$Adj.Close, amzn_svm_pred2)

#Plotting predicted Vs Actual Values
ggplot(data = amzn2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("AMZN Support Vector Regression") +
  geom_line(aes(Date, amzn_svm_pred2), color='red', data = amzn2_test)


#3.3 Facebook 
#Train model
fb_svm <- svm(Adj.Close ~ Date + Volume, data = fb2_train, kernel = "linear")
#predict
fb_svm_pred <- predict(fb_svm, fb2_test)

#Performance of SVM model
fb_svm_perf <- modelPerformance(fb2_test$Adj.Close, fb_svm_pred)

#plot
ggplot(data = fb2_test, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") +
  geom_line(aes(Date, fb_svm_pred), color='red', data = fb2_test)


#Tune the svm model to find the best parameters
fb_svm_tune <- tune(svm, Adj.Close ~ Date , data = fb2_train,
                      ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100), epsilon = c(0,0.1,0.5,1)), 
                      kernel = "linear")

#Print optimum value of parameters
print(fb_svm_tune)

fb_svm_best <- fb_svm_tune$best.model
summary(fb_svm_best)

#predict
fb_svm_pred2 <- predict(fb_svm_best, fb2_test)

#Performance of tuned SVM model
fb_svm_perf2 <- modelPerformance(fb2_test$Adj.Close, fb_svm_pred2)


#Plotting predicted Vs Actual Values
ggplot(data = fb2, mapping = aes(Date, Adj.Close)) + geom_point() +
  geom_line() + xlab("Date") + ylab("Adj.Close") + ggtitle("FB Support Vector Regression") +
  geom_line(aes(Date, fb_svm_pred2), color='red', data = fb2_test)




################################# ARIMA #############################################
install.packages(c("scales","zoo","forecast","tseries","fpp","ggplot2"))
install.packages("dplyr")
install.packages("MLmetrics")
library(scales)
library(zoo)
library(forecast)
library(tseries)
library(fpp)
library(ggplot2)
library(dplyr)


#facebook
facebook <- read.csv(file.choose())
facebook <- select(facebook,-Open,-High,-Low,-Adj.Close,-Volume)
facebook.ts <- as.ts(
  data.frame(
    Date = rev(facebook$Date),
    Value = rev(facebook$Close)
  )
)

View(facebook.ts)
facebook.ts <- facebook.ts[,-1]

adf.test(facebook.ts)
kpss.test(facebook.ts)

facebook882 <- head(facebook.ts,882) 
View(facebook882)


# plotting timeseries

autoplot(facebook882, geom = "line") + xlab("Time")+ylab("Value")


#finding plot and trend 

facebook.ts_trend <- ma(facebook882, order = 4, centre = T)
autoplot(facebook.ts_trend, geom = "line") + xlab("Time")+ylab("Value")
plot(facebook.ts_trend)

#removing trend and plotting de-trended series

facebook.ts_detrended <- facebook882 - facebook.ts_trend
autoplot(facebook.ts_detrended, geom = "line") + xlab("Time")+ylab("Value")

#avg sesonality

facebook.ts_avg = t(matrix(data = facebook.ts_detrended, nrow = 4))
facebook.ts_seasonal = colMeans(facebook.ts_avg, na.rm = T)
autoplot(as.ts(rep(facebook.ts_seasonal,16)), geom = "line") + xlab("Time") +
  ylab("Value")

#plot stochastic series

facebook.ts_stochastic <- facebook.ts_detrended - facebook.ts_seasonal
autoplot(facebook.ts_stochastic, geom = "line") + xlab("Time")+ylab("Value")


acf(facebook882)
pacf(facebook882)
acf(na.remove(facebook.ts_stochastic))
pacf(na.remove(facebook.ts_stochastic))

ar.model <- auto.arima(facebook882)
arimaorder(ar.model)


fcast <- forecast(ar.model, h = 377)
print(fcast)
plot(fcast)

View(fcast)

#testing
test <- tail(facebook.ts,337)
plot(test)
test.ts <-as.ts(test)
plot(test.ts)

library(MLmetrics)
MAPE(as.numeric(fcast$fitted),as.numeric(test.ts))


#amazon

amazon <- read.csv(file.choose())
amazon <- select(amazon,-Open,-High,-Low,-Adj.Close,-Volume)
amazon.ts <- as.ts(
  data.frame(
    Date = rev(amazon$Date),
    Value = rev(amazon$Close)
  )
)

View(amazon.ts)
amazon.ts <- amazon.ts[,-1]

adf.test(amazon.ts)
kpss.test(amazon.ts)

amazon882 <- head(amazon.ts,882) 
View(amazon882)

# plotting timeseries

autoplot(amazon882, geom = "line") + xlab("Time")+ylab("Value")


#finding plot and trend 

amazon.ts_trend <- ma(amazon882, order = 4, centre = T)
autoplot(amazon.ts_trend, geom = "line") + xlab("Time")+ylab("Value")
plot(amazon.ts_trend)

#removing trend and plotting de-trended series

amazon.ts_detrended <- amazon882 - amazon.ts_trend
autoplot(amazon.ts_detrended, geom = "line") + xlab("Time")+ylab("Value")

#avg sesonality

amazon.ts_avg = t(matrix(data = amazon.ts_detrended, nrow = 4))
amazon.ts_seasonal = colMeans(amazon.ts_avg, na.rm = T)
autoplot(as.ts(rep(amazon.ts_seasonal,16)), geom = "line") + xlab("Time") +
  ylab("Value")

#plot stochastic series

amazon.ts_stochastic <- amazon.ts_detrended - amazon.ts_seasonal
autoplot(amazon.ts_stochastic, geom = "line") + xlab("Time")+ylab("Value")


acf(amazon882)
pacf(amazon882)
acf(na.remove(amazon.ts_stochastic))
pacf(na.remove(amazon.ts_stochastic))

ar.model <- auto.arima(amazon882)
arimaorder(ar.model)


acast <- forecast(ar.model, h = 377)
print(acast)
plot(acast)



#testing
test <- tail(facebook.ts,337)
plot(test)
test.ts <-as.ts(test)
plot(test.ts)

MAPE(as.numeric(acast$fitted),as.numeric(test.ts))





#microsoft


microsoft <- read.csv(file.choose())
microsoft <- select(microsoft,-Open,-High,-Low,-Adj.Close,-Volume)
microsoft.ts <- as.ts(
  data.frame(
    Date = rev(microsoft$Date),
    Value = rev(microsoft$Close)
  )
)

View(microsoft.ts)
microsoft.ts <- microsoft.ts[,-1]

adf.test(microsoft.ts)
kpss.test(microsoft.ts)

microsoft882 <- head(microsoft.ts,882) 
View(microsoft882)

# plotting timeseries

autoplot(microsoft882, geom = "line") + xlab("Time")+ylab("Value")


#finding plot and trend 

microsoft.ts_trend <- ma(microsoft882, order = 4, centre = T)
autoplot(microsoft.ts_trend, geom = "line") + xlab("Time")+ylab("Value")
plot(microsoft.ts_trend)

#removing trend and plotting de-trended series

microsoft.ts_detrended <- microsoft882 - microsoft.ts_trend
autoplot(microsoft.ts_detrended, geom = "line") + xlab("Time")+ylab("Value")

#avg sesonality

microsoft.ts_avg = t(matrix(data = microsoft.ts_detrended, nrow = 4))
microsoft.ts_seasonal = colMeans(microsoft.ts_avg, na.rm = T)
autoplot(as.ts(rep(microsoft.ts_seasonal,16)), geom = "line") + xlab("Time") +
  ylab("Value")

#plot stochastic series

microsoft.ts_stochastic <- microsoft.ts_detrended - microsoft.ts_seasonal
autoplot(microsoft.ts_stochastic, geom = "line") + xlab("Time")+ylab("Value")


acf(microsoft882)
pacf(microsoft882)
acf(na.remove(microsoft.ts_stochastic))
pacf(na.remove(microsoft.ts_stochastic))

ar.model <- auto.arima(microsoft882)
arimaorder(ar.model)


mcast <- forecast(ar.model, h = 377)
print(mcast)
plot(mcast)


#testing
test <- tail(facebook.ts,337)
plot(test)
test.ts <-as.ts(test)
plot(test.ts)

MAPE(as.numeric(mcast$fitted),as.numeric(test.ts))




########################## Multiple Linear Regression #########################

install.packages("ISLR")
library("ISLR")
install.packages("tidyverse")
library("tidyverse")
install.packages("dplyr")
library("dplyr")
install.packages("visreg")
library("visreg")
install.packages("scatterplot3d")
library("scatterplot3d")
install.packages("ggplot2")
library("ggplot2")

## 1. Amazon
AMZ <- read.csv("AMZN.csv")

head(AMZ)
str(AMZ)
summary(AMZ)


AMZ$Open <- as.factor(AMZ$Open)
str(AMZ)
AMZ$Close <- as.factor(AMZ$Close)
str(AMZ)
AMZ$High <- as.factor(AMZ$High)
str(AMZ)
AMZ$Low <- as.factor(AMZ$Low)
str(AMZ)

trainn <- AMZ[1:630,]
summary(trainn)
testt <- AMZ[631:1260,]
summary(testt)

# APPLYING MULTIPLE LINEAR REGRESSION MODEL

trainn_model <- lm(AMZ$Volume ~ AMZ$High + AMZ$Low + AMZ$Open + AMZ$Close, data = trainn)
trainn_model

#SHOW RESULTS

summary(trainn_model)
predictor <- predict(trainn_model, newdata = testt)
summary(predictor)

confmatrix <- sum(diag(predictor)) / sum(predictor)
confmatrix

#PLOT

scatterplot3d(AMZ)


## 2. Microsoft
MIC <- read.csv("MSFT.csv")

head(MIC)
str(MIC)
summary(MIC)



MIC$Open <- as.factor(MIC$Open)
str(MIC)
MIC$Close <- as.factor(MIC$Close)
str(MIC)
MIC$High <- as.factor(MIC$High)
str(MIC)
MIC$Low <- as.factor(MIC$Low)
str(MIC)

trainn <- MIC[1:630,]
summary(trainn)
testt <- MIC[631:1260,]
summary(testt)

# APPLYING MULTIPLE LINEAR REGRESSION MODEL

trainn_model <- lm(MIC$Volume ~ MIC$High + MIC$Low + MIC$Open + MIC$Close, data = trainn)
trainn_model

#SHOW RESULTS

summary(trainn_model)
predictor <- predict(trainn_model, newdata = testt)
summary(predictor)

confmatrix <- sum(diag(predictor)) / sum(predictor)
confmatrix

#PLOT

scatterplot3d(MIC)

## 3. Facebook
ABC <- read.csv("FB.csv")


head(ABC)
str(ABC)
summary(ABC)



ABC$Open <- as.factor(ABC$Open)
str(ABC)
ABC$Close <- as.factor(ABC$Close)
str(ABC)
ABC$High <- as.factor(ABC$High)
str(ABC)
ABC$Low <- as.factor(ABC$Low)
str(ABC)

trainn <- ABC[1:630,]
summary(trainn)
testt <- ABC[631:1260,]
summary(testt)

# APPLYING MULTIPLE LINEAR REGRESSION MODEL

trainn_model <- lm(ABC$Volume ~ ABC$High + ABC$Low + ABC$Open + ABC$Close, data = trainn)
trainn_model

#SHOW RESULTS

summary(trainn_model)
predictor <- predict(trainn_model, newdata = testt)
summary(predictor)

confmatrix <- sum(diag(predictor)) / sum(predictor)
confmatrix

#PLOT

scatterplot3d(ABC)

