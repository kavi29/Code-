#read the data
data=read.csv("Train_data.csv")


#summary 
summary(data)
str(data)


#convert to date format
data$Month=as.Date(data$Month,"%m/%d/%y%y")
View(data)


#plot the graph of complete data set
plot(data$IssuesInMonth~data$Month)
plot(data$IssuesInMonth~data$Month,type="l")


# Call for library 
library(caTools)
library(timeSeries)
library(tseries)


#Spliting  the data set into training set and test set
#set.seed(123)
#sample=sample.split(data,SplitRatio=0.9)
#train1=subset(data,sample==TRUE)
#test1=subset(data,sample==FALSE)
train1=data[1:110,]
test1=data[111:120,]


#plot the time series data
plot(train1$IssuesInMonth~train1$Month)
plot(train1$IssuesInMonth~train1$Month,type="l")


# Dickey fuller test to check stationarity
adf.test(diff(train1$IssuesInMonth), alternative="stationary", k=0)


#Acf & Pacf plot
pacf(train1$IssuesInMonth)
acf(train1$IssuesInMonth)
pacf(diff(train1$IssuesInMonth))
acf(diff(train1$IssuesInMonth))


#Apply auto arima to find order for calculating order in arima model 
auto.arima(train1$IssuesInMonth)


#ARIMA Model
model1=arima(train1$IssuesInMonth,order=c(0,1,1))
summary(model1)

#forcast the pallet for the next 10 month
predict=predict(model1, n.ahead = 10)


#error calculation
error=sum((predict$pred-test1$IssuesInMonth)/test1$IssuesInMonth)*100/10

#predication for fucture
predict_future=predict(model,n.ahead=16)
prediction=predict_future$pred[11:16]
