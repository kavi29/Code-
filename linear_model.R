#read the data
data= read.csv("Train_data.csv")
future_data=read.csv("Test_data.csv")

#convert into date format
data$Month = as.Date(data$Month,format='%m/%d/%Y')

#call the library
library(caTools)


#split the data into train and test set
set.seed(123)
sample=sample.split(data,SplitRatio=0.9)
train=subset(data,sample==TRUE)
test=subset(data,sample==FALSE)


#plot the graph
plot(data$IssuesInMonth~data$Month,type="l")
plot(data$IssuesInMonth~data$BusinessDaysInMonth)
plot(data$IssuesInMonth~data$TransfersInMonth)

#Normalization
test$TransfersInMonth = (test$TransfersInMonth - mean(data$TransfersInMonth)) / sd(data$TransfersInMonth)
test$BusinessDaysInMonth = (test$BusinessDaysInMonth - mean(data$BusinessDaysInMonth)) / sd(data$BusinessDaysInMonth)
train$BusinessDaysInMonth = (train$BusinessDaysInMonth - mean(data$BusinessDaysInMonth)) / sd(data$BusinessDaysInMonth)
train$TransfersInMonth = (train$TransfersInMonth - mean(data$TransfersInMonth)) / sd(data$TransfersInMonth)
future_data$TransfersInMonth = (future_data$TransfersInMonth - mean(data$TransfersInMonth)) / sd(data$TransfersInMonth)
future_data$BusinessDaysInMonth = (future_data$BusinessDaysInMonth - mean(data$BusinessDaysInMonth)) / sd(data$BusinessDaysInMonth)

#linear regression
model = lm(IssuesInMonth~TransfersInMonth+BusinessDaysInMonth,data = train)
summary(model)

#error calculation
predication_test = predict(model,test)
error = abs(sum((test$IssuesInMonth - predication_test)/test$IssuesInMonth) / 0.3)

#future prediction

predict=predict(model,future_data)
