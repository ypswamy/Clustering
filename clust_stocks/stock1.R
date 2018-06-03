library(class)
library(dplyr)
library(lubridate)
set.seed(100)

stocks <-read.csv('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/clust_stocks/stocks.csv')

#Let us put all data before the year 2014 into the training set, and #the rest into the #test set.

stocks$Date <- ymd(stocks$Date)
stocksTrain <- year(stocks$Date) < 2014

#Now, we need to build the training set. It will consist of the #prices of stocks of Apple, #Google, and Microsoft on the previous #day. For this, we can use the lag function in dplyr.

predictors <- cbind(lag(stocks$Apple, default = 210.73), lag(stocks$Google, default = 619.98), lag(stocks$MSFT, default = 30.48))

#Since for the very first value (corresponding to January 4, 2010), #the lag function has #nothing to compare it to, it will default to #NA. To avoid this, I set the default #values for each stock to be #its value on the previous business day (December 31, 2009).

#Now, let’s build the prediction model.

prediction <- knn(predictors[stocksTrain, ], predictors[!stocksTrain, ], stocks$Increase[stocksTrain], k = 1)

#We can see it’s accuracy using table.
table(prediction, stocks$Increase[!stocksTrain])

#and we can measure it’s accuracy as follows:
mean(prediction == stocks$Increase[!stocksTrain])

#This is only marginally better than random guessing (50%). Let’s see #if we can get a #better accuracy by changing the value of k. We can #use a for loop to see how the #algorithm performs for different #values of k.

accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  prediction <- knn(predictors[stocksTrain, ], predictors[!stocksTrain, ],
                    stocks$Increase[stocksTrain], k = x)
  accuracy[x] <- mean(prediction == stocks$Increase[!stocksTrain])
}

plot(k, accuracy, type = 'b')

#As we can see, the model has the highest accuracy of ~52.5% when k = #5. While this may #not seem any good, it is often extremely hard to #predict the price of stocks. Even the #2.5% improvement over random #guessing can make a difference given the amount of money #at stake. 