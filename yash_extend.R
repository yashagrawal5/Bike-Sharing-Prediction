#######    ****  PROJECT: BIKE SHARING PREDICTION MODE  **** #############
###   

###################


##    DAY FILE    ##

###################
### PACKAGES THAT WILL BE USED 

install.packages("stringr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("pander")

install.packages("relaimpo")
install.packages("RColorBrewer")

library(MASS)
library(PASWR2)
library(dplyr)
library(tidyverse)
install.packages("Dominance")
library(lubridate)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(forecast)
library(zoo)
library(tseries)
library(corrplot)
library(e1071)
library(pander)
library(ggplot2)
library(relaimpo)
library(RColorBrewer)
library(caret)

day_file= read.csv("day.csv",header = TRUE)
head(day_file)

##############################

###### CSV files summary #####
nrow(day_file)
ncol(day_file)
dim(day_file)
typeof(day_file)

##############################
null_check = sum(is.null(day_file))
null_check
# Scatter plot, Visualize the linear relationship between the predictor and response
scatter.smooth(x=day_file$temp, y=day_file$cnt, main="cnt ~ temperature")


summary(day_file)


names(day_file)
day_file$instant=NULL
day_file$dteday=NULL
day_file$casual=NULL
day_file$registered=NULL
day_file$mnth=NULL
day_file$atemp=NULL
day_file$windspeed=NULL
head(day_file)

###finding the missing value 
print(paste("The total number of missing data are",sum(is.na(day_file))))

##### To check the exploratory data analysis by normal distribution curve ###### 
## of the data 
## responsible variable  count is distributed 
## here it is normally distributed 
## median here is normally distributed 
## plotting tyhe frquency of rental with total bikes 

#day
h <- hist(day_file$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue' )

xfit <- seq(min(day_file$cnt),max(day_file$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(day_file$cnt),sd=sd(day_file$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(day_file$cnt)
lines(xfit,yfit, col='red', lwd= 3)
### plotting the box plot for the working day and holiday 
##the medium value is increased for working dayy 
## this means that maximun bike rides are over the working days


boxplot(day_file$cnt ~ day_file$holiday,
        data = day_file,
        main = "Total Bike Rentals Vs Holiday",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 
####################################################################################3
##################333
#############33
#===== Sampling and evaluation metric ===

trControl = trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 3)
#### Support Vector Machine ################################

metric = 'RMSE'
seed = 10
library(kernlab)
m1=fit.svm <- train(cnt~., data=day_file, method="svmRadial", metric=metric, trControl=trControl)
fit.svm

head(day_file)
typeof(m1)
print
print(fit.svm)

##### SVM Adjusted r2 is 63.4 % 

#============================== D. Ensembles ==============================

#======++++++     Random Forest  ++++++++======

library(randomForest)
set.seed(seed)
m2 <-train(cnt~season, data=day_file, method="rf", ntree = 20, metric=metric, trControl=trControl)
fit.rf <- train(cnt~season, data=day_file, method="rf", ntree = 20, metric=metric, trControl=trControl)
print(fit.rf)
print(m2)


#====== Bagged CART ======
set.seed(seed)
fit.treebag <- train(cnt~., data=day_file, method="treebag", metric=metric, trControl=trControl)
print(fit.treebag)


#====== K-Nearest Neighbour ======
set.seed(seed)
tunegrid <- expand.grid(.k=1:10)
fit.knn <- train(cnt~., data=day_file, method="knn", metric=metric, tuneGrid=tunegrid, trControl=trControl)
print(fit.knn)

results = summary(resamples(list (knn=fit.knn)))


#===== Stochastic Gradient =====
#=========== Boosting ==========
install.packages("gbm")
library(gbm)
set.seed(seed)
fit.gbm <- train(cnt~., data=day_file, method="gbm", metric=metric, trControl=trControl, verbose=FALSE)
summary(resamples(list(gbm=fit.gbm)))
print(fit.gbm)
fit.gbm
#####################################################################
####### time Series Model#######################################
day_file$windspeed=NULL
data_ts <-msts(day_file[,'cnt'], seasonal.periods=c(7))
train_ts <- head(data_ts, round(length(data_ts) * 0.9))
test_ts <- tail(data_ts, round(length(data_ts) * 0.1))
plot(train_ts, xlab="Weeks", ylab="Bike riders")
plot(decompose(train_ts, type='add'), xlab="Weeks")
adf_test <- adf.test(train_ts, alternative='stationary')
print(adf_test)
#Fitting an ARIMA model requires the series to be stationary. So, checked the stationarity of the series by used ADF test (Augmented Dickey-Fuller).
#The test shows that the p-value is .19, which indicates series is non-stationary.





#STANDARD RESIDUAL VS PREDICTED VALUES

residual=rstandard(fit.svm)
plot(fitted(myaic),residual,main="Predicted values V/s the Standard Residual")
abline(a=0,b=0,col='red')

#EXAMINE RESIDUALS ARE NORMAL OR NOT
r<-qqnorm(residual)
r
qqline(residual,col=2)

#USING SHAPIRO WILK NORMALITY TEST
shapiro.test(residual)
#Jarque-Bera
x <- rnorm(r)    # null hypothesis
jarque.bera.test(x)