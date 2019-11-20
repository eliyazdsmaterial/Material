### Premise & Problem Statement
# An online gaming portal wants to understand their customer patterns based on their 
# transactional behavior. For this, they have constructed a customer level data based 
# on the details they are tracking. The customer database consists of demographic and 
# transactional information for each customer. Building a regression model to predict 
# the customer revenue based on other factors.

#------------------------ READING DATA ------------------------------#
### Clear environment
rm(list=ls(all=TRUE))

# Set directory and read the data 
setwd("")

data<-read.csv("CustomerData.csv",header=T)

# Data exploration
str(data)
summary(data)
levels(data$FavoriteGame)

#---------------------- DATA PRE PROCESSING ------------------------------#

# split the data into train and test data sets
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
test = data[-trainRows,] 

# remove CustomerID column
train$CustomerID <- NULL #Remove ID columns as they won't be used in model building
test$CustomerID <- NULL

# convert City attribute as factor
train$City <- as.factor(train$City)
test$City <- as.factor(test$City)

###Imputation
sum(is.na(train))
sum(is.na(test))


# BUILD LINEAR REGRESSION MODEL 

# Model1- Build model with all attributes into model 
LinReg1<- lm(TotalRevenueGenerated ~ ., data=train)

#Model Summary Analysis
summary(LinReg1)

#Residual analysis
par(mfrow=c(2,2))
plot(LinReg1)
plot(LinReg1,which=4)

#hist(LinReg1$residuals)
#res are almost linear;no patterns in res; a few lev/outliers are observed

##Model Performance Evaluation
#Error verification on train data
library(DMwR)
regr.eval(train$TotalRevenueGenerated, LinReg1$fitted.values) 

#Error verification on test data
Pred<-predict(LinReg1,test)
regr.eval(test$TotalRevenueGenerated, Pred)


# Model2 - Build model with significant attributes
LinReg2<-lm(TotalRevenueGenerated~.-NoOfGamesPlayed,data=train)
summary(LinReg2)

LinReg3<-lm(TotalRevenueGenerated~.-NoOfGamesPlayed-MaxAgeOfChild,data=train)
summary(LinReg3)

# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg3$fitted.values) 
#Error verification on test data
Pred<-predict(LinReg3,test)
regr.eval(test$TotalRevenueGenerated, Pred)

####################################################
## Standardizing the Data
# We will use the Caret pakcage to standardize the data after the split using the __"preProcess()"__ function
#It saves the metrics such as mean and standard deviation used for calculating the standardized value by creating a model object
#We can then use the model object in the "predict()"  function to standardize any other unseen dataset with the same distribuiton and variables

library(caret)
# The "preProcess()" function creates a model object required for standardizing unseen data
# Do not standardize the target variable

train_nonstd = train
test_nonstd = test

independentattr<-setdiff(names(train),c("TotalRevenueGenerated"))
std_model <- preProcess(train[, independentattr], method = c("range"))
std_model
# The predict() function is used to standardize any other unseen data

train[, independentattr] <- predict(object = std_model, newdata = train[, independentattr])
test[, independentattr] <- predict(object = std_model, newdata = test[, independentattr])


# Model3- Build linear regression with all standardized attributes 
LinReg_std1<-lm(TotalRevenueGenerated~., data=train)
summary(LinReg_std1)

#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg_std1$fitted.values) 

#Error verification on test data
Pred<-predict(LinReg_std1,test)
regr.eval(test$TotalRevenueGenerated, Pred)
plot(LinReg_std1)

# Check for multicollinearity and perform dimensionality reduction analysis
library(corrplot)
cor(train[,-c(1,11,12)])


# 2. Stepwise Regression
#Model4 - Build Model based on Stepwise Regression
library(MASS)
Step1 <- stepAIC(LinReg_std1, direction="backward")
#Step2 <- stepAIC(LinReg1, direction="forward")
Step3 <- stepAIC(LinReg_std1, direction="both")
summary(Step3)
Mass_LinReg1 <- lm(TotalRevenueGenerated~City+MinAgeOfChild+FavoriteChannelOfTransaction
                   +FrquncyOfPurchase+FavoriteGame+NoOfGamesBought+MinAgeOfChild
                   +Tenure+FrequencyOFPlay+NoOfUnitsPurchased,data=train)

summary(Mass_LinReg1)
par(mfrow=c(2,2))
plot(Mass_LinReg1)
plot(Mass_LinReg1,which=4)
par(mfrow=c(1,1))
head(train)

#Model5 - Build Model based on VIF Analysis
# 1. VIF: (check attributes with high VIF value)
library(car)
vif(Mass_LinReg1)


###Removing FrquncyOfPurchase column
str(train_std)
LinReg_std2<-lm(TotalRevenueGenerated~City+MinAgeOfChild+FavoriteChannelOfTransaction
                +FavoriteGame+NoOfGamesBought+MinAgeOfChild
                +Tenure+FrequencyOFPlay+NoOfUnitsPurchased, data=train[,-c(6)])
summary(LinReg_std2)
vif(LinReg_std2)
#Error verification on test data
Pred<-predict(LinReg_std2,test)
regr.eval(test$TotalRevenueGenerated, Pred)
plot(LinReg_std2)


#Model6 - Build model without the influencial point (record #2729) 
which(rownames(train)%in%c(2729))

train[230,]
LinReg_No_infl<- lm(TotalRevenueGenerated ~ ., data=train[-230,])
summary(LinReg_No_infl)

#Another Way to remove Influential Variables
#cooks distance
cook = cooks.distance(Mass_LinReg1)
plot(cook,ylab="Cooks distances")
max=as.numeric(which.max(cook))
points(max,cook[max],col='red', pch=19)
train[max,]
train <- train[-max,]


#Error verification on train data
regr.eval(train$TotalRevenueGenerated, Mass_LinReg1$fitted.values) 
#Error verification on test data
MASS_Pred1<-predict(Mass_LinReg1,test)
regr.eval(test$TotalRevenueGenerated, MASS_Pred1)

Error_calc = data.frame(train$TotalRevenueGenerated,Mass_LinReg1$fitted.values)
write.csv(x = Error_calc,file = "Error_calc.csv")


#Model7 - Build model with Variable Transformations on Y and/or X variables
par(mfrow=c(4,2))

plot(TotalRevenueGenerated ~ ., data = data)


dataForModel <- data
#Y Variable Transformation
dataForModel$TotalRevenueGenerated <- log(dataForModel$TotalRevenueGenerated)
#X Variable Transformation
dataForModel$Tenure <- sqrt(dataForModel$Tenure)

# split the data into train and test data sets
rows=seq(1,nrow(dataForModel),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(dataForModel))/100)
train = dataForModel[trainRows,] 
test = dataForModel[-trainRows,] 

LinReg5<- lm(TotalRevenueGenerated ~ ., data=train)
summary(LinReg5)
par(mfrow=c(2,2))
plot(LinReg5)
plot(LinReg5,which=4)
hist(resid(LinReg5))

#Error verification on train data
library(DMwR)
regr.eval(exp(train$TotalRevenueGenerated), exp(LinReg5$fitted.values)) 
#Error verification on test data
Pred<-predict(LinReg5,test)
regr.eval(exp(test$TotalRevenueGenerated), exp(Pred))


#Future value prediction
temp <- test[14,]
temp$TotalRevenueGenerated <- NULL
predict(LinReg1,temp)
temp$NoOfChildren <- 1
temp$NoOfGamesBought <- 2
temp
predict(LinReg1,temp)
##






