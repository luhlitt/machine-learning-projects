#importing libraries required for the analysis
library("ggplot2")                    
library("GGally")  
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library("e1071")


#Using regressrion tree
#read the csv file
diamond = read.csv("diamonds.csv", stringsAsFactors = TRUE)

#explorind and preparing the data
str(diamond)
hist(diamond$price)
diamond$price = log(diamond$price)
hist(diamond$price)

#checking for missing values
sapply(diamond,function(x) sum(is.na(x)))

#dropping the irrelevant column
diamond = diamond[-1]

#checking for outliers
boxplot(diamond$price, main="Boxplot of Diamond Prices", ylab="Price")

#checking for multicollineairty
round(cor(diamond[c("carat", "depth", "table", "x", "y", "z", "price")]), 2)
ggpairs(diamond[c("carat", "depth", "table", "x", "y", "z", "price")])

#removing columns with high correlation
diamond = diamond[c(-8, -9, -10)]

View(diamond)

#enconding categorical data
diamond$cut = factor(diamond$cut, 
                     levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
                     labels = c(1,2,3,4,5),
                     ordered = TRUE)
diamond$clarity = factor(diamond$clarity, 
                         levels = c("I1", "SI1", "SI2", "VS1", "VS2", "VVS1", "VVS2", "IF"),
                         labels = c(1,2,3,4,5,6,7,8),
                         ordered = TRUE)
diamond$color = factor(diamond$color, 
                       levels = c("J", "I", "H", "G", "F", "E", "D"),
                       labels = c(1,2,3,4,5,6,7),
                       ordered = TRUE)

diamond

#splitting the data into train set and test set
set.seed(70)
split = sample.split(diamond$price, SplitRatio = 0.8)
diamond_train = subset(diamond, split == TRUE)
diamond_test = subset(diamond, split == FALSE)

#fitting linear model to the training set
diamond_model <- lm(price ~ ., data = diamond_train)
summary(diamond01_model)
diamond_pred <- predict(diamond_model, diamond_test)

cor(diamond_pred, diamond_test$price)
MAE(diamond_pred, diamond_test$price)
RMSE(diamond_pred, diamond_test$price)

plot(diamond_pred, diamond_test$price)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

#using support vector regression model
regressor = svm(price ~., data = diamond_train, type = "eps-regression", kernel = "linear")
dia_pred <- predict(regressor, diamond_test)
summary(regressor)

MAE(dia_pred, diamond_test$price)
RMSE(dia_pred, diamond_test$price)
cor(dia_pred, diamond_test$price)

plot(dia_pred, diamond_test$price)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)




