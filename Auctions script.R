#Predicting end-price for online auction using regression trees
#importing the libraries
library(dplyr)
library(stats)
View(auctions)
#importing the dataset
data <- read.csv("auction.csv")
print(data)
str(data)

#data preprocessing
sapply(data,function(x) sum(is.na(x)))
data <- data[c(-4, -8, -9)]

glimpse(data)

#aggregrating the data and grouping by auction id
#creating avgbid, avgbidrate, avgtime, totalnum, openbid, and closebid per auction id.
data <- data %>%
  group_by(auctionid) %>%
  summarise(totalnum = n(), avgbid = mean(bid), avgtime = mean(bidtime), openbid = max(openbid), closebid = max(price))

data

data$avgbidrate <- (data$avgbid / data$avgtime) * data$totalnum
data$avgbidrate <- round(data$avgbidrate, 1)
data$avgbid <- round(data$avgbid, 1)

data <- data[-1]

#tranforming the dependent variable using log
hist(data$closebid)
data$logclosebid <- log(data$closebid)
data$logclosebid <- round(data$logclosebid, 1)
hist(data$logclosebid)
data
data <- data[-5]

#using caTools to split into train and test subsets

library(caTools)
set.seed(90)
data_subset = sample.split(data, SplitRatio = 0.75, group=NULL)
data_train = subset(data, data_subset==TRUE)
data_test = subset(data, data_subset==FALSE)

View(data_train)
#building the decision tree model
library(rpart)
m.rpart <- rpart(logclosebid ~ ., data = data_train)
m.rpart

#viusalizing the decision tree
install.packages("rpart.plot", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)

#making predictions
p.rpart <- predict(m.rpart, data_test)

#evaluating the model
summary(p.rpart)
summary(data$logclosebid)
cor(p.rpart, data_test$logclosebid)
MAE <- function(actual, predicted) {mean(abs(actual-predicted))}
MAE(p.rpart, data_test$logclosebid)
mean(data_train$logclosebid)
MAE(5.48, data_test$logclosebid)

rmse <- function(actual, predicted) {sqrt(mean((actual-predicted)^2))}
rmse(p.rpart, data_test$logclosebid)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home/jre/lib/server/libjvm.dylib')

#improving model performance
install.packages("RWeka")
library(RWeka)
m.m5p <- M5P(logclosebid ~ ., data = data_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, data_test)
summary(p.m5p)
cor(p.m5p, data_test$logclosebid)
MAE(data_test$logclosebid, p.m5p)
rmse(data_test$logclosebid, p.m5p)

#Predicting the end-price for online auction using linear regression
View(data)
datalinear_model <- lm(logclosebid ~ ., data = data_train)
View(datalinear_model)

summary(datalinear_model)

datalinear_pred <- predict(datalinear_model, data_test)

#evaluating the model
MAE(data_test$logclosebid, datalinear_pred)
rmse(data_test$logclosebid, datalinear_pred)

cor(data_test$logclosebid, datalinear_pred)

#using neural network
#data2_norm <- data %>% mutate_each_(list(~scale(.) %>% as.vector), vars = c("totalnum", "avgbid", "avgtime", "openbid", "avgbidrate"))
#normalize the data
normalize <- function(x) {return((x- min(x)) / (max(x) - min(x)))}
data_norm <- as.data.frame(lapply(data, normalize))
data_norm = data_norm[-1]

#using caTools to split into train and test subsets
set.seed(90)
nndata_subset <- sample.split(data_norm, SplitRatio = 0.75, group=NULL)
nndata_train = subset(data_norm, nndata_subset==TRUE)
nndata_test = subset(data_norm, nndata_subset==FALSE)

#building the neural network model
#install.packages("neuralnet")
#library(neuralnet)
#nnmodel <- neuralnet(logclosebid ~ ., data = nndata_train)
#plot(nnmodel)
#View(nndata_train)
#evaluating model performance
#model_results <- compute(nnmodel, nndata_test)
#predicted_closebid <- model_results$net.result
#cor(predicted_closebid, nndata_test$logclosebid)
#MAE(nndata_test$logclosebid, predicted_closebid)
#rmse(nndata_test$logclosebid, predicted_closebid)


#using neural network without normalization
#set.seed(90)
#nn_subset = sample.split(data, SplitRatio = 0.75, group=NULL)
#nn_train = subset(data, data_subset==TRUE)
#nn_test = subset(data, data_subset==FALSE)
#nn <- neuralnet(logclosebid ~ ., data = nn_train)
#plot(nn)
#View(nn_train)
#evaluating model performance
#nn_results <- compute(nn, nn_test)
#predicted_nnclosebid <- nn_results$net.result
#cor(predicted_nnclosebid, nn_test$logclosebid)
#MAE(nn_test$logclosebid, predicted_nnclosebid)
#rmse(nn_test$logclosebid, predicted_nnclosebid)

library(randomForest)
set.seed(1)
rf.auct = randomForest(logclosebid ~ ., data = data_train)
rf.auct

yhat.rf = predict(rf.auct, data = data_test)
yhat.rf
mean((yhat.rf - data_test$logclosebid)*2)

MAE(yhat.rf, data_test$logclosebid)
rmse(yhat.rf, data_test$logclosebid)


#using svm regression model
reg = svm(logclosebid ~., data = data_train, type = "eps-regression")
auct_pred <- predict(reg, data_test)

MAE(auct_pred, data_test$logclosebid)
rmse(auct_pred, diamond_test$logclosebid)

View(data)
