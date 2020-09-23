getwd()
setwd("C:/docs")
data <- read.csv("election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 
ncol(data)
nrow(data)
#data = subset(data, select = -c("cand_id","last_name"))

drop <- c("cand_id", "last_name", "first_name", "twitterbirth", "facebookdate", "facebookjan", "youtubebirth")
data = data[,!(names(data) %in% drop)]

ncol(data)
nrow(data)

data$twitter<-as.factor(data$twitter)
data$facebook<-as.factor(data$facebook)
data$youtube<-as.factor(data$youtube)
data$cand_ici<-as.factor(data$cand_ici)
data$gen_election<-as.factor(data$gen_election)

data<-data[complete.cases(data), ] #remove data with missing values
ncol(data)
nrow(data)

set.seed(32) 
#data$salary.class <- as.factor(data$salary.class) # Make sure that the target (salary.class) is a factor vairable.
n = nrow(data) # n will be ther number of obs. in data
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) # We create an index for 70% of obs. by random
train_data = data[trainIndex,] # We use the index to create training data
test_data = data[-trainIndex,] # We take the remaining 30% as the testing data
summary(train_data)

#random forest classifier
install.packages("randomForest")
library(randomForest)
set.seed(32)
rf <-randomForest(gen_election~., data=train_data, ntree=90, na.action=na.exclude, importance=T, proximity=T) 
print(rf)

set.seed(32)
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=70,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, , na.action=na.exclude)
best.m <- mtry[mtry[,2]== min(mtry[,2]),1]
print(mtry)
print(best.m)

set.seed(32)
rf_n<-randomForest(gen_election~., data=train_data, mtry=best.m, importance=TRUE, ntree=70)
print(rf_n)

#Confusion Matrix
library(caret)
predicted_values <- predict(rf_n, test_data,type= "prob") # Use the classifier to make the predictions. With the package that we used, type "raw" will give us the probabilities 
head(predicted_values) # Let's look at the predictions (probabilities)
threshold <- 0.5 
pred <- factor( ifelse(predicted_values[,2] > threshold, "W","L")) # We ask R to use the threshold and convert the probabilities to class labels (zero and one)
head(pred) # Let's look at the predicted class labels


confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])


## ROC Curve
library(ROCR)
library(ggplot2)
pred_v <- predict(rf_n, test_data,type= "prob")
pred_n <- prediction(pred_v[,2], test_data$gen_election)
perf <- performance(pred_n, measure = "tpr", x.measure = "fpr")

auc <- performance(pred_n, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),tpr=unlist(perf@y.values ))
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +  ggtitle(paste0("ROC Curve vs AUC=", auc))
print(auc)

varImpPlot(rf_n)



## Question 10 ANN
install.packages("nnet")
library(nnet)
ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000) # Size is the number of hidden nodes.
ann
predicted_v <- predict(ann, test_data,type= "raw")
head(predicted_v)
pred1 <- factor( ifelse(predicted_v[,1] > threshold, "W", "L") )
head(pred1)
levels(test_data$gen_election)[2]
confusionMatrix(pred1, test_data$gen_election, positive = levels(test_data$gen_election)[2])

install.packages("ggplot2")

library(ROCR)
library(ggplot2)
predicted_v <- predict(ann, test_data,type= "raw")
pred1 <- prediction(predicted_v, test_data$gen_election)
perf <- performance(pred1, measure = "tpr", x.measure = "fpr")
auc <- performance(pred1, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve & AUC=", auc))


ann1 <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000)
ann1
predicted_values1 <- predict(ann1, test_data,type= "raw")
head(predicted_values1)
threshold1 <- 0.5 

library(ROCR)
library(ggplot2)
predicted_values1 <- predict(ann, test_data,type= "raw")
pred1 <- prediction(predicted_values1, test_data$gen_election)
perf <- performance(pred1, measure = "tpr", x.measure = "fpr")
auc <- performance(pred1, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve & AUC=", auc))



##GBM Classifier
library(caret)
install.packages("Metrics")
library(Metrics)
gbm <- train(as.factor(gen_election) ~ ., 
             data = train_data, method = "gbm", trControl = trainControl
             (method = "repeatedcv", number = 4, repeats = 4),verbose = FALSE)
summary(gbm)

predicted_valuesgbm <- predict(gbm, test_data,type= "prob")[,2] 
threshold <- 0.5
pred_gbm<-factor(ifelse(predicted_valuesgbm > threshold, 'W','L'))
pred_gbm
test_data$gen_election
confusionMatrix(pred_gbm, test_data$gen_election, 
                positive = 'W')
library(ROCR)
library(ggplot2)
predicted_values_gbm <- predict(gbm, test_data,type= "prob")[,2]
pred_gbm <- prediction(predicted_valuesgbm, test_data$gen_election)

perf <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")
auc <- performance(pred_gbm, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve & AUC=", auc))
