library(readr)
library(dplyr)
library(tidyverse)
library(anytime)

# Loading data ------------------------------------------------------------
gender_submission <- read_csv("gender_submission.csv")
test <- read_csv("test.csv")
train <- read_csv("train.csv")

train <- train[,!colnames(train) %in% c("Name","Ticket","Cabin")]
test <- test[,!colnames(test) %in% c("Name","Ticket","Cabin")]
# Factor ------------------------------------------------------------------
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)
train$Sex <- factor(train$Sex)
train$Embarked <- factor(train$Embarked)

test$Survived <- factor(test$Survived)
test$Pclass <- factor(test$Pclass)
test$Sex <- factor(test$Sex)
test$Embarked <- factor(test$Embarked)

# # Cross validation --------------------------------------------------------
# sub <- sample(1:891, size = 446)
# train.train <- train[sub,]
# train.valid <- train[-sub,]
# 
# train.lg <- glm(Survived~. , data = train.train, family = "binomial")
# summary(train.lg)
# 
# #test the full model on the training set
# probs <- as.vector(predict(train.lg, type = "response"))
# preds <- rep(0,446)
# preds[probs>0.5] <- 1
# preds
# table(preds,train.train$Survived)
# # 52 + 191 = 243 wrong
# # preds   0   1
# # 0  91  52
# # 1 191 112


#Model based on AIC.
train.lg <- glm(Survived ~ . -PassengerId -Embarked -Fare -Parch, data = train, family = "binomial")
summary(train.lg)

#make predictions
probs <- as.vector(predict(train.lg, newdata = test, type ="response"))
preds <- rep(0,418)
preds[probs>0.5] <- 1


#export predictions 
final_result <- data_frame(test$PassengerId)
final_result["Survived"] <- preds
write.table(final_result, file = "submission.csv", row.names=F, col.names=c("PassengerId","Survived"), sep=",")


