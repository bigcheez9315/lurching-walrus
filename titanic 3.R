library(fields)
library(caret)
library(ggplot2)
library(rpart)
library(lattice)
library(randomForest)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
setwd("~/Documents/Data Science/Kaggle DataSets")
train <- read.table("train.csv", sep = ",", header = TRUE)
test <- read.table("test.csv", sep = ",", header = TRUE)

#We will make a variable that makes a tree out of all of the different variables
#That can affect Survival rating
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")


Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#Better version of fit
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))

#This will make a nice visual plot of the decision tree
fancyRpartPlot(fit)

#This model didn't end up being good, so I didn't submit it

