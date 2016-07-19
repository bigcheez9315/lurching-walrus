library(fields)
library(caret)
library(ggplot2)
library(lattice)
library(randomForest)
setwd("~/Documents/Data Science/Kaggle DataSets")
trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)

#Use a crosstab to see how the categorical variable, Pclass affected chance of surviving
print(table(trainSet[,c("Survived", "Pclass")]))

#Good to use boxplots to to see how numerical data affected rate of survival (age in this example)
 bplot.xy(trainSet$Survived, trainSet$Age)

#Age doesn't seem to affect survival, and there are a lot of NA's in age as we can see from summary
summary(trainSet$age)

# Comparing Survival Rate and Fare
bplot.xy(trainSet$Survived, trainSet$Fare)

# Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
# Set a random seed (so you will get the same results as me)
set.seed(42)
# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = trainSet, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)

#Check for NA's in testSet
summary(testSet)

#There are NA's in Fare so use ifelse statement to replace NA values with the mean fare
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)



testSet$Survived <- predict(model, newdata = testSet)

#Remove unneccesary columns from testSet. Only want PassengerID and whether or not they survived
submission <- testSet[,c("PassengerId", "Survived")]

#Turn submission into its own csv file
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")

#Check the proportion of male and female that survived: row-wise proportion
print(prop.table(table(trainSet$Sex, trainSet$Survived), 1))

#Check the proportion of male and female that survived: column-wise proportion
print(prop.table(table(trainSet$Sex, trainSet$Survived), 2))

#Lets add make the survived column of the testSet = 0 for males and 1 for females because we know that nearly 75% of females survived
testSet$Survived <- 0
testSet$Survived[testSet$Sex == 'female'] <- 1

#Create new submission, called submission2 that tests purely based on gender


submission2 <- testSet[,c("PassengerId", "Survived")]

#Save submission2 as a .csv file then submit to kaggle

write.table(submission2, file = "submission2.csv", col.names = TRUE, row.names = FALSE, sep = ",")


