setwd("~/Documents/Data Science/Kaggle DataSets")
trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)


#Will Create new column that splits up the fare into different categories to see if amount paid affects survival rate
trainSet$Fare2 <- '30+'
trainSet$Fare2[trainSet$Fare < 30 & trainSet$Fare >= 20] <- '20-30'
trainSet$Fare2[trainSet$Fare < 20 & trainSet$Fare >= 10] <- '10-20'
trainSet$Fare2[trainSet$Fare < 10] <- '<10'

#make aggregate table to visualize results
print(aggregate(Survived ~ Fare2 + Pclass + Sex, data=trainSet, FUN=function(x) {sum(x)/length(x)}))

#Note that females in 3rd class who paid more than 20 dollars for their seats had a smaller survival rate. 
#Perhaps this is because the more expensive 3rd class tickets were closer to the iceberg

#Now let's create the survived column in the test set
#We will predict that all males + females in 3rd class who also spent more than 20 dollars did not survive

testSet$Survived <- 0
testSet$Survived[testSet$Sex == 'female'] <- 1
testSet$Survived[testSet$Sex == 'female' & testSet$Pclass == 3 & testSet$Fare >= 20] <- 0

submission3 <- testSet[ ,c("PassengerId", "Survived")]
write.table(submission3, file = "submission3.csv", col.names = TRUE, row.names = FALSE, sep = ",")





