library(caret)
library(ggplot2)
library(rpart)
library(rattle)
library(randomForest)

training <- read.csv('/Users/daniel/Desktop/Practical Machine Learning Course Project/training data.csv')
testing <- read.csv('/Users/daniel/Desktop/Practical Machine Learning Course Project/test data.csv')

#Clean Data
##Keep the columns that have no NAs and remove the rest
training <- training[,colSums(is.na(training))==0]
testing <- testing[,colSums(is.na(training))==0]
##Remove the first seven columns, as they just are for identification purposes
training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]

##Remove variables that have a majority of values near zero
nz <- nearZeroVar(training)
training <- training[, -nz]
testing <- testing[, -nz]


#training
#qplot(user_name,roll_belt,color=classe,data=training)

#Split the data into training and test parts
trainingset <- createDataPartition(training$classe, p=.7, list = FALSE)
train <- training[trainingset, ]
test <- training[-trainingset, ]

##Checking number of rows for correct partitioning, and number of columns for same column count
c(nrow(train), ncol(train))
c(nrow(test), ncol(test))


#Decision Tree
set.seed(15000)
modFit1 <- train(classe~., data=train, method="rpart")
print(modFit1$finalModel)
##Our Dendogram of our train data
fancyRpartPlot(modFit1$finalModel)

##Comparing results to the test data for Decision Tree
predict1 <- predict(modFit1, newdata=test)
confusionMatrix(predict, as.factor(test$classe))
##Results show the Accuracy as only .49 which is low. Recommend a different algorithm


#Random Forests
#set.seed(15000)
#modFit2 <- train(classe~., data=train, method="rf", prox=TRUE)
##Comparing results to the test data for Random Forests
#predict2 <- predict(modFit2, newdata=test)
#confusionMatrix(predict2, as.factor(test$cclasse))


#Boosting
set.seed(15000)
modFit3 <- train(classe~., data=train, method="gbm")
##Comparing results to the test data for Boosting
predict3 <- predict(modFit3, newdata=test)
confusionMatrix(predict3, as.factor(test$classe))
##Results show the Accuracy is much higher at .96, will use this algorithm for our testing dataset.


#Results
##From our two models, we can see that Boosting has much higher accuracy than Random Forests. The results are:
#Boosting has .963 accuracy and .037 out of sample
#Decision Tree has .49 accuracy and .51 out of sample

#Prediction
predict(modFit3, newdata=testing)

plot(modFit1)
