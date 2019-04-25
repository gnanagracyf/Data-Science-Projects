library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess() function from the caret package. 
#Calculate the number of principal components needed to capture 90% of the variance. How many are there?

training_pred <- training[,c("IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8")]

prepro <- preProcess(training_pred,method="pca")
testPC <- predict(prepro,training)
confusionMatrix(testPC)

#q5
#Create a training data set consisting of only the predictors with variable names
#beginning with IL and the diagnosis. Build two predictive models, one using the predictors as 
#they are and one using PCA with principal components explaining 80% of the variance in the predictors.
#Use method="glm" in the train function.
#What is the accuracy of each method in the test set? Which is more accurate?
        
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]training = adData[ inTrain,]
testing = adData[-inTrain,]
training_pred <- training[,c("diagnosis","IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8")]

prepro <- preProcess(training_pred,method="pca",pcaComp = 8)
trainPC <- predict(prepro,training_pred[,-1])
modelFit <- train(training_pred$diagnosis ~ ., method="glm",data=trainPC)


modelFit1 <- train(diagnosis ~ .,data=training_pred, method="glm")


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]training = adData[ inTrain,]
testing = adData[-inTrain,]
training_pred <- training[,c("IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6","IL_6_Receptor","IL_7")]
prepro <- preProcess(training_pred,method="pca")
