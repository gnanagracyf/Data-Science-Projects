library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]



#Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color by each of the variables in the data set 
#(you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). What do you notice in these plots?
library(Hmisc)        
qplot(training$CompressiveStrength ,pch=19,color=cutage , data= training)

cutcement <- cut2(training$Cement)
table(cutcement)

cutBlastFurnaceSlag <- cut2(training$BlastFurnaceSlag)
table(cutBlastFurnaceSlag)

plot(training$CompressiveStrength, color=cutage, data=training)

cutFlyAsh <- cut2(training$FlyAsh)
table(cutFlyAsh)

cutage <- cut2(training$Age)
table(cutage)


#Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(log(training$Superplasticizer))