#Loading caret package
library("caret")

#1. Loading training and test data
train <- read.csv(file="../input/train.csv", header=TRUE, sep=",",stringsAsFactors = F)
test <- read.csv(file="../input/test.csv", header=TRUE, sep=",",stringsAsFactors = F)

#Looking at the structure loaded data.
str(train)
str(test)

#combine together test and train for pre processing and preparing data
test$Survived <- 0

finalData <- rbind(train,test)
str(finalData)
#n this problem, we have to predict the Survival Status of a person based on his/ her details

#2. Pre-processing using Caret
#We need to pre-process our data before we can use it for modeling. Let’s check if the data has any missing values:
sum(is.na(finalData))

#There are 264 missing values. 

#Imputing missing values 

library(Hmisc)
#Take the count of family members...large family less survival rate

finalData$Familycount <- finalData$SibSp + finalData$Parch + 1 # total of siblings n spouse n parent n children travelling with them plus themselves


library(dplyr)
finalData <- finalData %>% 
        mutate(tempcol = strsplit(Name, "[,.]")) %>% 
        rowwise() %>% 
        mutate(Surname = unlist(tempcol)[1], Title = unlist(tempcol)[2], Firstname = unlist(tempcol)[3], Maidenname = unlist(ifelse(Title == " Mrs",tail(strsplit(strsplit(tempcol[3],"[\\(\\)]")[[1]][2]," ")[[1]],1) ,"U")))  %>%
        select(-c(Name,tempcol))

#second with surname and family count create familyid..no family for single travellers
finalData$Familyid <- ifelse( finalData$Familycount == 1,"single",paste(as.character(finalData$Familycount), finalData$Surname, sep=""))

unique(finalData$Title)

finalData$Title[finalData$Title %in% c( ' Mlle', ' Ms')] <- ' Miss'
finalData$Title[finalData$Title %in% c(' Capt', ' Don', ' Major', ' Sir',' Col')] <- ' Sir'
finalData$Title[finalData$Title %in% c(' Dona', ' Lady', ' the Countess', ' Jonkheer', ' Mme')] <- ' Lady'

#argImpute() automatically identifies the variable type and treats them accordingly.
finalData$Title <- as.factor(finalData$Title)

impute_arg <- aregImpute(~  Age + Sex + Pclass + SibSp + Parch + Fare + Title,
                         data = finalData, n.impute = 10, nk = 0 )

# Get the imputed values
imputed <-impute.transcan(impute_arg, data=finalData, imputation = 5, list.out=TRUE, pr=FALSE, check=FALSE)

# convert the list to the database
imputed.data <- as.data.frame(do.call(cbind,imputed))

# arrange the columns accordingly
#as of now consider age alone...and fare can be imputed manually as it is missing only a value for 674 passenger_id
finalData <- cbind(finalData,imputed.data$Age)
finalData$Age <- NULL
names(finalData)[names(finalData) == 'imputed.data$Age'] <- 'Age'
finalData$Age <- as.numeric(finalData$Age)
finalData$Fare[1044] <- imputed.data$Fare[1044]

# Impute Embark column
# two embarked value is empty..they both r ladies with same tickt id ..their pclass = 1 and age = 38 & 62 and not related..might be friends
# same cabin..and both survivied..cabin B cabin...with these find Embarked place..
finalData[finalData$Embarked == "", ]

prop.table(table(finalData$Embarked,finalData$Survived),1)


#Wiith the above info check whole data base#
table(finalData$Embarked[finalData$Pclass == 1 & finalData$Sex == "female" & finalData$Age >= 38 & finalData$Age <= 62 & finalData$Survived == 1])
# mostly from Embarked = "S satisfies this ...therefore fill this missing wiht "S
finalData$Embarked[62] <- "S"
finalData$Embarked[830] <- "S"

str(finalData)

#Extract cabin level info..if unknow mark 'U'
finalData$Cabin <- sub("^$", "U" , finalData$Cabin)
finalData <- finalData %>% rowwise() %>% mutate(CabinLevel = substring(Cabin,1,1))


#create ticket category

finalData <- finalData %>% rowwise() %>% mutate(Ticketcategory = strsplit(Ticket, " ")[[1]][1])
finalData$Ticketcategory <- gsub("^\\d*$","XX",finalData$Ticketcategory)

# create family category
# if family size = 2 & 3  then small family  else big family
finalData <- finalData %>% rowwise() %>% mutate(Familycategory = ifelse(Familycount %in% c(2,3), "small" , ifelse(Familycount %in% c(1), "single", "big")))
#check people with similar familyid and family count matches and their age,sex, cabin, 
#Groupby familyid can be used to fill in missing values, coz people in same family tend to be in same cabin..embark at same place..etc..

df <- finalData %>% group_by(Surname) %>% summarise(n())

#Categoriese age
finalData <- finalData %>% mutate(Agecategory = ifelse(Age < 18, "kid",ifelse(Age>60, "old", "adult")))

#SAVE FAMIY By finding family members latter
#There are 9 Andersson but only 7 family members..so two are other Andersson..change family id . Later we can finalDatane them with their family
#How come they also have come with 7 family members each...find it
finalData$Familyid[finalData$Surname == "Andersson" & finalData$Familycount == 7 & finalData$Ticket == "3101281"] <- "Andersson1"
finalData$Familyid[finalData$Surname == "Andersson" & finalData$Familycount == 7 & finalData$Ticket == "347091"] <- "Andersson2"

#Put all Richards together..they have diff familyid, same ticketid so same family..find other two siblings of Mrs. Richards ( 438 )
finalData$Familyid[finalData$Ticket == "29106" ]  <- "Richards" # might be hocking is sibiling of Richards..lil bit confusing..leaving it for now

#Find two siblings of Mr. Kink-Heilmann and give them the same family id
finalData$Familyid[finalData$Ticket == "315153" ] <- "Kink-Heilmann"

#Hocking family all confusion..two children missing for Mrs. Hocking..n find her sibling also
finalData$Familyid[finalData$Ticket == "29105" ] <- "Hocking"

#Find sibling of MR Vander Planke , united Mr and MRs...find his sibling
finalData$Familyid[finalData$Ticket == "345763" ] <- "Vander Planke"
finalData$Familyid[finalData$Ticket == "345764" ] <- "Vander Planke" # Same surname and Fare 

finalData$Familyid[finalData$Ticket == "31027" ] <- "Renouf"

#Unite Mr and Mrs
finalData$Familyid[finalData$Ticket == "243847" ] <- "Jacobsohn"


finalData$Familyid[finalData$Ticket =="F.C. 12750" ] <- "Davidson" # Find her 2 children ?? Mr. Davidson..no children..so correct the value

finalData$Familyid[finalData$Ticket =="3101278" ] <- "Backstrom" # Find 2 siblings of Mrs Bakstrom

finalData$Familyid[finalData$Ticket =="345763" ] <-  "Vander Planke" # Some error with other Vander Planke..find out correct count of Vander Planke family members

finalData$Familyid[finalData$Ticket =="2625" ] <- "Thomas" # Find her sibling of Mrs. Thomas..she didnt come with spouse

finalData$Familyid[finalData$Ticket =="347054" ] <- "Strom" #Find sibling 

finalData$Familyid[finalData$Ticket =="C.A. 33112" & finalData$Surname == "Davies" ] <- "Davies" # might be find parent of Mrs.. or correct Parch count
finalData$Familyid[finalData$Ticket =="A/4 48871" & finalData$Surname == "Davies" & finalData$Ticket =="A/4 48873" ] <- "Davies1"

finalData$Familyid[finalData$Surname =="Brown" & finalData$Ticket == "29750" ] <- "3Brown1" # Find sibling of 1248 another brown not this family

finalData$Familyid[finalData$Ticket =="11769"] <- "3Appleton" # find one sibling

#Wilkes sibling missing...893 id
# p id 19 Mrs. Vander Planke  sibling missing check with ticket id 345763

# Mrs stephenson sibling missing ticket 36947 pid 592

# Uniting mother n child coz mothers surname and daughters maiden name same & same ticket id
finalData$Familyid[finalData$Ticket =="230433"] <- "Parrish"

# Might be father and daughter
finalData$Familyid[finalData$Ticket =="13236"] <- "Mock"

finalData$Familyid[finalData$Ticket == "31027"] <- "2Renouf"

#Sisters unite now
finalData$Familyid[finalData$Ticket == "13502" & finalData$PassengerId == "276"] <- "Andrews"
finalData$Familyid[finalData$Ticket == "13502" & finalData$PassengerId == "766"] <- "Andrews"

finalData$Familyid[finalData$Ticket == "3101298"] <- "Hirvonen"

finalData$Familyid[finalData$Ticket == "PC 17572" & finalData$Surname == "Harper"] <- "Harper1"

#Find sibling of Miss. Eustis pid 497

finalData$Familyid[finalData$PassengerId == "311"] <- "Potters"
finalData$Familyid[finalData$PassengerId == "1042"] <- "Potters" # mother n daughter

finalData$Familyid[finalData$Ticket == "113505"] <- "Bowermans"


#Fill in Maidenname
# if no maidenname for Mrs..fill with U
finalData$Maidenname[finalData$PassengerId %in% c(20,257,707,798,911,1148)] <- "U"

finalData <- finalData[finalData$CabinLevel != "T", ]

# 3. Splitting Data Manually as it was before

#problem dont use factor for all num and int values..coz it gives more than 53 factore..cannot be used in rfe feature selection
#finalData[] <- lapply(finalData, factor)

finalData$Pclass <- ifelse(finalData$Pclass == 1, "One", ifelse(finalData$Pclass == 2,"Two","Three"))
finalData$Survived <- ifelse(finalData$Survived == 1, "Y","N")


finalData$Survived <- as.factor(finalData$Survived)
finalData$Pclass <- as.factor(finalData$Pclass)
finalData$Sex <- as.factor(finalData$Sex)
finalData$Embarked <- as.factor(finalData$Embarked)
finalData$Embarked <- as.factor(finalData$Embarked)
finalData$Title <- as.factor(finalData$Title)
finalData$CabinLevel <- as.factor(finalData$CabinLevel)
finalData$Ticketcategory <- as.factor(finalData$Ticketcategory)
finalData$Familycategory <- as.factor(finalData$Familycategory)
finalData$Agecategory <- as.factor(finalData$Agecategory)
finalData$Familyid <- as.factor(finalData$Familyid)
finalData$Familyid_num <- as.numeric(finalData$Familyid)

#Feature selection using rfe in caret

control <- rfeControl(functions = rfFuncs,method = "repeatedcv",number = 10)
predictors <- c("Pclass","Sex","SibSp","Parch","Fare","Embarked","Familycount","Title","Age","CabinLevel","Ticketcategory","Familycategory","Agecategory")
Survival_Pred_Profile <- rfe(finalData[ ,predictors], finalData$Survived, sizes = c(1:12),
                             rfeControl = control,metric = "Accuracy")
print(Survival_Pred_Profile) 
# list the chosen features
predictors(Survival_Pred_Profile)
# plot the results
plot(Survival_Pred_Profile, type=c("g", "o"))

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Survived ~ ., data = finalData, 
               method="rf", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#So going to use the top 10 features in the coming predictions
#Top 5 : Title, CabinLevel, Sex, Fare, Pclass
predictors <- c("Pclass","Sex","Parch","Embarked","Familycount","Title","Age","CabinLevel","Fare","Ticketcategory","Familyid_num")

selectedData <- finalData[,predictors]
selectedData <- cbind(selectedData,finalData$Survived)
names(selectedData)[12] <- "Survived"
names(finalData)[names(finalData) == 'finalData$Survived'] <- 'Survived'

#Splitting data
anyNA(selectedData)
trainSet <- selectedData[1:891,]
testSet <- selectedData[892:1309,]

#Stacking: In stacking multiple layers of machine learning models are placed one 
#over another where each of the models passes their predictions to the model in the 
#layer above it and the top layer model takes decisions based on the outputs of the models in layers below it.
#two important criteria that we previously discussed on individual model accuracy
#and inter-model prediction correlation which must be fulfilled. 
#If the predictions are highly correlated, then using these three might not give better results than individual models. 


##STEPS for STACKING
#1.Train the individual base layer models on training data.
#2.Predict using each base layer model for training data and test data.
#3.Now train the top layer model again on the predictions of the bottom layer models that has been made on the training data.
#4.Finally, predict using the top layer model with the predictions of bottom layer models that has been made for testing data.

# Stacking algorithms

# create submodels
library(caret)
library(caretEnsemble)
library(rpart)
fitControl <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions=TRUE,classProbs=TRUE)
#algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial','rf','gbm','lr','treebag','svmLinear','xgbTree','LogitBoost')
set.seed(1234)
#algorithmList <- c( 'rpart','glm','rf','gbm','treebag') #rf and glm high corr..so remove
algorithmList <- c( 'rpart','gbm','treebag','xgbTree','LogitBoost','naive_bayes')
models <- caretList(trainSet[,1:11],trainSet$Survived , trControl=fitControl, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# correlation between results
modelCor(results)
splom(results)

# stack using gbm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(1234)
stack.gbm <- caretStack(models, method="gbm", metric="Accuracy", trControl=stackControl)
print(stack.gbm)


stack1Pred <- predict(stack.gbm,testSet)

Pred <- ifelse(stack1Pred == "Y", 1,0)
