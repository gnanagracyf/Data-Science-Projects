##############################################################################
# The script assume that the follows files are in the same work directory:
# 
# ./ work directory
# ./features.txt
# ./test/subject_test.txt
# ./test/y_test.txt
# ./test/X_test.txt
# ./train/subject_train.txt
# ./train/y_train.txt
# ./train/X_train.txt
##############################################################################       

# Load dplyr library
library(dplyr)

# Load the test files (subject_test.txt, y_test.txt, X_test.txt) in
# auxiliary dataframes
Subject_Test <- read.table('./test/subject_test.txt')
Y_Test <- read.table('./test/y_test.txt')
X_Test <- read.table('./test/X_test.txt', stringsAsFactors = FALSE)

# Merge te auxiliary dataframe in a unique dataframe named Data_Test
Data_Test <- cbind(Subject_Test, Y_Test, X_Test)

# Remove the auxiliaries dataframes to free memory
remove(Subject_Test, Y_Test, X_Test)

        
# Load the train files (subject_train.txt, y_train.txt, X_train.txt) in
# auxiliary dataframes
Subject_Train <- read.table('./train/subject_train.txt')
Y_Train <- read.table('./train/y_train.txt')
X_Train <- read.table('./train/X_train.txt', stringsAsFactors = FALSE)

# Merge te auxiliary dataframe in a unique dataframe named Data_Test
Data_Train <- cbind(Subject_Train, Y_Train, X_Train)

# Remove the auxiliaries dataframes to free memory
remove(Subject_Train, Y_Train, X_Train)

# Merge dataframes Data_Test and Data_Train in a unique dataframe
# named Data_All
Data_All <- rbind(Data_Test, Data_Train)

# Remove dataframes  Data_Test and Data_Train to free memory
remove(Data_Test, Data_Train)

# Load the descriptive names of variables from the file features.txt
Features <- read.table('./features.txt')
# Make a list Names_Variables with de first and second name of the columns:
# Subject and Activities
Names_Variables <- c('Subject', 'Activities')
# Add to Names_Variables object the rest of the names variables that are in
# Features object
Names_Variables <- c(Names_Variables, as.vector(Features[,2]))
# Change the names of variables from Data_All to Names_variables
names(Data_All) <- Names_Variables

# Extract from dataframe Data_All the columns that I need: Subjetc, Acitivities,
# and all columns with the strings 'mean()' or 'std()' to a object named
# Correc_Columns
Corrects_Columns <- grep('Subject|Activities|mean()|std()', Names_Variables)
Data_Select <- Data_All[,Corrects_Columns]
# Remove Features, Names_Variables and Correct_Colums objets to free memory
remove(Features, Names_Variables, Corrects_Columns)
# Remove dataframe Data_All to free memory
remove(Data_All)

# Load the activities_labels file in a object
Activities <- read.table('./activity_labels.txt')
names(Activities) <- c('Activities', 'Activities_Names')

# Merge the Data_Select and Activities dataframe in one to change the activities
# number by descriptive activities names
Data_Select_Tidy <- merge(Activities, Data_Select, by='Activities')
# Remove the column Activities from the dataframe Data_Select_Tidy
Data_Select_Tidy <- Data_Select_Tidy[,-1]
# Remove dataframe Activities and Activities objetc to free memory
remove(Data_Select, Activities)

# At this point the data set in Data_Select_Tidy is complete Tidy
# Now group the dataframe Data_Select_Tidy by Activities_Names and Subject and
# calculate the mean or average of all variables
Data_Select_Tidy_Group <- Data_Select_Tidy %>%
        group_by(Activities_Names, Subject) %>%
        summarise_all(funs(mean))

# Remove dataframe Data_Select_Tidy to free memory
remove(Data_Select_Tidy)
write.table(Data_Select_Tidy_Group, file = 'Tidy_Data.txt', row.names=FALSE)
View(Data_Select_Tidy_Group)