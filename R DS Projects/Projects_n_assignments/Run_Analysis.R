
library(data.table)
library(plyr)
library(dplyr)

#Download the Zip file and unzip its contents 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(fileUrl,temp)
unzip(temp)

# Read into R the required text files as data frames
activity_labels <- fread(".\\UCI HAR Dataset\\activity_labels.txt")
features <- fread(".\\UCI HAR Dataset\\features.txt")
test_x <- fread(".\\UCI HAR Dataset\\test\\X_test.txt")
test_y <- fread(".\\UCI HAR Dataset\\test\\y_test.txt")
train_x <- fread(".\\UCI HAR Dataset\\train\\X_train.txt")
train_y <- fread(".\\UCI HAR Dataset\\train\\y_train.txt")
train_subject <- fread(".\\UCI HAR Dataset\\train\\subject_train.txt")
test_subject <- fread(".\\UCI HAR Dataset\\test\\subject_test.txt")

#Rename the columns to appropriate variable names
colnames(test_x) <- c(features$V2)
colnames(train_x) <- c(features$V2)

# Combine the Train and the Test set
train_df <- cbind(train_subject,train_x)
test_df <- cbind(test_subject,test_x)

# Correct the variable names
colnames(train_df)[1] <- "subject"
colnames(test_df)[1] <- "subject"

# Add the activity data and rename the column 
train_dff <- cbind(train_y,train_df)
test_dff <- cbind(test_y,test_df)
colnames(train_dff)[1] <- "activity"
colnames(test_dff)[1] <- "activity"


# Resulting Data Frame 
final_df <- rbind(train_dff,test_dff)

#Select the variable names with mean and std value only
selected_col_vec <- c("activity","subject",grep("mean|std",names(final_df),value=TRUE,ignore.case = TRUE))
extracted_df <- final_df[,selected_col_vec,with=FALSE]

# Provide descriptive activity names
extracted_df$activity <- mapvalues(extracted_df$activity, from=c("1", "2", "3","4","5","6"), to=activity_labels$V2)

# Provide descriptive variable names
colnames(extracted_df) <- gsub("-","",colnames(extracted_df))
colnames(extracted_df) <- gsub("\\(\\)","",colnames(extracted_df))

#Groupby subject and activity and print the mean of each variable 

result <- extracted_df %>% group_by(subject,activity) %>% summarise_each(funs(mean))
View(result)

write.table(result, file = "tidy.txt", row.names = FALSE)
