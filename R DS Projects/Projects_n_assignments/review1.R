require(plyr)

# Set up directory and file
uci_dir <- "UCI HAR Dataset"
feature_file <- paste(uci_dir, "/features.txt", sep = "")
activity_labels_file <- paste(uci_dir, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_hard_dir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_hard_dir, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_hard_dir, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_hard_dir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_hard_dir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_hard_dir, "/test/subject_test.txt", sep = "")


# Load raw data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)


# 1. Merges the training and the test sets to create one data set.


training_signal_data <- cbind(cbind(x_train, subject_train), y_train)
test_signal_data <- cbind(cbind(x_test, subject_test), y_test)
signal_data <- rbind(training_signal_data, test_signal_data)

signal_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(signal_data) <- signal_labels


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

signal_data_mean_std <- signal_data[,grepl("mean|std|Subject|ActivityId", names(signal_data))]


# 3. Uses descriptive activity names to name the activities in the data set


signal_data_mean_std <- join(signal_data_mean_std, activity_labels, by = "ActivityId", match = "first")
signal_data_mean_std <- signal_data_mean_std[,-1]


# 4. Labels the data set with descriptive names.



names(signal_data_mean_std) <- gsub('\\(|\\)',"",names(signal_data_mean_std), perl = TRUE)

names(signal_data_mean_std) <- make.names(names(signal_data_mean_std))

names(signal_data_mean_std) <- gsub('Acc',"Accelerometer",names(signal_data_mean_std))
names(signal_data_mean_std) <- gsub('Gyro',"Gyroscope",names(signal_data_mean_std))
names(signal_data_mean_std) <- gsub('Mag',"Magnitude",names(signal_data_mean_std))
names(signal_data_mean_std) <- gsub('^t',"TimeDomain",names(signal_data_mean_std))
names(signal_data_mean_std) <- gsub('^f',"FrequencyDomain",names(signal_data_mean_std))
names(signal_data_mean_std) <- gsub('\\.std',"StandardDeviation",names(signal_data_mean_std))
names(signal_data_mean_std) <- gsub('Freq\\.',"Frequency",names(signal_data_mean_std))
names(signal_data_mean_std) <- gsub('Freq$',"Frequency",names(signal_data_mean_std))


# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

signal_avg_by_act_sub = ddply(signal_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(signal_avg_by_act_sub, file = "tidyData.txt")