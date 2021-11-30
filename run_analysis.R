library(data.table)
library(dplyr)

## Set working directory for source files
setwd("C:/Users/ckblu/Desktop/Coursera/Data Science/datasciencecoursera")
setwd("./Getting and Cleaning Data Assignment")

## Read training files
features_train <- read.table("X_train.txt")
activity_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

## Read test files
features_test <- read.table("X_test.txt")
activity_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

## Read Activity Labels and Features names
activity_labels <- read.table("./activity_labels.txt")
features_names <- read.table("./features.txt")

## Merge training and test files 
features_merged <- rbind(features_train, features_test)
activity_merged <- rbind(activity_train, activity_test)
subject_merged <- rbind(subject_train, subject_test)

## Setting column names 
colnames(features_merged) <- t(features_names[2])
colnames(activity_merged) <- 'Activity'
colnames(subject_merged) <- 'Subject'

## Create one large dataset containing Subject, Activity and Features data
dataset <- cbind(subject_merged, activity_merged, features_merged)

## Extract data containing mean and standard deviation measurements
columns_meanstd <- grep("mean\\(\\)|std\\(\\)", names(dataset))
columns_needed <- c(1,2,columns_meanstd)
extracted_dataset <- dataset[,columns_needed]

## Name activities according to descriptive activity names
extracted_dataset$Activity <- factor(extracted_dataset$Activity, levels = activity_labels[, 1], labels = activity_labels[, 2])

## Label dataset with descriptive variable names
names(extracted_dataset)<-gsub("Acc", "Accelerometer", names(extracted_dataset))
names(extracted_dataset)<-gsub("Gyro", "Gyroscope", names(extracted_dataset))
names(extracted_dataset)<-gsub("BodyBody", "Body", names(extracted_dataset))
names(extracted_dataset)<-gsub("Mag", "Magnitude", names(extracted_dataset))
names(extracted_dataset)<-gsub("^t", "Time", names(extracted_dataset))
names(extracted_dataset)<-gsub("^f", "Frequency", names(extracted_dataset))
names(extracted_dataset)<-gsub("tBody", "TimeBody", names(extracted_dataset))

## From the dataset above, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summarized_dataset <- extracted_dataset %>% 
  group_by(Subject, Activity) %>%
  summarize(across(everything(), mean))
  
# output to file "tidy_data.txt"
write.table(summarized_dataset, "tidy_data.txt", row.names = FALSE, quote = FALSE)




