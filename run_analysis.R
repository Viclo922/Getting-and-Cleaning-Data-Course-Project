#Preparation
#loading required packages.
library(dplyr) 
library(data.table)
#Download the Data
filename <- "Coursera_DS3_Final.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  
  if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
  }

# Reading the Data and name the colomn
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


# 1. Merge the training and test sets.
M<- rbind(x_train, x_test)
N<- rbind(y_train, y_test)
Subject <- rbind(sub_train, sub_test)
Merged_Data <- cbind(Subject, N, M)
#2. Extracting only the measurements on the mean and standard deviation for each measurement
#Read column names
MeanStd_Data <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
# 3. Use descriptive activity names to name the activities in the dataset.
MeanStd_Data$code <- activities[MeanStd_Data$code, 2]

# 4. Label the data set with descriptive variable names

#Acc is replaced by Accelerometer
#Gyro is replaced by Gyroscope
#Mag is replaced by Magnitude
#BodyBody is replaced by Body
#^t is replaced by Time
#^f is replaced by Frequency
#tBody is replaced by TimeBody
names(MeanStd_Data)[2] = "activity"
names(MeanStd_Data)<-gsub("Acc", "Accelerometer", names(MeanStd_Data))
names(MeanStd_Data)<-gsub("Gyro", "Gyroscope", names(MeanStd_Data))
names(MeanStd_Data)<-gsub("BodyBody", "Body", names(MeanStd_Data))
names(MeanStd_Data)<-gsub("Mag", "Magnitude", names(MeanStd_Data))
names(MeanStd_Data)<-gsub("^t", "Time", names(MeanStd_Data))
names(MeanStd_Data)<-gsub("^f", "Frequency", names(MeanStd_Data))
names(MeanStd_Data)<-gsub("tBody", "TimeBody", names(MeanStd_Data))
names(MeanStd_Data)<-gsub("-mean()", "Mean", names(MeanStd_Data), ignore.case = TRUE)
names(MeanStd_Data)<-gsub("-std()", "STD", names(MeanStd_Data), ignore.case = TRUE)
names(MeanStd_Data)<-gsub("-freq()", "Frequency", names(MeanStd_Data), ignore.case = TRUE)
names(MeanStd_Data)<-gsub("angle", "Angle", names(MeanStd_Data))
# 5. Creating a second,  independent tidy data set with the average of each variable for each activity and subject
FinalData <- MeanStd_Data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)