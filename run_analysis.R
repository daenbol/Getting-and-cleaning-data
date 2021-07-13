library(tidyr)
library(tidyverse)

#1. Merging the training and the test sets to create one data set.

#1.0. Downloading and unzipping files
if(!file.exists('D:/SomeR/Coursera/Third_course_gacd/final_project')){dir.create('D:/SomeR/Coursera/Third_course_gacd/final_project')}
fileurl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(fileurl, destfile = 'D:/SomeR/Coursera/Third_course_gacd/final_project/getdata_projectfiles_UCI_HAR_Dataset.zip')

unzip('D:/SomeR/Coursera/Third_course_gacd/final_project/getdata_projectfiles_UCI_HAR_Dataset.zip', exdir = 'D:/SomeR/Coursera/Third_course_gacd/final_project')
setwd('D:/SomeR/Coursera/Third_course_gacd/final_project/UCI HAR Dataset')

#1.1. Reading files

features_info <- readLines('features_info.txt')

#1.1.1 Reading trainings tables
train <- read.csv('train/X_train.txt', sep = '', header = FALSE)
train_y <- read.csv('train/y_train.txt', sep = '', header = FALSE)
train_subject <- read.csv('train/subject_train.txt', sep = '', header = FALSE)

bodyAccX_train <- read.table("./train/Inertial Signals/body_acc_x_train.txt",header=FALSE)
bodyAccY_train <- read.table("./train/Inertial Signals/body_acc_y_train.txt",header=FALSE)
bodyAccZ_train <- read.table("./train/Inertial Signals/body_acc_z_train.txt",header=FALSE)
bodyGyroX_train <- read.table("./train/Inertial Signals/body_gyro_x_train.txt",header=FALSE)
bodyGyroY_train <- read.table("./train/Inertial Signals/body_gyro_y_train.txt",header=FALSE)
bodyGyroZ_train <- read.table("./train/Inertial Signals/body_gyro_z_train.txt",header=FALSE)
totalAccX_train <- read.table("./train/Inertial Signals/total_acc_x_train.txt",header=FALSE)
totalAccY_train <- read.table("./train/Inertial Signals/total_acc_y_train.txt",header=FALSE)
totalAccZ_train <- read.table("./train/Inertial Signals/total_acc_z_train.txt",header=FALSE)

#1.1.2 Reading testing tables
test <- read.csv('test/X_test.txt', sep = '', header = FALSE)
test_y <- read.csv('test/y_test.txt', sep = '', header = FALSE)
test_subject <- read.csv('test/subject_test.txt', sep = '', header = FALSE)


bodyAccX_test <- read.table("./test/Inertial Signals/body_acc_x_test.txt",header=FALSE)
bodyAccY_test <- read.table("./test/Inertial Signals/body_acc_y_test.txt",header=FALSE)
bodyAccZ_test <- read.table("./test/Inertial Signals/body_acc_z_test.txt",header=FALSE)
bodyGyroX_test <- read.table("./test/Inertial Signals/body_gyro_x_test.txt",header=FALSE)
bodyGyroY_test <- read.table("./test/Inertial Signals/body_gyro_y_test.txt",header=FALSE)
bodyGyroZ_test <- read.table("./test/Inertial Signals/body_gyro_z_test.txt",header=FALSE)
totalAccX_test <- read.table("./test/Inertial Signals/total_acc_x_test.txt",header=FALSE)
totalAccY_test <- read.table("./test/Inertial Signals/total_acc_y_test.txt",header=FALSE)
totalAccZ_test <- read.table("./test/Inertial Signals/total_acc_z_test.txt",header=FALSE)

#1.1.3 Reading feature vector
features <- read.csv('features.txt', sep = '\n', header = FALSE)

#1.1.4 Reading activity labels
activity <- read.csv('activity_labels.txt', sep = '\n', header = FALSE)
activity <- separate(activity, V1, c('number', 'vars'), sep = ' ')

#1.2 Assigning column names
var_names <- features %>% separate(V1, c('number', 'vars'), sep = ' ') %>% transmute(vars = vars) %>% list %>% unlist %>% as.list 
colnames(train) <- var_names
colnames(test) <- var_names

colnames(train_y) <- 'activities'
colnames(test_y) <- 'activities'

colnames(train_subject) <- 'subjects'
colnames(test_subject) <- 'subjects'

#1.3 Merging all data in one set
train <- cbind(train, train_y, train_subject)
test <- cbind(test, test_y, test_subject)

all_data <- rbind(train, test)

#2. Extracting only the measurements on the mean and standard deviation for each measurement
subset <- all_data %>% select(matches('std()|mean()'))

#3. Using descriptive activity names to name the activities in the data set

#There are some duplicated column names in a dataset (fBodyAcc-bandsEnergy()-1,8  -  fBodyAcc-bandsEnergy()-25,48). Idk the reason of that. 
#There are same duplicates even in features.txt, no explanation in README or features_info. However data in columns is not the same.

#Renaming duplicates
colnames(all_data) <- make.unique(colnames(all_data))

subset_desc <- all_data %>% mutate(activities = as.character(factor(activities, levels=1:6, labels= activity$vars)))

#4. Appropriately labeling the data set with descriptive variable names

#I've tried to make code DRY, unfortunately I didn't suceed

"""

gsub_m <- function(arg_1, arg_2, ...) {
  a <- grep(str_split(arg_1, '-')[[1]][1], features_info, value=TRUE)
  class(a)
  print(a)
  gsub('[A-Za-z]*.-', a, arg_1)
}

lapply(names(subset_desc), gsub_m)

"""

names(all_data) <- gsub("tBodyAcc-", "Body acceleration signal in time domain (from the accelerometer)", names(all_data))
names(all_data) <- gsub("tBodyAccMag-", "Body acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)", names(all_data))
names(all_data) <- gsub("tBodyAccJerk-", "Body acceleration jerk signal in time domain (from the accelerometer)", names(all_data))
names(all_data) <- gsub("tBodyAccJerkMag-", "Body acceleration jerk signal in time domain applied to Fast Fourrier Transform (from the accelerometer)", names(all_data))
names(all_data) <- gsub("tGravityAcc-", "Gravity acceleration signal in time domain (from the accelerometer)", names(all_data))
names(all_data) <- gsub("tGravityAccMag-", "Gravity acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)", names(all_data))
names(all_data) <- gsub("tBodyGyro-", "Body acceleration signal in time domain (from the gyroscope)", names(all_data))
names(all_data) <- gsub("tBodyGyroMag-", "Body acceleration signal in time domain applied to Fast Fourrier Transform(from the gyroscope)", names(all_data))
names(all_data) <- gsub("tBodyGyroJerk-", "Body acceleration jerk signal in time domain (from the gyroscope)", names(all_data))
names(all_data) <- gsub("tBodyGyroJerkMag-", "Body acceleration jerk signal in time domain applied to Fast Fourrier Transform(from the gyroscope)", names(all_data))
names(all_data) <- gsub("fBodyAcc-", "Body acceleration signal in frequence domain (from the accelerometer)", names(all_data))
names(all_data) <- gsub("fBodyAccMag-", "Body acceleration signal in frequence domain applied to Fast Fourier Transform(from the accelerometer)", names(all_data))
names(all_data) <- gsub("fBodyAccJerk-", "Body acceleration jerk signal in frequence domain (from the accelerometer)", names(all_data))
names(all_data) <- gsub("fBodyGyro-", "Body acceleration signal in frequence domain (from the gyroscope)", names(all_data))
names(all_data) <- gsub("fBodyAccJerkMag-", "Body acceleration jerk signal in frequence domain applied to Fast Fourrier Transform (from the accelerometer)", names(all_data))
names(all_data) <- gsub("fBodyGyroMag-", "Body acceleration signal in frequence domain applied to Fast Fourier Transform (from the gyroscope)", names(all_data))
names(all_data) <- gsub("mean()", "MEAN", names(all_data))
names(all_data) <- gsub("std()", "SD", names(all_data))
  
#5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject

#5.1 Making second tidy data set
tidy_data <- all_data %>% group_by(subjects, activities) %>% summarise_all(mean)

#5.2 Writing second tidy data set in txt file
write.table(tidy_data, "tidy_data.csv", row.name=FALSE, sep = ', ')


#gather(train, )
#stocks_m <- gather(train, "axes", "value", -ends_with('X|Y|Z'))

