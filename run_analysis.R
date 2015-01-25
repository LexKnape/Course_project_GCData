## The purpose of this project is to demonstrate your ability to collect, work with, 
## and clean a data set. The goal is to prepare tidy data that can be used for later 
## analysis. You will be graded by your peers on a series of yes/no questions related 
## to the project. You will be required to submit: 

## 1) a tidy data set as described below, 
## 2) a link to a Github repository with your script for performing the analysis,  
## 3) a code book that describes the variables, the data, and any 
## transformations or work that you performed to clean up the data called CodeBook.md. 
## 4) You should also include a README.md in the repo with your scripts. 

## This repo explains how all of the scripts work and how they are connected.  

## One of the most exciting areas in all of data science right now is wearable 
## computing - see for example this article. Companies like Fitbit, Nike, and 
## Jawbone Up are racing to develop the most advanced algorithms to attract new 
## users. The data linked to from the course website represent data collected 
## from the accelerometers from the Samsung Galaxy S smartphone. A full description 
## is available at the site where the data was obtained: 

## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Here are the data for the project: 

## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the 
##    average of each variable for each activity and each subject.

getwd()

## Install packages
install.packages("data.table")
install.packages("reshape2")
install.packages("dplyr")
install.packages("bitops")
install.packages("RCurl")

## libraries
library(data.table)
library(reshape2)
library(dplyr)
library(bitops)
library(RCurl)

## Search the site for downloading data and setting the destination file  
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "getdata-projectfiles-UCI-HAR-Dataset.zip"
## Downloading the fileURL define the separators which are "/" and using method Curl because of use of mac 
download.file(fileUrl, destfile=paste("Course_project", destfile, sep="/"), method="curl")
## Unzip the file to the created local folder Course_Project
unzip(paste("Course_project", destfile, sep="/"), exdir="Course_project")
data_dir <- setdiff(dir("Course_project"), destfile)

## examening the downloaded files to get an idea of its contents
## In relation to the design of the solution to go from raw to tidy data.

## 1. Merge the training and test sets to create one data set
## Get the test and training data set from it's respective files Course_project/train/X_train.txt
## and Course_project/test/X_test.txt.
X_train <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/train/X_train.txt", quote="\"")
X_test <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/test/X_test.txt", quote="\"")
## Check the number of test and training set before merging. 
merge <- rbind(X_train, X_test)
merge

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## As shown in the Readme file, the features.txt are the variables names and are obtained by 
## read.table function 
features <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/features.txt", quote="\"")
## by selecting the columns of the table we see an overview of the "feature columns. 
col <-(features[,2])
col

## The names of variables (feature) are added to the columns of the table merge
colnames(merge)<- features[,2]

## In order to only extract the variables with the mean() and the std() measurement I use 
## the pattern matching and replacement (grep) command. 
## MStd <- sort(c(grep("std",colnames(merge)),grep("mean[\\(\\)]",colnames(merge))))
MStd <- (c(grep("std",colnames(merge)),grep("mean[\\(\\)]",colnames(merge))))
MStd_order <-sort(MStd)
MStd_order

Subset_MStd <- merge [,MStd]
Subset_MStd

## 3. Uses descriptive activity names to name the activities in the data set
## First I add the subjects to the data set by reading the subject both for the train.txt and test.txt files
## I bind the rows with rbind and then bind the respective columns with cbind 
subject_train <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/train/subject_train.txt", quote="\"")
subject_test <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/test/subject_test.txt", quote="\"")
##  We bind the rows with rbind 
subject_merge <- rbind(subject_train,subject_test)
## We bind the respective columns with cbind 
Subset_MStd <- cbind(Subset_MStd,subject_merge)

## Finally I give the 67 column the name "subject" with colnames
colnames(Subset_MStd)[67]<-"subject"
## By checking the data I checked that it had the correct column name

## the activities are generated by the y_train.txt and the y-test.txt files
## I read the files through the read.table command
y_train <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/train/y_train.txt", quote="\"")
y_test <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/test/y_test.txt", quote="\"")
## the rows of the activities are binded together through rbind
y_TrainTest_Activity <- rbind(y_train,y_test)
## and column binded to the Subset_MStd
Subset_MStd <- cbind(Subset_MStd,y_TrainTest_Activity)

## reading the activity labels and merging with the Subset_MStd
activity_labels <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/activity_labels.txt", quote="\"")
Subset_MStd <- merge(Subset_MStd,activity_labels,by="V1")
colnames(Subset_MStd)[69]<-"activity"
colnames(Subset_MStd)[1]<-"activityID"

## 4. Appropriately labels the data set with descriptive variable names
## I used the gsub command that replaces the "content" of the first
## variable with the "content" second variable
colnames(Subset_MStd)<- gsub ("tBody","time body",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("tGravity","time gravity",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("fBody","frequency domain signal body",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("Acc"," accelerometer ",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("Gyro"," gyroscope ",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("Jerk"," jerk signal ",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("Mag"," magnitude ",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("mean\\(\\)","  Mean value ",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("std\\(\\)","  Standard deviation value ",colnames(Subset_MStd))
colnames(Subset_MStd)<- gsub ("Body","",colnames(Subset_MStd))

## 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable 
## for each activity and each subject.
## As I have created in exercise 3 both the subjects and activity through r and cbind we can now
## split the data set into a subset by aggregating both list of the column subject and activityand of the 
## data set Subset_MStd with the function mean.
Sec_Ind_tidy_data <- aggregate(Subset_MStd,list(Subset_MStd$subject,Subset_MStd$activity),FUN=mean,na.rm=TRUE)

## The tidy data set have correct labels therfor I give the first column the name "subject"
colnames(Sec_Ind_tidy_data)[1]<-"subject"
## and the second column the name "activity"
colnames(Sec_Ind_tidy_data)[2]<-"activity"

## The 70 and 71 column must be deleted as the subject and activity has been added as first and second column 
Sec_Ind_tidy_data <- Sec_Ind_tidy_data [,1:69]
## And finally write the data set to the local folder
write.table(Sec_Ind_tidy_data,file="/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/Sec_Ind_tidy_data.txt",row.name=FALSE)
