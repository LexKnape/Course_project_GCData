# Course_project_GCData
README

This readme file is part of the course project “Getting and cleaning data”. The readme file explains how all of the scripts of the run_analysis.R work and how they are connected. 

The hardware and software used 
Hardware: 	Macbook Pro, processor 2.4 GHz Intel Core i5, Memory 8 GB 1067 MHz 		DDr3 Software:	
Software:	OSX Yosemite 
		R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet" 
		R Studio Version 0.98.1091 – © 2009-2014 RStudio, Inc.
		R library data.table
		R library reshape2
		R library dplyr
 
The script has been run several times and generated the same output. As a double check a fellow student named Mischa Nanne ran the script on his hardware and software and got the same output.  

After reading the article about the wearable computing and gathering the necessary information I checked the working directory and created the file run_analysis. 

I installed the required packages  

install.packages("data.table")
install.packages("reshape2")
install.packages("dplyr")
install.packages("bitops")
install.packages("RCurl")

Opened the installed packages 
library(data.table)
library(reshape2)
library(dplyr)
library(bitops)
library(RCurl)

I than searched and downloaden the required file to it’s local folder “Coursera_project”,  define the “/” separators and using method Curl because of use of mac
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

After downloading I examened the downloaded files to get an idea of its contents in relation to the design of the solution to go from raw to tidy data.

Investigation of the download file, which txt.file generates which data set?
By investigating the Readme text files and each individual file and by going through some coursera threads I got an overview of which text file generates the required data set. I also found a image on the Coursera discussion forum which gave a clear picture which text file was used to create a tidy data set from the raw. 

Overview:
Data: X_train.txt and x_test.txt files
Subjects: subject_train.txt and subject_test.txt 
Activity: y_train.txt and y-test.txt (activity_labels.txt)
Variable names: features.txt

1. Merge the training and test sets to create one data set
I first create the test and train data sets from its respective text files. I did this by loading the text files in X_train en X_test. 

X_train <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/train/X_train.txt", quote="\"") 
Result data set: 7.352 obs. of 561 variables 
X_test <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/test/X_test.txt", quote="\"") 
Result data set: 2.947 obs. of 561 variables

After checking if the numbers of variables were them same for both X_test and X_Train (561 variables) I merged them with rbind
merge <- rbind(X_train, X_test)
Result data set: 10.299 obs. of 561 variables

2. Extracts only the measurements on the mean and standard deviation for each measurement. 
As shown above in the overview, the features.txt are the variables names and are obtained by the read.table function. 
features <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/features.txt", quote="\"")
In order to get an overview of all the “features” columns I select the columns of the feature table:
- col <-(features[,2])
- col 
Result: 477 Levels: angle(tBodyAccJerkMean),gravityMean) angle(tBodyAccMean,gravity) ... tGravityAccMag-std()

The names of variables (feature) are added to the columns of the table merge
- colnames(merge)<- features[,2]

In order to only extract the variables with the mean() and the std() measurement I use 
the pattern matching and replacement (grep) command. 

- MStd <- (c(grep("std",colnames(merge)),grep("mean[\\(\\)]",colnames(merge))))
The data is not in the correct order and I us th sort command to get a clean and sorted data set 
- MStd_order <-sort(MStd)
- MStd_order
Result
[1]   1   2   3   4   5   6  41  42  43  44  45  46  81  82  83  84  85  86 121 122 123 124 125 126 161 162 163 164 165 166
[31] 201 202 214 215 227 228 240 241 253 254 266 267 268 269 270 271 345 346 347 348 349 350 424 425 426 427 428 429 503 504
[61] 516 517 529 530 542 543

I want to have a subset with only the 66 variables containing the means and the standard deviations which I call Subset_MStd 
- Subset_MStd <- merge [,MStd]
Result: 10.299 obs with 66 variables

3. Uses descriptive activity names to name the activities in the data set
First I add the subjects to the data set by reading the subject both for the train.txt and test.txt files. I bind the rows with rbind and then bind the respective columns with cbind 
- subject_train <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/train/subject_train.txt", quote="\"")
- subject_test <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/test/subject_test.txt", quote="\"")

We bind the rows with rbind 
- subject_merge <- rbind(subject_train,subject_test)
We bind the respective columns with cbind 
- Subset_MStd <- cbind(Subset_MStd,subject_merge)

Finally I give the 67 column the name "subject" with colnames
- colnames(Subset_MStd)[67]<-"subject"
By checking the data I checked that it had the correct column name

The activities are generated by the y_train.txt and the y-test.txt files
I read the files through the read.table command
- y_train <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/train/y_train.txt", quote="\"")
- y_test <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/test/y_test.txt", quote="\"")

The rows of the activities are binded together through rbind
- y_TrainTest_Activity <- rbind(y_train,y_test)
the column is binded to the Subset_MStd
- Subset_MStd <- cbind(Subset_MStd,y_TrainTest_Activity)

Reading the activity labels and merging with the Subset_MStd
- activity_labels <- read.table("/Users/anknape/Desktop/Coursera/Course_project/UCI HAR Dataset/activity_labels.txt", quote="\"")
- Subset_MStd <- merge(Subset_MStd,activity_labels,by="V1")
- colnames(Subset_MStd)[69]<-"activity"
- colnames(Subset_MStd)[1]<-"activityID"

4. Appropriately labels the data set with descriptive variable names.
I used the gsub command that replaces the "content" of the first
variable with the "content" second variable
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

I checked the result and the labels were modified accordingly.

5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
As I have created in exercise 3 both the subjects and activity through r and cbind we can now split the data set into a subset by aggregating both list of the column subject and activityand of the data set Subset_MStd with the function mean.
- Sec_Ind_tidy_data <- aggregate( Subset_MStd,list(Subset_MStd$subject,Subset_MStd$activity),FUN=mean,na.rm=TRUE)

The tidy data set has correct labels therefore I give the first column the name "subject"
- colnames(Sec_Ind_tidy_data)[1]<-"subject"

and the second column the name "activity"
- colnames(Sec_Ind_tidy_data)[2]<-"activity"

The 70 and 71 column must be deleted as the subject and activity has been added as first and second column 
- Sec_Ind_tidy_data <- Sec_Ind_tidy_data [,1:69]

And finally write the data set to the local folder
- write.table(Sec_Ind_tidy_data,file="/Users/anknape/Desktop/Coursera/Course_project/ UCI HAR Dataset/Sec_Ind_tidy_data.txt",row.name=FALSE)



