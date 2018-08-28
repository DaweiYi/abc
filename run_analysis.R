#import library
library(data.table)
library(dplyr)

#load datsets
Feature_test <-
  read.table("~/Downloads/UCI HAR Dataset/test/X_test.txt", header = FALSE)
Subject_test <-
  read.table("~/Downloads/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
Activity_test <-
  read.table("~/Downloads/UCI HAR Dataset/test/y_test.txt", header = FALSE)
Feature_train <-
  read.table("~/Downloads/UCI HAR Dataset/train/X_train.txt", header = FALSE)
Subject_train <-
  read.table("~/Downloads/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
Activity_train <-
  read.table("~/Downloads/UCI HAR Dataset/train/y_train.txt", header = FALSE)
Feature_names <-
  read.table("~/Downloads/UCI HAR Dataset/features.txt", header = FALSE)
Activity_names <-
  read.table("~/Downloads/UCI HAR Dataset/activity_labels.txt", header = FALSE)
#1.Merges the training and the test sets to create one data set.
#bind data
Subject_df <- rbind(Subject_train, Subject_test)
colnames(Subject_df)<-"subject"
##bind Feature data and rename the colnames
Feature_df<-rbind(Feature_train,Feature_test)
colnames(Feature_df)<-Feature_names$V2
##bind Feature data and rename the colnames
Activity_df<-rbind(Activity_train,Activity_test)
colnames(Activity_df)<-"activity"


Total_df<-cbind(Feature_df,Activity_df,Subject_df)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
STD_MEAN_COL<-colnames(Total_df)[grepl("mean|std|subject|activity",tolower(colnames(Total_df)))]

STD_MEAN_df<-Total_df[,STD_MEAN_COL]

#3.Uses descriptive activity names to name the activities in the data set
STD_MEAN_df$activity <- as.character(STD_MEAN_df$activity)
for (i in 1:6){
  STD_MEAN_df$activity[STD_MEAN_df$activity == i] <- as.character(Activity_names[i,2])
}
#4. Appropriately labels the data set with descriptive variable names.
names(STD_MEAN_df)<-gsub("Acc", "Accelerometer", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("Gyro", "Gyroscope", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("BodyBody", "Body", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("Mag", "Magnitude", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("^t", "Time", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("^f", "Frequency", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("tBody", "TimeBody", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("-mean()", "Mean", names(STD_MEAN_df), ignore.case = TRUE)
names(STD_MEAN_df)<-gsub("-std()", "STD", names(STD_MEAN_df), ignore.case = TRUE)
names(STD_MEAN_df)<-gsub("-freq()", "Frequency", names(STD_MEAN_df), ignore.case = TRUE)
names(STD_MEAN_df)<-gsub("angle", "Angle", names(STD_MEAN_df))
names(STD_MEAN_df)<-gsub("gravity", "Gravity", names(STD_MEAN_df))

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
STD_MEAN_df$subject<-factor(STD_MEAN_df$subject)
tidy_df<-STD_MEAN_df %>% 
  group_by(subject,activity) %>% 
  summarise_all(funs(mean))

write.table(tidy_df, file = "Tidy.txt", row.names = FALSE)