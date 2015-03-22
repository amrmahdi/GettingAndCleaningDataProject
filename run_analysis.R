# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Given path to features file, activity labels file and dubject file
# this method extracts the mean and standard deviation from the features file
# and labels the headers for the features using the headers from features.txt file 
# It then add to each observation the subject that performed this observation and the
# type of activity the subject performed during this observation
ExtractDataSet <- function(featuresPath, labelsPath, subjectsPath) {
  labels <- tbl_df(read.table(labelsPath))
  features <- tbl_df(read.table(featuresPath, col.names=header))
  subjects <- tbl_df(read.table(subjectsPath, col.names=c("Subject")))
  
  activities <- mutate(labels, V1 = activitiesHeaders[V1])
  colnames(activities) = c("Activity")
  activities = tbl_df(activities)
  
  features <- select(features, matches("\\W+(mean|std)\\W+", ignore.case = TRUE))
  
  dataset = tbl_df(cbind(cbind(subjects, activities), features))
  
  dataset
}

# read the column headers from features.txt
header <- as.vector(read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt", sep=" ")[[2]])

# read the activity labels from activity_labels.txt
activitiesHeaders <- as.vector(read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", sep=" "))[[2]]

# exctracts the test data from test folder
test <- ExtractDataSet("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/x_test.txt", "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")

# extracts train data from train folder
train <- ExtractDataSet("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/x_train.txt", "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")

# merges train and test datasets into 1 dataset
merged <- tbl_df(rbind(test,train))


# use descritptive names for the headers (replace with readable names)
# we will replace abbreviations with the full words suffixed and prefixed with '.'
names(merged) <- gsub('^t',".Time.",names(merged))
names(merged) <- gsub('^f',".Frequency.",names(merged))
names(merged) <- gsub('Acc',".Acceleration.",names(merged))
names(merged) <- gsub('GyroJerk',".AngularAcceleration.",names(merged))
names(merged) <- gsub('Gyro',".AngularVelocity.",names(merged))
names(merged) <- gsub('Mag',".Magnitude.",names(merged))
names(merged) <- gsub('Freq\\.',".Frequency.",names(merged))

# reformat the mean and standard deviation headers
names(merged) <- gsub('\\.mean',".Mean.",names(merged))
names(merged) <- gsub('\\.std',".StandardDeviation.",names(merged))

# remove extra 'Body' word feom the headers
names(merged) <- gsub("BodyBody",".Body.",names(merged))


#remove dots at the beggining
names(merged) <- gsub("^(\\.)+","",names(merged))

# remove dots at the end
names(merged) <- gsub("(\\.)+$","",names(merged))

# remove extra dots at the middle
names(merged) <- gsub("(\\.)+",".",names(merged))

# now group by the Activity and Subject the mean of each observation we have in the merged data
grouped <- merged  %>%
          group_by(Activity, Subject) %>%
          summarise_each(funs(mean))

# write the grouped data in a txt file
write.table(merged, row.name=FALSE, file="tidyData.txt", quote = FALSE) 
