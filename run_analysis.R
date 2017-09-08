# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


# Cleaning the workspace
rm(list=ls())

# Task 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped

# Read in the data from files
features     = read.table('./features.txt',header=FALSE); #to import features.txt
activity_label = read.table('./activity_labels.txt',header=FALSE); #to import activity_labels.txt
subject_train = read.table('./train/subject_train.txt',header=FALSE); #to import subject_train.txt
x_train       = read.table('./train/x_train.txt',header=FALSE); #to import x_train.txt
y_train       = read.table('./train/y_train.txt',header=FALSE); #to import y_train.txt

# Assigin column names to the data data sets
colnames(activity_label)  = c('activityId','activity_label');
colnames(subject_train)  = "subjectId";
colnames(x_train)        = features[,2]; 
colnames(y_train)        = "activityId";

# to create the final training set by merging y_train, subject_train, and x_train
training_ata = cbind(y_train,subject_train,x_train);

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #to import subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #to import x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #to import y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);


# Combine training and test data to create a final data set
finalData = rbind(training_ata,testData);

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finalData); 

# Task 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];

# Task 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activity_label,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# Task 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# Task 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoactivity_label without the activity_label column
finalDataNoactivity_label  = finalData[,names(finalData) != 'activity_label'];

# Summarizing the finalDataNoactivity_label table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoactivity_label[,names(finalDataNoactivity_label) != c('activityId','subjectId')],by=list(activityId=finalDataNoactivity_label$activityId,subjectId = finalDataNoactivity_label$subjectId),mean);

# Merging the tidyData with activity_label to include descriptive acitvity names
tidyData    = merge(tidyData,activity_label,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');