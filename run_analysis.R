# 1. Merge the training and the test sets to create one data set.

traindata <- read.table("UCI HAR Dataset/train/X_train.txt")
testdata <- read.table("UCI HAR Dataset/test/X_test.txt")
xdata <- rbind(traindata,testdata)

traindata <- read.table("UCI HAR Dataset/train/subject_train.txt")
testdata <- read.table("UCI HAR Dataset/test/subject_test.txt")
subjectdata <- rbind(traindata,testdata)

traindata <- read.table("UCI HAR Dataset/train/y_train.txt")
testdata <- read.table("UCI HAR Dataset/test/y_test.txt")
ydata <- rbind(traindata,testdata)

# 2. Extract only the measurements on the mean and standard
# deviation for each measurement.

features <- read.table("UCI HAR Dataset/features.txt")
indexes <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
xdata <- xdata[, indexes]
names(xdata) <- features[indexes, 2]
names(xdata) <- gsub("\\(|\\)", "", names(xdata))
names(xdata) <- tolower(names(xdata))

# 3. Use descriptive activity names to name the activities in the data set.

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
ydata[,1] = activities[ydata[,1], 2]
names(ydata) <- "activity"

# 4. Appropriately label the data set with descriptive activity names.

names(subjectdata) <- "subject"
deleted <- cbind(subjectdata, ydata, xdata)
write.table(deleted, "UCI HAR Dataset/merged_clean_data.txt")

# 5. Create a 2nd, independent tidy data set with the average
# of each variable for each activity and each subject.

uniqueSubjects = unique(subjectdata)[,1]
numSubjects = length(unique(subjectdata)[,1])
numActivities = length(activ[,1])
numCols = dim(deleted)[2]
result = deleted[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activ[a, 2]
    tmp <- deleted[deleted$subject==s & deleted$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}

# Write results table to a file
write.table(result, "UCI HAR Dataset/data_set_with_the_averages.txt")
