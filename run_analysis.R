library(dplyr) 

# Download the file and put the file in the working folder
#
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI_HAR_Dataset.zip"
if (!file.exists(zipFile)) { 
  download.file(zipUrl, zipFile, mode = "wb") 
}

# Unzip the file
#
dataPath <- "UCI HAR Dataset" 
if (!file.exists(dataPath)) { unzip(zipFile) }

# Read data from the files into the variables
# 
# Training files
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"),header = FALSE)
trainValues   <- read.table(file.path(dataPath, "train", "X_train.txt"),header = FALSE)
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"),header = FALSE) 

# Test files
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"),header = FALSE)
testValues   <- read.table(file.path(dataPath, "test", "X_test.txt"),header = FALSE)
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"),header = FALSE)

# Features files
features <- read.table(file.path(dataPath, "features.txt"),header = FALSE)

# Labels files
activities <- read.table(file.path(dataPath, "activity_labels.txt"),header = FALSE)
colnames(activities) <- c("Id", "Label") 

# 1. Merges the training and the test sets to create one data set.

# Merge the training and the test 
allSubjects <- rbind(trainSubjects, testSubjects) 
allActivity <- rbind(trainActivity, testActivity) 
allValues   <- rbind(trainValues, testValues) 

# set the names to variables columns
names(allSubjects)<-c("subject")
names(allActivity)<-c("activity")
names(allValues)  <- features$V2

# Merge columns of the 3 files to get the data frame
allData <- cbind(allSubjects, allActivity, allValues)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
searchFeaturesNames <- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
selectedNames <- c("subject", "activity", as.character(searchFeaturesNames))
Data <- subset(allData, select=selectedNames)

# 3. Uses descriptive activity names to name the activities in the data set

# replace activity values with named factor from activities
Data$activity <- factor(Data$activity, levels = activities[, 1], labels = activities[, 2]) 

# 4. Appropriately labels the data set with descriptive variable names.

# get column names 
DataCols <- colnames(Data) 

# expand abbreviations and clean up names 
DataCols <- gsub("^f", "frequency", DataCols) 
DataCols <- gsub("^t", "time", DataCols) 
DataCols <- gsub("Acc", "Accelerometer", DataCols) 
DataCols <- gsub("Gyro", "Gyroscope", DataCols) 
DataCols <- gsub("Mag", "Magnitude", DataCols) 
DataCols <- gsub("Freq", "Frequency", DataCols) 
DataCols <- gsub("mean", "Mean", DataCols) 
DataCols <- gsub("std", "StandardDeviation", DataCols) 

colnames(Data) <- DataCols

# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject. 

# group by subject and activity and summarise using mean 
DataMeans <- Data %>% group_by(subject, activity) %>% summarise_all(funs(mean)) 

write.table(DataMeans, file = "tidydata.txt", row.name=FALSE, quote=FALSE)

