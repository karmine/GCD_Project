#########################################################
# Author: Sergio Contador
# Date: February 2017
# Description: Getting and Cleaning Data Course Project from Coursera
# Steps:
## 0. Load datasets.
## 1. Merges the training and test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation
##    for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set.
## 4. Appropriately labels the data set with descriptive activity names. 
## 5. Creates a second, independent tidy data set with the average of each 
##    variable for each activity and each subject.
#########################################################







# Load Required Libraries
library(plyr)





# 0. Load datasets.
## Create dir principal
dir.principal <- paste(getwd(), "/R/Programas/Data_Scientist/GCD/Project/UCI HAR Dataset", sep = "")


# Load activity_labels
dir <- paste(dir.principal, "/activity_labels.txt", sep = "")
activity <- read.table(dir)


# Load features
dir <- paste(dir.principal, "/features.txt", sep = "")
feature <- read.table(dir)


# Load train data
dir <- paste(dir.principal, "/train/X_train.txt", sep = "")
train <- read.table(dir)

dir <- paste(dir.principal, "/train/y_train.txt", sep = "")
trainActivity <- read.table(dir)

dir <- paste(dir.principal, "/train/subject_train.txt", sep = "")
trainSubject <- read.table(dir)


# Load test data
dir <- paste(dir.principal, "/test/X_test.txt", sep = "")
test <- read.table(dir)

dir <- paste(dir.principal, "/test/y_test.txt", sep = "")
testActivity <- read.table(dir)

dir <- paste(dir.principal, "/test/subject_test.txt", sep = "")
testSubject <- read.table(dir)





# 1. Merges the training and test sets to create one data set.
train <- cbind(trainSubject, trainActivity, train)
test <- cbind(testSubject, testActivity, test)
frame <-  data.frame(rbind(train, test))
names(frame)[1:3] <- c("Subject", "Activity", "V1")





# 2. Extracts only the measurements on the mean and standard deviation
#    for each measurement. 
position <- grep("-(mean|std)\\(\\)", feature[, 2])
position <- position + 2
frame <- frame[, c(1, 2, position)]


# Order new data
frame <- frame[order(as.numeric(frame$Subject), as.character(frame$Activity)),]
row.names(frame) <- c(1:dim(frame)[1])





# 3. Uses descriptive activity names to name the activities in the data set.
frame$Activity <- as.factor(as.character(frame$Activity))
levels(frame$Activity) <- as.character(activity$V2)





# 4. Appropriately labels the data set with descriptive activity names. 
names(frame)[3:dim(frame)[2]] <- as.character(feature$V2[position - 2])





# 5. Creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject.
## Required library(plyr)
frame2 <- ddply(frame, .(Subject, Activity), function(x) colMeans(x[, 3:dim(frame)[2]]))


# Order new data
frame2 <- frame2[order(as.numeric(frame2$Subject), as.character(frame2$Activity)),]
row.names(frame2) <- c(1:dim(frame2)[1])


# Save new data
dir <- paste(dir.principal, "/tidy.txt", sep = "")
write.table(frame2, dir, row.names = FALSE, quote = FALSE)





