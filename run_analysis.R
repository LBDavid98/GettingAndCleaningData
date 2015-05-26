# Creates one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy 
#  data set with the average of each variable for each activity and each subject.

run_analysis <- function() {

# Merge training and test sets to create one data set
# Compile test data  
  features <- read.table("features.txt")
  test_x <- read.table("x_test.txt")
  test_y <- read.table("y_test.txt")
  test_subj <- read.table("subject_test.txt")

  data_set <- matrix(rep("test", nrow(test_x)), 2947, 1)
  colnames(test_x) <- features[,2]
  colnames(test_y) <- "Activity"
  colnames(test_subj) <- "Subject"

  full_test <- cbind(data_set, test_subj, test_y, test_x)

# Compile training data  
  train_x <- read.table("x_train.txt")
  train_y <- read.table("y_train.txt")
  train_subj <- read.table("subject_train.txt")
  data_set <- matrix(rep("train", nrow(train_x)), nrow(train_x), 1)
  full_train <- cbind(data_set, train_subj, train_y, train_x)

# Combine into one data set
  colnames(full_train) <- colnames(full_test)
  complete_data <- rbind(full_test, full_train)
  rm(test_x, test_y, test_subj, train_x, train_y, train_subj, data_set, features) # clean up old data sets to free up memory

# Extract only the measurements on the mean and standard deviation for each measurement. 
  complete_data <- cbind(complete_data[,1:3], complete_data[, grep("mean\\(\\)|std\\(\\)", colnames(complete_data))])

# Use descriptive activity names for column 3
  complete_data$Activity <- factor(complete_data$Activity)
  activities <- read.table("activity_labels.txt")
  levels(complete_data$Activity) <- activities$V2
  complete_data

# For each subject and activity, get the mean value of each observation
  tidy_data <- aggregate(complete_data[4:ncol(complete_data)], list(Subject=complete_data$Subject, Activity=complete_data$Activity), mean)


# Project Step 5: Export tidy data set
  write.table(tidy_data, "tidydata.txt", row.names=FALSE)

# Clean house
  rm(tidy_data)
  rm(complete_data)

}
