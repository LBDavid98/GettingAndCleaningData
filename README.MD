## Getting and Cleaning Data: Course Project 
### README.md

Steps for using the run_analysis() function included in this repository.

1. Download the data https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
2. Unzip the data and move the following text files to a single folder
	* features.txt
	* x_test.txt
	* y_test.txt
	* subject_test.txt
	* x_train.txt
	* y_train.txt
	* subject_train.txt
	* activity_labels.txt
3. Download the R script run_analysis.R from this repository and place it in the same folder
4. Set the folder containing the script and your data to your working directory (?setwd for documentation)
5. Source("run_analysis.R")
6. Execute the run_analysis() function
7. A tidy data set ("tidydata.txt") will be placed in your working directory
8. TidyData <- read.table("tidydata.txt")