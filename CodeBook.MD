## Getting and Cleaning Data: Course Project 
### Code Documentation

1. All provided data tables must be unzipped and moved to the same folder
	* features.txt
	* x_test.txt
	* y_test.txt
	* subject_test.txt
	* x_train.txt
	* y_train.txt
	* subject_train.txt
	* activity_labels.txt
2. The data tables are read into memory and compiled
	* Features, x_test, y_test and subject_test are read
	* Column names for test_x are set to the values from the features table
	* Data sets combined by column: DataSet Identifier ("test"), Subject, Y, and X
	* The process is repeated for the train data using the dataset identifier ("train")
3. Test and training data are combined using rbind
4. The resulting data set is subset to include only measurements of mean or std using cbind on:
	* Columns 1:3, indicating data set, subject and activity
	* Subset of the data set identified using grep() 
5. Activities are identified in descriptive English
	* complete_data$Activity factored with levels from the activity_labels table.
6. A final data set is created for each subject and activity 
	* Aggegate() of mean() is applied to the dataset
	* Columns selected for mean() are 4:ncol(complete_data)
	* List of observed aggregates: complete_data$Subject, complete_data$Activity
7. The tidy data set is written as a table to the file "tidydata.txt" in the same folder.

