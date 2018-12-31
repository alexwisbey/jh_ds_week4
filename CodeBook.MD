Code Book 
========

## Data
* subject_test : Each row identifies the subject who performed the activity
* X_test : Test Set
* y_test : Test lables
* subject_train : Each row identifies the subject who performed the activity
* X_Train : Training Set
* y_train : Training lables
* activity_labels: Links the class lables with their activity names

## Step 1: Merge the training and the test sets to create one data set
*  rbind the X_train and X_test data sets
## Step 2: Extracts only the measurement on the mean and standard deviation for each measurement
* Using regex extract the std deviation or mean from the feautre df 
## Step 3: Use descriptive activity names to name the activites in the data set
## Step 4: Appropriatelty label the data set with descriptive variable names
* After extracting the features that correspond to the regex add these to the column names of the rbind dataset from step 1
* Additional processing is done to remove the "()" from the features dataset
* Activity names are taken from the activity dataset 
## Step 5: From the data set in step 4, create a second, independent tidy data set with the average of each variable for eachactivity and each subject
* Created a tidy data set and wrote to a text file names "tidy_data.txt"
