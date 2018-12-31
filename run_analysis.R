library(tidyr)
library(dplyr)
library(reshape2)
# Set working directory to where "unzipped" folder was saved
setwd("/insert/your/path/here")

# Read in data files
subject_test <- read.table("test/subject_test.txt")
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_train <- read.table("train/subject_train.txt")
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")


activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")


# Req: 
# Step 1: Merge the training and the test sets to create one data set
# Step 2: Extracts only the measurement on the mean and standard deviation for each measurement
# Step 3: Use descriptive activity names to name the activites in the data set
# Step 4: Appropriatelty label the data set with descriptive variable names
# Step 5: From the data set in step 4, create a second, independent tidy data set with the average of each variable for each
# activity and each subjectAc
# rbind to combine both the train and test datasets
combined_df <- rbind(X_train, X_test)
# mean_std = regular expression looking for mean or standard deviation within the second column on the feature df
mean_std <- grep("mean()|std()", features[,2])
combined_df <- combined_df[,mean_std]

# substitue parenthises with the feature names and replcae with nothing then apply parsed names to combined_df
clean_feature_names <- sapply(features[,2], function(x){gsub("[()]","",x)})
names(combined_df) <- clean_feature_names[mean_std]

# combining the subject training and test datasets
subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'


# combine subject, activity, and the mean/std deviation to create the final dataset. This time using a column bind opposed to 
# a row bind
final_df <- cbind(subject, activity, combined_df)


activity_group <- factor(final_df$activity)
levels(activity_group) <- activity_labels
final_df$activity <- activity_group


# Creating a new dataset with the average of each variable for each activity and subject
data <- melt(final_df,(id.vars=c("subject","activity")))
to_write <- dcast(data, subject + activity ~ variable, mean)
names(to_write)[-c(1:2)] <- paste("[mean of]" , names(to_write)[-c(1:2)] )
write.table(two_write, "tidy_data.txt", sep = ",")

