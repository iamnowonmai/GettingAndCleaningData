head# You should create one R script called run_analysis.R that does the following. 
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 
# Good luck!
# People have lost major marks in previous sessions by having a brain-fade and 
# forgetting about critical files so not getting the marks: you want a run_analysis R script, 
# a ReadMe markdown document, a Codebook markdown document, and a tidy data text file 
# (this last goes on Coursera).

run_analysis <- function() {
        # read the files
        x_train <- read.table("train/X_train.txt")
        subject_train <- read.table("train/subject_train.txt")
        y_train <- read.table("train/y_train.txt")
        x_test <- read.table("test/X_test.txt")
        subject_test <- read.table("test/subject_test.txt")
        y_test <- read.table("test/y_test.txt")
        features <- readLines("features.txt")
        # features needs some cleaning up before it can be used:
        ## remove number prefixes and parenthesis 
        features <- sub("^[0-9]+ a", "a", features)
        features <- sub("^[0-9]+ f", "f", features)
        features <- sub("^[0-9]+ t", "t", features)
        features <- sub("\\(", "", features)
        features <- sub("\\)", "", features)
        features <- sub("mean", "Mean", features) #make it more readable
        features <- sub("std", "Std", features)
        ##get rid of dashes gsub because there is more than one dash per variable sometimes!
        features <- gsub("-", "" , features) 
        # remove BodyBody
        features <- sub("fBodyBody", "fBody", features)
        # combined <- rbind(x_train, x_test)
        ## add columns to x_test data for subject and activity
        names(x_train) <- features
        names(x_test) <- features
        x_test$subject <- readLines("test/subject_test.txt")
        x_test$activity <- readLines("test/y_test.txt")
        x_train$subject <- readLines("train/subject_train.txt")
        x_train$activity <- readLines("train/y_train.txt")
        # merge the files into one Yes, join the data together. Add rows (and columns for the subject and activity).
        x_combined <- rbind(x_train, x_test)
        ## columns that I want to keep:
        # create a new table with extracted mean/stdev
        # need to select the columns that have mean or std()
        x_dataset <- subset(x_combined, select = grep("Std|Mean|subject|activity", names(x_combined)))
        
        # activity lables can be used as a factor label to activities. 
        
        x_dataset$activity <- factor(x_dataset$activity, levels = 1:6, labels = c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"), ordered = TRUE)


        # calculate mean of quantitative data
        
        x_melt <- melt(x_dataset, id = c("subject", "activity"))
        x_grouped <- ddply(x_melt, .(subject, activity), summarize, ave = mean(value))
        x_final <- spread(x_grouped, activity, ave)
        x_tidy <- x_final[order(as.numeric(x_final$subject)),]
        write.table(x_tidy, file = "tidy.txt", row.names = FALSE)
        x_tidy
        
}