acceldata <-  function () {
  library(plyr)
  library(dplyr)
  library(reshape2)
  
  #
  #Download and unzip the dataset
  #
  filename <- "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename)
  }  
  if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
  }
  
  #
  #Read in both datasets first and combine them
  #
    test <- read.table("UCI_HAR_Dataset/test/X_test.txt")
    train <- read.table("UCI_HAR_Dataset/train/X_train.txt")
    combined_ds <- rbind(test,train)

    y_test <- read.table("UCI_HAR_Dataset/test/y_test.txt")
    subject_test <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
    y_train <- read.table("UCI_HAR_Dataset/train/y_train.txt")
    subject_train <- read.table("UCI_HAR_Dataset/train/subject_train.txt")
    activity_values <- rbind(y_test,y_train)
    subject_values <- rbind(subject_test,subject_train)
    
  #
  #Find the indices of relevant columns containing mean and std values
  #
  
  #Read in the list of data features, aka accel variable data (all 561)
  data_key <- read.table("UCI_HAR_Dataset/features.txt", sep=" ", header = FALSE)
  
  #find the locations of the means and standard deviations
  desired_mean_std_indices <- grep("mean|std",data_key[,2])
    
  #create a vector std_data_labels of all the items that match for the mean and std indices
  mean_std_data_labels <- filter(data_key,V1 %in% desired_mean_std_indices )
    
  #
  #Create a new datafile with only the mean and standard deviation columns
  #
  selected_ds <- combined_ds[,desired_mean_std_indices]
  selected_ds_all <- cbind(subject_values,activity_values,selected_ds)
    
  #
  #Attach the names of those columns to the dataset
  #
  #Give the columns headings
  col_data_labels <- as.character(mean_std_data_labels[,"V2"])
    
  #Remove the extra characters from the names to make them more readable
  col_data_labels <- gsub("-|\\(|\\)","",col_data_labels)
  
  #Assign the column names to the table
  col_names_vector <- c("Subject","Activity",col_data_labels)
  colnames(selected_ds_all) <- col_names_vector
    
  #
  #Find the mean for each subject/activity using the melt function
  #
    
  melted_subject_activity <- melt(selected_ds_all, id = c("Subject", "Activity"))
  table_means <- dcast(melted_subject_activity, Subject + Activity ~ variable, mean)
     
  #
  #Replace numerical activity labels with corresponding text
  #
  activity_labels<- read.table("UCI_HAR_Dataset/activity_labels.txt")
     
  for (i in 1:length(table_means[,2])){
       match_index <- match(table_means[i,2],activity_labels[,1])
       table_means[i,2] <- as.character(activity_labels[match(table_means[i,2],activity_labels[,1]),2])
  }
  
  #
  #Write the tidy table
  #
  write.table(table_means, "tidy.txt", row.names = FALSE, quote = FALSE)
}