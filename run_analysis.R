

#1. Downloads the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Download_data <- function(url, directory, filename){
  if(!file.exists(directory)) {
    dir.create(directory)
  }
  setwd(directory)
  download.file(url, destfile = filename)
  unzip(filename)
   
}

Download_data("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
         directory,filename)

#2. Importing the data  
train_data <- read.csv("~/UCI HAR Dataset/train/x_train.txt", sep="", header=FALSE )
train_id <- read.csv("~/UCI HAR Dataset/train/subject_train.txt", sep= "", header = FALSE)
train_labels <- read.csv("~/UCI HAR Dataset/train/y_train.txt", header = FALSE)
test_data <- read.table("~/UCI HAR Dataset/test/x_test.txt",  sep= "", header = FALSE)
test_id <- read.csv("~/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_labels <- read.csv("~/UCI HAR Dataset/test/y_test.txt", header = FALSE)
activity_labels <- read.csv("~/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = " ")
features <- read.csv("~/UCI HAR Dataset/features.txt", header = FALSE, sep = "", colClasses = c("character"))

#3. merger data in one set
train <- cbind(train_id, train_labels, train_data)
test <- cbind(test_id, test_labels, test_data)
alldata <- rbind(train, test)

#4. Appropriately labels the data set with descriptive variable names.
features <- gsub("\\()", "", features[,2]) #removing ()
features <- tolower(features)
colnames(alldata) <- c("subjectid", "activityid", features)

#5. filtering for std and mean
filterlist <- as.data.frame(grep("mean()|std", features, value = TRUE)) 
colnames(filterlist) <- c("V1")
filteraddon <- as.data.frame(matrix(c("subjectid","activityid"), nrow = 2, ncol = 1), dimnames = list(c(), c("Subset")))
filterlist <- rbind(filteraddon, filterlist)
DB <- alldata[colnames(alldata)%in%filterlist[,1]]


#6. data set with the average of each variable for each activity and each subject.
library(plyr)
library(dplyr)

FUN_calc_mean <- function(df) {
  output <- data.frame()     
  for (j in 1:30) {          
    tmp_1 <- filter(df, subjectid == j) 
        for (i in 1:6){  
        tmp_2 <- filter(tmp_1, activityid == i) 
        tmp_3 <- cbind(subjectid = j, activityid = i, as.data.frame(t(apply(tmp_2[4:82], 2, function(x) mean(x))))) 
        output <- rbind(output, tmp_3) 
        } 
  }
  return(output) 
}

finalDB<- FUN_calc_mean(DB)

#7.Adding activity_labels and saves the data set. 
colnames(activity_labels) <- c("activityid", "activity")
tidydataset <- merge(activity_labels , finalDB, by.x = 1, by.y = 2, sort = FALSE)
tidy <- tidydataset[-c(1)] 
write.table(tidy, "~/tidy_dataset", row.names = FALSE)


