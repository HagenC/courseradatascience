
setwd("C:/Users/UNC/Desktop/R_Coursera")

Download_data <- function(url, directory, filename){
  if(!file.exists(directory)) {
    dir.create(directory)
  }
  setwd(directory)
  download.file(url, destfile = filename)
  unzip(filename)
   
}

Download_data("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
         "C:/Users/UNC/Desktop/R_Coursera/assignment5","DataSet.zip")

#Importing the data  
train_data <- read.csv("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/train/x_train.txt", sep="", header=FALSE )
train_id <- read.csv("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/train/subject_train.txt", sep= "", header = FALSE)
train_labels <- read.csv("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/train/y_train.txt", header = FALSE)
test_data <- read.table("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/test/x_test.txt",  sep= "", header = FALSE)
test_id <- read.csv("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_labels <- read.csv("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/test/y_test.txt", header = FALSE)
activity_labels <- read.csv("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = " ")
features <- read.csv("C:/Users/UNC/Desktop/R_Coursera/Assignment Week 4/UCI HAR Dataset/features.txt", header = FALSE, sep = "", colClasses = c("character"))

#merger data in one set
train <- cbind(train_id, train_labels, train_data)
test <- cbind(test_id, test_labels, test_data)
alldata <- rbind(train, test)

#Appropriately labels the data set with descriptive variable names.
features <- gsub("\\()", "", features[,2]) #removing ()
features <- tolower(features)
colnames(alldata) <- c("subjectid", "activityid", features)

#filtering for std and mean
filterlist <- as.data.frame(grep("mean()|std", features, value = TRUE)) 
colnames(filterlist) <- c("V1")
filteraddon <- as.data.frame(matrix(c("subjectid","activityid"), nrow = 2, ncol = 1), dimnames = list(c(), c("Subset")))
filterlist <- rbind(filteraddon, filterlist)
allsubdata <- alldata[colnames(alldata)%in%filterlist[,1]]

#adds activitylabels
colnames(activity_labels) <- c("activityid", "activity")
DB <- merge(activity_labels, allsubdata, by.x= 1, by.y = 2)


#data set with the average of each variable for each activity and each subject.
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
names(finalDB)


