## run_analysis break-down:

##Introduction
The script "run_analysis.R"  generates a tidy data set from "Human Activity Recognition Using Smartphones Data Set".
The script works in 7 steps. (The numbers matches the numbers in the script run_analysis.R)

1. Downloads the file from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and extracts. User input "directory"(directory where the files should be downloaded to) and "filename" (the name of the file).
2. Importing the data into R. User input is the directory where the downloaded file is extracted.
3. Merging the data into a single data set. 
4. Changing the variables names to more tidy names.
5. Filtering the merged data set for mean() and std() variables.
6. The function calculates the mean of the mean() and std() for each observation for each activity and subject.
7. Adds activity labels instead of numbers and saves the file. The user input is the destination for the file to be saved. 
