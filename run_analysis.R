XTrain <- XTest <- NULL
    # Get and extract data
runAnalysis <- function() {
    
    filePath <- function(...) { paste(..., sep = "/") }
    
    downloadData <- function() {
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        downloadDir <- "data"
        
        if(!dir.exists(downloadDir)) { dir.create(downloadDir) }
        
        zipFile <- filePath(downloadDir, "dataset.zip")
        if(!file.exists(zipFile)) { download.file(url, zipFile) }
        
        dataDir <- "UCI HAR Dataset"
        
        if(file.exists(zipFile)) { unzip(zipFile, exdir = downloadDir) }
    
        dataDir
    }
    
    
    
    newPath<- paste(getwd(),"data/UCI HAR Dataset",sep= "/")
    dataDir <- newPath
    setwd(newPath)
    
    readData <- function(path) {
        read.table(filePath(dataDir, path))
    }
    
    # Read and cache XTrain and XTest data
    if(is.null(XTrain)) { XTrain <<- readData("train/X_train.txt") }
    if(is.null(XTest))  { XTest  <<- readData("test/X_test.txt") }
    merged <- rbind(XTrain, XTest)
    
    
    featureNames <- readData("features.txt")[, 2]
    names(merged) <- featureNames
    
    matches <- grep("(mean|std)\\(\\)", names(merged))
    limited <- merged[, matches]
    
    
    yTrain <- readData("train/y_train.txt")
    yTest  <- readData("test/y_test.txt")
    yMerged <- rbind(yTrain, yTest)[, 1]
    
    activityNames <-
        c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
    activities <- activityNames[yMerged]
    
    activityNames <-
        c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
    activities <- activityNames[yMerged]
    
    
    names(limited) <- gsub("^t", "Time", names(limited))
    names(limited) <- gsub("^f", "Frequency", names(limited))
    names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
    names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
    names(limited) <- gsub("-", "", names(limited))
    names(limited) <- gsub("BodyBody", "Body", names(limited))

    
    subjectTrain <- readData("train/subject_train.txt")
    subjectTest  <- readData("test/subject_test.txt")
    subjects <- rbind(subjectTrain, subjectTest)[, 1]
    
    tidy <- cbind(Subject = subjects, Activity = activities, limited)
    
    # Create a second, independent tidy data set with the average of each variable for each activity and each subject.
    library(plyr)
    # Column means for all but the subject and activity columns
    limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
    tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
    names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])
    
    # Write file
    write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE)
    
    # Also return data
    tidyMeans
}

# Use to check that the tidyMeans.txt is properly readable
checkData <- function() {
    read.table("tidyMeans.txt", header = TRUE)
}