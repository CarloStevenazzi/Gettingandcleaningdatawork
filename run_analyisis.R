# create data set "tiny.txt" and take variables filePath, filteredFeatures, features
# * filePath: path of the file
# * filteredFeatures: ids of the features 
# * features: all features names
readBaseSet <- function(filePath, filteredFeatures, features) {
        cols_widths <- rep(-16, length(features))
        cols_widths[filteredFeatures] <- 16
        rawSet <- read.fwf(
                file=filePath,
                widths=cols_widths,
                col.names=features[filteredFeatures])
}

#This part is to add an additional file 

readAdditionalFile <- function(dataDirectory, filePath) {
        filePathTest <- paste(dataDirectory, "/test/", filePath, "_test.txt", sep="")
        filePathTrain <- paste(dataDirectory, "/train/", filePath, "_train.txt", sep="")
        data <- c(read.table(filePathTest)[,"V1"], read.table(filePathTrain)[,"V1"])
        data
}

# Cleaning the data set by removing parenthesis

correctFeatureName <- function(featureName) {
        featureName <- gsub("\\(", "", featureName)
        featureName <- gsub("\\)", "", featureName)
        featureName
}

# Read sets and returns a complete sets
readSets <- function(dataDirectory) {
        # Adding main data files (X_train and X_test)
        featuresFilePath <- paste(dataDirectory, "/features.txt", sep="")
        features <- read.table(featuresFilePath)[,"V2"]
        filteredFeatures <- sort(union(grep("mean\\(\\)", features), grep("std\\(\\)", features)))
        features <- correctFeatureName(features)
        set <- readBaseSet(paste(dataDirectory, "/test/X_test.txt", sep=""), filteredFeatures, features)
        set <- rbind(set, readBaseSet(paste(dataDirectory, "/train/X_train.txt", sep=""), filteredFeatures, features))
        
        # Adding subjects
        set$subject <- readAdditionalFile("UCI HAR Dataset", "subject")
        
        # Adding activities
        activitiesFilePath <- paste(dataDirectory, "/activity_labels.txt", sep="")
        activities <- read.table(activitiesFilePath)[,"V2"]
        set$activity <- activities[readAdditionalFile("UCI HAR Dataset", "y")]
        
        set
}

# creating the summary

createSummary <- function(dataDirectory) {
        sets <- readSets(dataDirectory)
        sets_x <- sets[,seq(1, length(names(sets)) - 2)]
        summary_by <- by(sets_x,paste(sets$subject, sets$activity, sep="_"), FUN=colMeans)
        summary <- do.call(rbind, summary_by)
        summary
}

dataDirectory <- "UCI HAR Dataset"
if (!file.exists(dataDirectory)) {
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        tmp_file <- "./temp.zip"
        download.file(url,tmp_file, method="curl")
        unzip(tmp_file, exdir="./")
        unlink(tmp_file)
}
# output the sumary
summary <- createSummary(dataDirectory)
write.table(summary, "tidy.txt")
