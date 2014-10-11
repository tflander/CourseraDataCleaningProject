library(data.table)

init <- function() {

  # Define Variables
  dataFolder <- "UCI HAR Dataset"
  
  ####################################
  # Define methods used by main flow #
  ####################################
  
  # download the data set "Human Activity Recognition Using Smartphones Data Set" if it doesn't exist
  downloadDataIfNecessary <- function() {
    zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipFile <- "Dataset.zip"
    
    if(!file.exists(zipFile)) {
      download.file(zipUrl, destfile=zipFile, method="curl")
      unzip(zipFile)
      if(!file.exists(dataFolder)) {
        stop("Error: expected data folder ", dataFolder)
      }
      message("data extracted")
    }
  }
    
  # helper function to paste strings together without decoration
  concat <- function(x) {
    paste(x, collapse = '')
  }
  
  buildSubjectsForFile <- function(fileSpec) {
    subjectFile <- concat(c(dataFolder, fileSpec))
    subjects <- read.csv(subjectFile, header = FALSE)    
    names(subjects) <- "subjectId"
    subjects      
  }
  
  buildSubjects <- function() {
    testSubject <- buildSubjectsForFile("/test/subject_test.txt")
    trainSubject <- buildSubjectsForFile("/train/subject_train.txt")
    rbind(trainSubject, testSubject)    
  }
    
  # This function satisfies the following requirements for the project:
  #    1) Merges the training and the test sets to create one data set.
  #        We ignore the raw inertia signals files and instead use the prebuilt
  #        data sets in the files X_train.txt and X_test.txt.
  #    TODO:  Should I do step 2 here? Probably not, since I ignores the inertia signals files
  #    3) Uses descriptive activity names to name the activities in the data set
  #        The activity names come from the file activity_labels.txt.
  #    4) Appropriately labels the data set with descriptive variable names. 
  #        The data set data columns come from the file features.txt.
  #
  #    Refer to the code book for detailed information
  buildMasterData <- function() {
    activityLabelsFile <- concat(c(dataFolder, "/activity_labels.txt"))
    
    activityLabels <- fread(activityLabelsFile, sep = " ", header = FALSE)
    setkey(activityLabels, V1)
        
    buildActivityLabels <- function() {
      
      buildActivityLabel <- function(fileName) {
        alabels <- fread(fileName)
        setkey(alabels, V1)
        dt <- merge(activityLabels, alabels) 
        setnames(dt, c("V1", "V2"), c("activityId", "activityName"))
        dt
      }
      
      trainYFile <- concat(c(dataFolder, "/train/y_train.txt"))
      testYFile <- concat(c(dataFolder, "/test/y_test.txt"))
      trainingLabels <- buildActivityLabel(trainYFile) 
      testingLabels <- buildActivityLabel(testYFile) 
      rbind(trainingLabels, testingLabels)
    }
    
    buildFeatures <- function() {
      featuresFile <- concat(c(dataFolder, "/features.txt"))      
      lapply(read.csv(featuresFile, header = FALSE, sep = " ")[,2], as.character)
    }  
    
    buildSetData <- function() {
      
      trainXFile <- concat(c(dataFolder, "/train/X_train.txt"))
      testXFile <- concat(c(dataFolder, "/test/X_test.txt"))
      trainingSet <- read.table(trainXFile, header = FALSE) # (7552 observances of 561 variables)
      testingSet <- read.table(testXFile, header = FALSE) #  (2947 observances of 561 variables)      
      data.table(rbind(trainingSet, testingSet))
    }
                
    features <- buildFeatures()
    masterSet <- buildSetData()
    setnames(masterSet, names(masterSet), unlist(features))
    masterLabels <- buildActivityLabels()
    subjects <- buildSubjects()
    cbind(masterLabels, subjects, masterSet)
  }
  
  buildMainDataSet <- function() {
    trainFolder <- concat(c(dataFolder, "/train/Inertial Signals"))
    testFolder <- concat(c(dataFolder, "/test/Inertial Signals"))
    
    trainFiles <- dir(trainFolder)
    testFiles <- dir(testFolder)
    
    result <<- data.frame(measurement=character(),
                             feature=character(),
                             mean=numeric(), 
                             stddev=numeric(), 
                             stringsAsFactors=FALSE)
    
    testSubjects <- buildSubjectsForFile("/test/subject_test.txt")
    trainSubjects <- buildSubjectsForFile("/train/subject_train.txt")
    
    for (file in testFiles) {
      measurement <- substr(file, 1, nchar(file) - 9)
      filePath <- concat(c(dataFolder, "/test/Inertial Signals/", file))
      message(filePath)
      data <- read.table(filePath, header = FALSE)
      message(dim(data)) # all are 2947 obs of 128 vars
      meanData <- apply(data, 1, mean)
      stddevData <- apply(data, 1, sd)
      a <- cbind(data.frame(rep(measurement, length(mean))), testSubjects, meanData, stddevData)
      names(a) <- c("measurement", "subjectId", "mean", "stddev")
      ## todo: this should be very slow
      result <<- rbind(result, a)
    }    
  }
  
  #########################
  ### main program flow ###
  #########################
  
  downloadDataIfNecessary()
  # master <<- buildMasterData()
    
  buildMainDataSet()
    
}

message("analyzing data (downloading if necessary)...")
init()
message("done.")

# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

