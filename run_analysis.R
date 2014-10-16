suppressPackageStartupMessages(library(data.table))

init <- function() {
  
  dataFolder <- "UCI HAR Dataset"
  
  ####################################
  # Define methods used by main flow #
  ####################################
  
  # download the data set "Human Activity Recognition Using Smartphones Data Set" 
  # if it doesn't exist
  downloadDataIfNecessary <- function() {
    zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipFile <- "Dataset.zip"
    
    if(!file.exists(zipFile)) { # skip download if we already have the zip file
      message("Downloading data the \"Human Activity Recognition Using Smartphones Data Set\"...")
      download.file(zipUrl, destfile=zipFile, method="curl")
      unzip(zipFile)
      if(!file.exists(dataFolder)) { # this should never happen.  Just make sure we have the data folder
        stop("Error: expected data folder ", dataFolder)
      }
      message("data extracted")
    }
  }
    
  # helper function to paste strings together without decoration
  # useful for constructing data paths.
  concat <- function(x) {
    paste(x, collapse = '')
  }
    
  # return the subjects for the experiment, since 
  buildSubjects <- function() {
    
    # Function to load an individual subject file.
    # The orginal experiment used 30 subjects (people).
    # We identify them by ordinal numbers 1 - 30.
    buildSubjectsForFile <- function(fileSpec) {
      subjectFile <- concat(c(dataFolder, fileSpec))
      subjects <- read.csv(subjectFile, header = FALSE)    
      names(subjects) <- "subjectId"
      subjects      
    }
    
    testSubject <- buildSubjectsForFile("/test/subject_test.txt")
    trainSubject <- buildSubjectsForFile("/train/subject_train.txt")
    rbind(trainSubject, testSubject)    
  }
    
  # This function satisfies the following requirements for the project:
  #    1) Merges the training and the test sets to create one data set.
  #        We ignore the raw inertia signals files and instead use the prebuilt
  #        data sets in the files X_train.txt and X_test.txt.
  #    2) Extracts only the measurements on the mean and standard deviation for each measurement. 
  #    3) Uses descriptive activity names to name the activities in the data set
  #        The activity names come from the file activity_labels.txt.
  #    4) Appropriately labels the data set with descriptive variable names. 
  #        The data set data columns come from the file features.txt.
  #
  #    Refer to the code book for detailed information
  buildMasterData <- function() {
        
    # function to load the activity labels for the train and test data set.
    # The data sets are not labeled with the activity, so we need to match
    # the list of activity ids with the data set.  The list of activity ids
    # has the same number of rows as the data set, so we just match them in
    # sequence order.
    buildActivityLabels <- function() {
      
      # read the descriptive list of unique activities (e.g. Walking)
      activityLabelsFile <- concat(c(dataFolder, "/activity_labels.txt"))
      activityLabels <- fread(activityLabelsFile, sep = " ", header = FALSE)
      setkey(activityLabels, V1)
      
      # function to read the list of activity id's for a data set and
      # add the descriptive activity name.  
      buildActivityLabel <- function(fileName) {
        alabels <- fread(fileName)
        setkey(alabels, V1)
        dt <- merge(activityLabels, alabels) 
        setnames(dt, c("V1", "V2"), c("activityId", "activityName"))
        dt
      }
      
      # load the activities for the train and test set (adding the descriptive 
      # activity name), and merge them into a single data set.
      trainYFile <- concat(c(dataFolder, "/train/y_train.txt"))
      testYFile <- concat(c(dataFolder, "/test/y_test.txt"))
      trainingLabels <- buildActivityLabel(trainYFile) 
      testingLabels <- buildActivityLabel(testYFile) 
      rbind(trainingLabels, testingLabels)
    }
    
    # This function builds the list of features for both the train data and the test data.
    # The features are the variable names for the observations (e.g. tBodyAcc-mean())
    buildFeatures <- function() {
      featuresFile <- concat(c(dataFolder, "/features.txt"))      
      lapply(read.csv(featuresFile, header = FALSE, sep = " ")[,2], as.character)
    }  
    
    # Function to build the merged experiment data for the train data and test data.
    # The experiment data is an unlabeled table of measurements.  Observations are for 
    # a particular subject and activity.  Variables measurements for each of the features.
    buildExperimentData <- function() {
      trainXFile <- concat(c(dataFolder, "/train/X_train.txt"))
      testXFile <- concat(c(dataFolder, "/test/X_test.txt"))

      message('...loading train data...')
      trainingSet <- read.table(trainXFile, header = FALSE)

      message('...loading test data...')
      testingSet <- read.table(testXFile, header = FALSE)
      
      message('...merging data to one set...')
      rbind(trainingSet, testingSet)
    }
    
    # build the feature descriptions (e.g. tBodyAcc-mean()) and the experiment data
    features <- buildFeatures()
    fullSet <- buildExperimentData()
    
    # set the column names of the experiment data with the feature descriptions
    setnames(fullSet, names(fullSet), unlist(features))
    
    # use the feature descriptions to grab the columns that are measurements of mean
    # or standard deviation.  We don't need the other measurements.
    tdNames <- names(fullSet)
    masterSet <- fullSet[,c(grep("mean\\(\\)", tdNames), grep("std\\(\\)", tdNames))]
    
    # build the activity labels (e.g. Walking) and subjects (number from 1 to 30)
    activityLabels <- buildActivityLabels()
    subjects <- buildSubjects()
    
    # return a data table with the columns for the activity, subject, mean measurements
    # and standard deviations for each feature.
    cbind(activityLabels, subjects, masterSet)
  }
  
  # This message satisfies requirement #5
  #    From the data set in step 4, creates a second, independent tidy data 
  #    set with the average of each variable for each activity and each subject.
  buildSummaryData <- function(sourceData) {
    suppressPackageStartupMessages(library(dplyr))
    a <- group_by(sourceData, activityId, activityName, subjectId)
    suppressPackageStartupMessages(library(plyr))
    ddply(masterData, .(activityName, subjectId, activityId), numcolwise(mean))
  }
      
  #########################
  ### main program flow ###
  #########################
  
  downloadDataIfNecessary()
  message("building masterData...")
  masterData <<- buildMasterData()
  message("building summaryData...")
  summaryData <<- buildSummaryData(masterData)
  message("saving summardData in the file summaryData.rtbl...")
  write.table(summaryData, file="summaryData.rtbl", row.name=FALSE)
}

message("analyzing data (downloading if necessary)...")
init()
message("done.")

