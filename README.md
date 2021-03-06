#Code Book for the "Getting and Cleaning Data Class" Project
##The Human Activity Recognition Using Smartphones Data Set 

## Study Design

The data collected in this study was downloaded from [The Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).  You can download the data from the Center for Machine Learning and Intelligent Systems [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip), or download the data from this project [here](https://github.com/tflander/CourseraDataCleaningProject/blob/master/Dataset.zip).

The original data set includes files that were not used to create the summary data set for this project.  These unused files are found in the "Inertia Signals" folders in the zip file's "train" and "test" folders.  You could use the data in these folders to reconstruct the input data for this project.

The input data measurements for this project is found in the files "X\_train.txt" and "X\_test.txt".  Other files are used to give context to the measurements for subject being studied, the activity being performed, and the measurements being summarized.  For more information on the study design, see the [R script](https://github.com/tflander/CourseraDataCleaningProject/blob/master/run_analysis.R) used to create the [summary data](https://github.com/tflander/CourseraDataCleaningProject/blob/master/summaryData.rtbl.txt) resulting from this project.

Note:  I could have created a "tall table" where each row represented a smartphone function, with colunms for mean, standard deviation, subject, function name, activity name & id, and the method used for creating the summary (frequency domain signal or time domain signal).  It made more sense, however, to summarize this data as a wide table.  I presumed the next logical step in the data pipeline is to determine column-by-column if the measurement is useful for identifying the activity.  Rather than having the researcher melt the data into a wide table, I choose to submit the data already summarized in wide format.

##DATA DICTIONARY

###activityName
 The activity name.  One of:  LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, WALKING_UPSTAIRS
###subjectId
 Ordinal number from 1 - 30 representing the person being studied.
###activityId
 Ordinal number from 1 - 6 representing the activity:
- 1 WALKING
- 2 WALKING_UPSTAIRS
- 3 WALKING_DOWNSTAIRS
- 4 SITTING
- 5 STANDING
- 6 LAYING

 ```
 Note:  The following measurements are numeric values with 5 decimal places, ranging from -1 to 1:
 ```
###tBodyAcc-mean()-X
Mean of BodyAcc-X using time domain signals
###tBodyAcc-mean()-Y
Mean of BodyAcc-Y using time domain signals
###tBodyAcc-mean()-Z
Mean of BodyAcc-Z using time domain signals
###tGravityAcc-mean()-X
Mean of GravityAcc-X using time domain signals
###tGravityAcc-mean()-Y
Mean of GravityAcc-Y using time domain signals
###tGravityAcc-mean()-Z
Mean of GravityAcc-Z using time domain signals
###tBodyAccJerk-mean()-X
Mean of BodyAccJerk-X using time domain signals
###tBodyAccJerk-mean()-Y
Mean of BodyAccJerk-Y using time domain signals
###tBodyAccJerk-mean()-Z
Mean of BodyAccJerk-Z using time domain signals
###tBodyGyro-mean()-X
Mean of BodyGyro-X using time domain signals
###tBodyGyro-mean()-Y
Mean of BodyGyro-Y using time domain signals
###tBodyGyro-mean()-Z
Mean of BodyGyro-Z using time domain signals
###tBodyGyroJerk-mean()-X
Mean of BodyGyroJerk-X using time domain signals
###tBodyGyroJerk-mean()-Y
Mean of BodyGyroJerk-Y using time domain signals
###tBodyGyroJerk-mean()-Z
Mean of BodyGyroJerk-Z using time domain signals
###tBodyAccMag-mean()
Mean of BodyAccMag using time domain signals
###tGravityAccMag-mean()
Mean of GravityAccMag using time domain signals
###tBodyAccJerkMag-mean()
Mean of BodyAccJerkMag using time domain signals
###tBodyGyroMag-mean()
Mean of BodyGyroMag using time domain signals
###tBodyGyroJerkMag-mean()
Mean of BodyGyroJerkMag using time domain signals
###fBodyAcc-mean()-X
Mean of BodyAcc-X using frequency domain signals
###fBodyAcc-mean()-Y
Mean of BodyAcc-Y using frequency domain signals
###fBodyAcc-mean()-Z
Mean of BodyAcc-Z using frequency domain signals
###fBodyAccJerk-mean()-X
Mean of BodyAccJerk-X using frequency domain signals
###fBodyAccJerk-mean()-Y
Mean of BodyAccJerk-Y using frequency domain signals
###fBodyAccJerk-mean()-Z
Mean of BodyAccJerk-Z using frequency domain signals
###fBodyGyro-mean()-X
Mean of BodyGyro-X using frequency domain signals
###fBodyGyro-mean()-Y
Mean of BodyGyro-Y using frequency domain signals
###fBodyGyro-mean()-Z
Mean of BodyGyro-Z using frequency domain signals
###fBodyAccMag-mean()
Mean of BodyAccMag using frequency domain signals
###fBodyBodyAccJerkMag-mean()
Mean of BodyBodyAccJerkMag using frequency domain signals
###fBodyBodyGyroMag-mean()
Mean of BodyBodyGyroMag using frequency domain signals
###fBodyBodyGyroJerkMag-mean()
Mean of BodyBodyGyroJerkMag using frequency domain signals
###tBodyAcc-std()-X
Standard deviation of BodyAcc-X using time domain signals
###tBodyAcc-std()-Y
Standard deviation of BodyAcc-Y using time domain signals
###tBodyAcc-std()-Z
Standard deviation of BodyAcc-Z using time domain signals
###tGravityAcc-std()-X
Standard deviation of GravityAcc-X using time domain signals
###tGravityAcc-std()-Y
Standard deviation of GravityAcc-Y using time domain signals
###tGravityAcc-std()-Z
Standard deviation of GravityAcc-Z using time domain signals
###tBodyAccJerk-std()-X
Standard deviation of BodyAccJerk-X using time domain signals
###tBodyAccJerk-std()-Y
Standard deviation of BodyAccJerk-Y using time domain signals
###tBodyAccJerk-std()-Z
Standard deviation of BodyAccJerk-Z using time domain signals
###tBodyGyro-std()-X
Standard deviation of BodyGyro-X using time domain signals
###tBodyGyro-std()-Y
Standard deviation of BodyGyro-Y using time domain signals
###tBodyGyro-std()-Z
Standard deviation of BodyGyro-Z using time domain signals
###tBodyGyroJerk-std()-X
Standard deviation of BodyGyroJerk-X using time domain signals
###tBodyGyroJerk-std()-Y
Standard deviation of BodyGyroJerk-Y using time domain signals
###tBodyGyroJerk-std()-Z
Standard deviation of BodyGyroJerk-Z using time domain signals
###tBodyAccMag-std()
Standard deviation of BodyAccMag using time domain signals
###tGravityAccMag-std()
Standard deviation of GravityAccMag using time domain signals
###tBodyAccJerkMag-std()
Standard deviation of BodyAccJerkMag using time domain signals
###tBodyGyroMag-std()
Standard deviation of BodyGyroMag using time domain signals
###tBodyGyroJerkMag-std()
Standard deviation of BodyGyroJerkMag using time domain signals
###fBodyAcc-std()-X
Standard deviation of BodyAcc-X using frequency domain signals
###fBodyAcc-std()-Y
Standard deviation of BodyAcc-Y using frequency domain signals
###fBodyAcc-std()-Z
Standard deviation of BodyAcc-Z using frequency domain signals
###fBodyAccJerk-std()-X
Standard deviation of BodyAccJerk-X using frequency domain signals
###fBodyAccJerk-std()-Y
Standard deviation of BodyAccJerk-Y using frequency domain signals
###fBodyAccJerk-std()-Z
Standard deviation of BodyAccJerk-Z using frequency domain signals
###fBodyGyro-std()-X
Standard deviation of BodyGyro-X using frequency domain signals
###fBodyGyro-std()-Y
Standard deviation of BodyGyro-Y using frequency domain signals
###fBodyGyro-std()-Z
Standard deviation of BodyGyro-Z using frequency domain signals
###fBodyAccMag-std()
Standard deviation of BodyAccMag using frequency domain signals
###fBodyBodyAccJerkMag-std()
Standard deviation of BodyBodyAccJerkMag using frequency domain signals
###fBodyBodyGyroMag-std()
Standard deviation of BodyBodyGyroMag using frequency domain signals
###fBodyBodyGyroJerkMag-std()
Standard deviation of BodyBodyGyroJerkMag using frequency domain signals
