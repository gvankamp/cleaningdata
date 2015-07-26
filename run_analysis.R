'Extract and merge R data'
'============================'

#This scripts assumes, the packages 'dplyr' and 'tidyr' are loaded

#read features, set to lower-case
features <- read.table("features.txt", quote = "\"", comment.char = "", stringsAsFactors = F)
features <- lapply(features, tolower)

#read names
subject_test <-  read.table("subject_test.txt", quote = "\"", comment.char = "", stringsAsFactors = F)
subject_train <-  read.table("subject_train.txt", quote = "\"", comment.char = "", stringsAsFactors = F)
activity_labels <- read.table("activity_labels.txt", quote = "\"", comment.char = "", stringsAsFactors = T)


# read actual data
y_test <- read.table("y_test.txt", quote = "\"", comment.char = "")
x_test <- read.table("X_test.txt", quote = "\"", comment.char = "")
y_train <- read.table("y_train.txt", quote = "\"", comment.char = "")
x_train <- read.table("X_train.txt", quote = "\"", comment.char = "")

#make a testing and training data set
testData <- cbind(subject_test, y_test, x_test)
trainData <- cbind(subject_train, y_train, x_train)

#adapt column names
columnNames <- c("subject", "activity", features$V2)
colnames(testData) <- columnNames
colnames(trainData) <- columnNames

#combine testing and training data into one dataset
rbind(testData, trainData) %>%
gather(var.name, measurement, -subject, -activity) -> data

#add nice activity labels to the dataset instead of just numbers
colnames(activity_labels) <- c("activity.code", "activity")
activity_labels$activity <- lapply(activity_labels$activity, tolower)

#activity_labels$activity <- factor("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")

merge(activity_labels, data, by.x = "activity.code", by.y = "activity") -> data

#make a dataset with only standard deviation and means
filter(data, grepl("std|mean", var.name)) -> reduced.data

reduced.data %>% 
group_by(subject, var.name) %>%
  summarise(subject.average = mean(measurement)) -> subjectAverage


reduced.data %>% group_by(var.name, activity.code) %>%
  summarise(activity.average = mean(measurement)) -> activityAverage

final.data <- merge(activityAverage, subjectAverage, by.x = 'var.name', by.y = 'var.name')