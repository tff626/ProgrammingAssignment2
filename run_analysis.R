# 1. Merges the training and the test sets to create one data set.
## Step 1. Create train table
subject_train <- read.table("getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\subject_train.txt")
Y_train <- read.table("getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\y_train.txt")
X_train <- read.table("getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt")
train_table <- cbind(subject_train,Y_train,X_train)

## Step 2. Create test table
subject_test <- read.table("getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\subject_test.txt")
Y_test <- read.table("getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\y_test.txt")
X_test <- read.table("getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt")
test_table <- cbind(subject_test,Y_test,X_test)

## Step 3. Combine(append) train and test tables
myData <- rbind(train_table,test_table)

## Step 4. Join labels
labels <- read.table("getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\features.txt")
labels <- as.vector(labels[,2])
first_second_names <- c("subject","activity")
labels <- append(first_second_names,labels)
colnames(myData) <- labels


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
is.mean <- grepl("-mean()", labels, fixed = TRUE)
is.std <- grepl("-std()", labels, fixed = TRUE)
subData <- myData[is.mean==TRUE|is.std==TRUE]


# 3. Uses descriptive activity names to name the activities in the data set
subject_activity <- myData[,1:2]
subData <- cbind(subject_activity, subData)


# 4. Creates a second, independent tidy data set with the average of each variable 
# for each activity and each subject. 
# Step 1. split data by subjects
sample = split(subData, subData$subject)
# Step 2. create index numbers for subject and activity id
subject_id <- gl (30,6)
activity_index <- c(rep(1:6, 30))
a <- rbind(subject_id, activity_index)
agg2 <- vector()
# Step 3. use tapply function to calculate mean for each activity in a given subject
for (i in 1:30) {
  try_agg <- vector()
  for (k in 1:66) {
  try_temp <- tapply(sample[[i]][,k+2], sample[[i]][,2], mean)
  try_agg <- rbind(try_agg,try_temp)
 }
  agg2<-cbind(agg2,try_agg)
}
# Step 4. combine subject and activity indexes with means
a <- rbind(a,agg2)
a <- t(a)
colnames(a) <- colnames(subData)

# 5. Appropriately labels the data set with descriptive activity names.
subData$activity[subData$activity == 1] <- "WALKING"
subData$activity[subData$activity == 2] <- "WALKING_UPSTAIRS"
subData$activity[subData$activity == 3] <- "WALKING_DOWNSTAIRS"
subData$activity[subData$activity == 4] <- "SITTING"
subData$activity[subData$activity == 5] <- "STANDING"
subData$activity[subData$activity == 6] <- "LAYING"



#6. Write data into a .txt file
write.table(a, file = "./tidydata.txt", sep="\t",row.names = FALSE)



