# Read in the data from files
features     = read.table('./features.txt',header=FALSE); 
activityType = read.table('./activity_labels.txt',header=FALSE); 
subjectTrain = read.table('./train/subject_train.txt',header=FALSE);
x       = read.table('./train/x_train.txt',header=FALSE);
y       = read.table('./train/y_train.txt',header=FALSE);

# Assigin column names to the data imported above
columns(activityType)  = c('activityId','activityType');
columns(subjectTrain)  = "subjectId";
columns(x)        = features[,2]; 
columns(y)        = "activityId";

# cCreate the final training set by merging y, subjectTrain, and x
trainingData = cbind(y,subjectTrain,x);

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
columns(subjectTest) = "subjectId";
columns(xTest)       = features[,2]; 
columns(yTest)       = "activityId";


# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);


# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData);

# Create a vector for the column names from the finalData, which will be used
columns  = columns(finalData); 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) & !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | grepl("-std..",columns) & !grepl("-std()..-",columns));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Update the columns vector to include the new column names after merge
columns  = columns(finalData); 

# Clean up the variable names
for (i in 1:length(columns)) 
{
  columns[i] = gsub("\\()","",columns[i])
  columns[i] = gsub("-std$","StdDev",columns[i])
  columns[i] = gsub("-mean","Mean",columns[i])
  columns[i] = gsub("^(t)","time",columns[i])
  columns[i] = gsub("^(f)","freq",columns[i])
  columns[i] = gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] = gsub("[Gg]yro","Gyro",columns[i])
  columns[i] = gsub("AccMag","AccMagnitude",columns[i])
  columns[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] = gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] = gsub("GyroMag","GyroMagnitude",columns[i])
};

# Reassigning the new descriptive column names to the finalData set
columns(finalData) = columns;

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');