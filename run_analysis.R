library(data.table)

#dt_outTest <- data.table()

dt_features <- data.table::fread("./UCI HAR Dataset/features.txt") #561 variables in dt_test/dt_train names
dt_activity_labels <- data.table::fread("./UCI HAR Dataset/activity_labels.txt")
names(dt_activity_labels) <- c('activityID','activity')


#get the column numbers for column names that are either std or mean
v_featureCols <- sort(c(
  dt_features[V2 %like% "mean\\("]$V1
  ,dt_features[V2 %like% "std\\("]$V1
))

newColNames <- dt_features[dt_features$V1 %in% v_featureCols,V2]
n <- length(newColNames) + 2


fn_getTest <- function() {
  
  df_subject_test <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header=F)
  length(unique(df_subject_test$V1)) #9 test subjects
  
  # df_subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header=F)
  # length(unique(df_subject_train$V1)) #21 train subjects
  
  df_y_test <- read.csv("./UCI HAR Dataset/test/Y_test.txt", header=F) # this is the activity by the subject
  # unique(df_y_test$V1)
  #[1] 5 4 6 1 3 2
  
  #df_y_train <- read.csv("./UCI HAR Dataset/train/Y_train.txt", header=F) # this is the activity by the subject
  
  dt_test <- data.table::fread("./UCI HAR Dataset/test/X_test.txt")
  
  
  out <- df_subject_test
  names(out)[1] <- "subjectID"
  out$activityID <- df_y_test$V1
  
  # v_mean <- dt_features[V2 %like% "mean\\("]$V1 # measurements for mean()
  # v_std <- dt_features[V2 %like% "std\\("]$V1 # measurements for std()
  
  #get the column numbers for dt_test that are either std or mean
  # v_featureCols <- sort(c(
  #   dt_features[V2 %like% "mean\\("]$V1
  #   ,dt_features[V2 %like% "std\\("]$V1
  # ))
  
  dt_test_stdMean <- dt_test[,..v_featureCols]
  df_outTest <- merge(out,dt_test_stdMean,by="row.names")
  df_outTest$Row.names <- NULL
  n <- length(newColNames) + 2
  names(df_outTest)[3:n] <- newColNames
  
  df_outTest <- merge(df_outTest,dt_activity_labels)
  dt_outTest <- df_outTest
  setDT(dt_outTest)
  dt_outTest <- dplyr::arrange(dt_outTest,subjectID)
  # 
  # rm(df_outTest)
  # rm(df_subject_test)
  # rm(df_y_test)
  # rm(dt_test_stdMean)
  # rm(dt_test)
  # rm(out)
  # rm(n)
  return (dt_outTest)
  
}

fn_getTrain <- function() {
 
  df_subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header=F)
  # length(unique(df_subject_train$V1)) #21 train subjects
  
  df_y_train <- read.csv("./UCI HAR Dataset/train/Y_train.txt", header=F) # this is the activity by the subject
 
  dt_train <- data.table::fread("./UCI HAR Dataset/train/X_train.txt")
  
  out <- df_subject_train
  names(out)[1] <- "subjectID"
  out$activityID <- df_y_train$V1
  
  dt_train_stdMean <- dt_train[,..v_featureCols]
  df_outTrain <- merge(out,dt_train_stdMean,by="row.names")
  df_outTrain$Row.names <- NULL
  names(df_outTrain)[3:n] <- newColNames
  
  df_outTrain <- merge(df_outTrain,dt_activity_labels)
  dt_outTrain <- df_outTrain
  setDT(dt_outTrain)
  dt_outTrain <- dplyr::arrange(dt_outTrain,subjectID)
  return (dt_outTrain)
  
}


dt_outTest <- fn_getTest() #data table for Test subjects
dt_outTrain <- fn_getTrain() # data table for Train subjects 

dt_all <-  merge(dt_outTest, dt_outTrain, all = T) #outer join

rm(dt_activity_labels,dt_features,n,v_featureCols,newColNames)
rm(dt_outTest,dt_outTrain)

# names(dt_all)[3:68]
dt_melt <- melt(dt_all,id=c("subjectID","activityID","activity"), measure.vars=3:68)
# unique(dt_melt$variable)
#dt_melt$subjectActivity <- paste(dt_melt$subjectID,dt_melt$activityID,dt_melt$activity)
dt_melt$subjectActivity <- paste(dt_melt$subjectID,dt_melt$activity)
dt_mean <- dcast(dt_melt, subjectActivity ~ variable, mean)

# confirm a few of the variables manually 
# h <- dt_all[dt_all$subjectID == 10 & dt_all$activityID == 1,"tBodyAcc-mean()-Y"]
# mean(h$`tBodyAcc-mean()-X`)

dt_mean$subjectID <- as.numeric(gsub(" .+$","",dt_mean$subjectActivity))
dt_mean$activity <- gsub("^.+ ","", dt_mean$subjectActivity)
dt_mean$subjectActivity <- NULL 

dt_mean <- dt_mean[,c(67:68,1:66)]
dt_mean <- plyr::arrange(dt_mean,subjectID,activity)

Outfn = "./tidy_means.csv"
if(file.exists(Outfn)) {file.remove(Outfn)}
write.csv(dt_mean,Outfn, row.names = F)
