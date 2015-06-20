# getwd()
# setwd("./project/") # assumption

# Prepare constants
dataDir <- "./data/"
featuresFile <- paste(dataDir, "features.txt", sep = "")

features <- read.table(featuresFile)
featuresLabels <- c("featureId", "featureName")
colnames(features) <- featuresLabels

activityLabelsFile <- paste(dataDir, "activity_labels.txt", sep = "")
activityLabels <- read.table(activityLabelsFile)
activityLabelsLabels <- c("activityLabelId", "activityLabelName")
colnames(activityLabels) <- activityLabelsLabels

subjectFile <- paste(dataDir, "#PREFIX#/", "subject_#PREFIX#.txt", sep = "")
xFile <- paste(dataDir, "#PREFIX#/", "X_#PREFIX#.txt", sep = "")
yFile <- paste(dataDir, "#PREFIX#/", "y_#PREFIX#.txt", sep = "")

datasetTypes <- c("test", "train") 

standardColumnNames <- c("SubjectId", "Activity")
revisedColumnNames <-c(
  standardColumnNames,
  as.character(features[, featuresLabels[2]]))


# Read datasets based on types
TEST_DATA <-  data.frame(
    read.table(gsub("#PREFIX#", datasetTypes[1], subjectFile)),
    read.table(gsub("#PREFIX#", datasetTypes[1], yFile)),
    read.table(gsub("#PREFIX#", datasetTypes[1], xFile))) # test

TRAIN_DATA <-  data.frame(
    read.table(gsub("#PREFIX#", datasetTypes[2], subjectFile)),
    read.table(gsub("#PREFIX#", datasetTypes[2], yFile)),
    read.table(gsub("#PREFIX#", datasetTypes[2], xFile))) # train

# Union data
UNION_DATA <- rbind(TEST_DATA, TRAIN_DATA)
colnames(UNION_DATA) <- revisedColumnNames

# Subset for measurements of mean and standard deviation in nature
pattern <- "(mean\\(\\)|std\\(\\))"
SUBSET_DATA <- UNION_DATA[, grepl(pattern, names(UNION_DATA))]

# Rename to Activity
for (i in activityLabels[, activityLabelsLabels[1]]) {
  UNION_DATA$Activity[UNION_DATA$Activity == i] =
      as.character(
        activityLabels[activityLabels$activityLabelId == i, activityLabelsLabels[2]])
}

# Renames column names
newColumnNames <- colnames(UNION_DATA)
for (i in 1:length(newColumnNames)) {
  newColumnNames[i] = gsub("\\)$", "", newColumnNames[i])
  newColumnNames[i] = gsub("\\(\\)$", "", newColumnNames[i])
  newColumnNames[i] = gsub("\\(\\)-", ".", newColumnNames[i])
  newColumnNames[i] = gsub("[[:punct:]]", ".", newColumnNames[i])
}

# Compute mean group by SubjectId and Activity
colnames(UNION_DATA) <- newColumnNames

AGGREGATED_DATA <- 
  aggregate(
    UNION_DATA[, 3:ncol(UNION_DATA)], 
    by = list(UNION_DATA$SubjectId, UNION_DATA$Activity), 
    FUN = mean)

colnames(AGGREGATED_DATA)[1:2] <- standardColumnNames

# Write to a file
fileName <-
  paste("tidyData", format(Sys.time(), "%Y-%m-%d.%H-%M-%S"), ".csv", sep = "")
write.csv(AGGREGATED_DATA, file = fileName, row.names = FALSE);
