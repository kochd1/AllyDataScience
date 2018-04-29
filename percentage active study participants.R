#indicator «percentage active study participants»

#does not work correctly at the moment!

# Load library
library(jsonlite)

studyData <-read_json("study_fullExport (dummy).json", simplifyVector = TRUE)

#ally_df <- data.frame(studyData)

#ally_df

studyData$entry$resource$resourceType
