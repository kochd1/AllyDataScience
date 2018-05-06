#indicator «prevalence of study participants»

# Load library
library(jsonlite)

studyData <- read_json("study_fullExport (dummy).json", simplifyVector = TRUE)

# Get all patients which submitted at least one symptom during the last 30 days.

activeStudyParticipants <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')

#Count all these patients
NumberOfActiveStudyParticipants <- 10

NumberOfActiveStudyParticipantsWithSymptom <- 5

#Calculate this indicator
NumberOfActiveStudyParticipants / NumberOfActiveStudyParticipantsWithSymptom