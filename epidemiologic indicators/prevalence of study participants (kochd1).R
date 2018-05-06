#indicator «prevalence of study participants»

# Load library
library(jsonlite)

#prepare study data

#options(stringsAsFactors=FALSE)

#studyData <- read_json("study_fullExport (dummy).json", simplifyVector = TRUE)

study_df <- data.frame(read_json("study_fullExport (dummy).json", simplifyVector= TRUE)) #2x practitioner, 11x patient, 36x observation -> 49 obs.

#a subset of all observations (with and without symptoms) -> 36 obs.
observationsSym <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation')

#Observations last 30 days -> with/without symptoms! (Only one per study participant counted)
observationsSymPeriod <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation' & !duplicated(study_df$entry.resource$subject$reference) & study_df$entry.resource$effectiveDateTime >= Sys.Date()-30) #alternative -> >="YYYY-MM-DD"

StuPartSymEntry <- data.frame(observationsSymPeriod$subject$reference)

# get number of active study participants with/without symptoms
nbrStuPartSymEntry <- length(StuPartSymEntry$observationsSymPeriod.subject.reference)
str(nbrStuPartSymEntry)


#a subset of all observations (only with symptoms) -> 28 obs.
observationsSymYes <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation' & study_df$entry.resource$valueQuantity >0)

#Observations last 30 days -> only with symptoms! (Only one per study participant counted)
observationsSymYesPeriod <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation' & study_df$entry.resource$valueQuantity >0 & !duplicated(study_df$entry.resource$subject$reference) & study_df$entry.resource$effectiveDateTime >= Sys.Date()-30)

StuPartSymYesEntry <- data.frame(observationsSymYesPeriod$subject$reference)

# get number of active study participants with symptoms
nbrStuPartSymYesEntry <- length(StuPartSymYesEntry$observationsSymYesPeriod.subject.reference)
str(nbrStuPartSymYesEntry)

#calculate ratio of stuPart with a symptom in relation with all stuPart
nbrStuPartSymYesEntry / nbrStuPartSymEntry


