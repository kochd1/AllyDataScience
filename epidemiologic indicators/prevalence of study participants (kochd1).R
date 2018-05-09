#indicator «prevalence of study participants»

# Load library
library(jsonlite)

#prepare study data

#options(stringsAsFactors=FALSE)

#studyData <- read_json("study_fullExport (dummy).json", simplifyVector = TRUE)

study_df <- data.frame(read_json("study_fullExport (dummy).json", simplifyVector= TRUE)) #2x practitioner, 11x patient, 36x observation -> 49 obs.

#a subset of all observations (with and without symptoms) -> 36 obs.
observations <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation')

#auch nach typ der Observation filtern!
x <- observations$code$coding

y <- matrix(unlist(sapply(x, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Get Code an Display
y[,2:3]
y[,2]

# Subset only allergy-to-pollen Observation with Coding 300910009
allergy_to_pollen_df <- subset(observations, y[,2] == 300910009)
#class(allergy_to_pollen_df)
  
#Observations last 30 days -> with/without symptoms! (Only one per study participant counted)

#filter by day in a period of e.g. 30 days, not only in the last 30 days
listOfDataFrames <- list()

for(i in 1:30){
  observationsOnDayX <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$effectiveDateTime == Sys.Date()-i)
  obsSym_df <- data.frame(observationsOnDayX)
  listOfDataFrames[[paste0("dataframe: ", i)]] <- obsSym_df
  #str(obsSym)
}

class(listOfDataFrames$`dataframe: 1`)

listOfObservationsPerDay <- list()
nbrOfObservationsPerDay <- length(listOfDataFrames[29]$`dataframe: 29`$id) 

for(i in 1:30){

nbrOfObservationsPerDay <- length(listOfDataFrames[i])$   #$`dataframe: 1`$id)

listOfObservationsPerDay[[paste0("Number of Observations: ", nbrOfObservationsPerDay)]]
}






observationsSymPeriod <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$effectiveDateTime >= Sys.Date()-30) #alternative -> >="YYYY-MM-DD"
#class(observationsSymPeriod)

StuPartSymEntry <- data.frame(observationsSymPeriod$subject$reference)

# get number of active study participants with/without symptoms
nbrStuPartSymEntry <- length(StuPartSymEntry$observationsSymPeriod.subject.reference)
str(nbrStuPartSymEntry)

#zähler

#a subset of all observations (only with symptoms) -> 28 obs.
observationsSymYes <- subset(allergy_to_pollen_df, allergy_to_pollen_df$valueQuantity>0)

#Observations last 30 days -> only with symptoms! (Only one per study participant counted)
observationsSymYesPeriod <- subset(allergy_to_pollen_df, allergy_to_pollen_df$valueQuantity>0 & !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$effectiveDateTime >= Sys.Date()-30)

StuPartSymYesEntry <- data.frame(observationsSymYesPeriod$subject$reference)

# get number of active study participants with symptoms
nbrStuPartSymYesEntry <- length(StuPartSymYesEntry$observationsSymYesPeriod.subject.reference)
str(nbrStuPartSymYesEntry)

#calculate ratio of stuPart with a symptom in relation with all stuPart
result <- nbrStuPartSymYesEntry / nbrStuPartSymEntry
str(result)


#visualization example
x <- c(result, 0.8, 0.75)

barplot(x)
lines(x)

