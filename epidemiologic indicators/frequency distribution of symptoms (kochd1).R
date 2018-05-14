#kochd1

#script for the indicator "frequency distribution of symptoms"

# Load library
library(jsonlite)

#prepare study data

#options(stringsAsFactors=FALSE)

#studyData <- read_json("study_fullExport (dummy).json", simplifyVector = TRUE)

study_df <- data.frame(read_json("study_fullExport (dummy).json", simplifyVector= TRUE)) #2x practitioner, 11x patient, 36x observation -> 49 obs.

#-------------------------------------------------------------



#a subset of all observations
observations <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation')

#auch nach typ der Observation filtern!
ObsType <- observations$code$coding

y <- matrix(unlist(sapply(ObsType, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Get Code an Display
#y[,2:3] #text and code
y[,2]

# Subset only allergy-to-pollen Observations with Coding 300910009
allergy_to_pollen_df <- subset(observations, y[,2] == 300910009)
#class(allergy_to_pollen_df)

uniqueSymYes <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$valueQuantity>0)

#filter all stuPart with nose symptoms

#auch nach typ der Observation filtern!
bodySiteCoding <- uniqueSymYes$bodySite$coding

bodySiteCodes <- matrix(unlist(sapply(bodySiteCoding, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Get Code an Display
#y[,2:3] #text and code
bodySiteCodes[,2]

noseSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 45206002)
eyeSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 81745001)
skinSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 39937001)
lungSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 39607008)
