# author: Dominik R. Kocher (kochd1)

# script for the indicator «Ratio unconfirmed to confirmed study participants»

# Load library
library(jsonlite)

# Data preparation

study_df <- data.frame(read_json("study_fullExport (dummy).json", simplifyVector= TRUE)) #2x practitioner, 11x patient, 36x observation, 1x AllergyIntolerance -> 50 obs.

#-------------------------------------------------------------------------------------------------------------------------------------------------

# creation of a subset of all observations
observations <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation')

# Get the coding of this observation
ObsType <- observations$code$coding

# prepare it as a dataframe
colObsCoding <- matrix(unlist(sapply(ObsType, as.data.frame)),ncol=3, byrow=T)
# class(colObsCoding) -> matrix

# Get Code an Display
# colObsCoding[,2:3] #text and code
# only Code
colObsCoding[,2]

# Subset only allergy-to-pollen Observations with Coding 300910009 (Allergy to Pollen)
allergy_to_pollen_df <- subset(observations, colObsCoding[,2] == 300910009)
#class(allergy_to_pollen_df)

# Get all unique allergy to pollen obs. (inlc. symptoms value==0)
uniqueSym <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference))
sumStuPart <-length(uniqueSym$id)

# creation of a subset of all "AllergyIntolerance" resourceTypes
allergyIntolerance_df <- subset(study_df$entry.resource, study_df$entry.resource$resourceType =='AllergyIntolerance')

uniqueAllergyIntolerances <- subset(allergyIntolerance_df, !duplicated(allergyIntolerance_df$patient$reference))

uniqueAllergyIntolerancesConfirmed <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$verificationStatus=='confirmed')
uniqueAllergyIntolerancesUnonfirmed <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$verificationStatus=='unconfirmed')

#TODO: filter one day, length, calculation

# compare months -> loop must be redefined!

#  YYYY-MM
# year_month <- "2018-04"

# DD
#  days_of_month <- 30
# 
#  for(j in 1:days_of_month) {
#    if(j < 10) {
#      j <- paste("0",j, sep = "")
#    }
# 
#    date <- paste(year_month, j, sep = "-")
# 
# }

# Calculation
