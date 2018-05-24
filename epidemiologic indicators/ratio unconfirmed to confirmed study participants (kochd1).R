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

# get all StuPart with an Allergy Intolerance (regardless of the type)
uniqueAllergyIntolerances <- subset(allergyIntolerance_df, !duplicated(allergyIntolerance_df$patient$reference))

uniqueAllergyIntolerancesConfirmed <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$verificationStatus =='confirmed')
uniqueAllergyIntolerancesUnconfirmed <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$verificationStatus =='unconfirmed')

sumStuPartAllergyIntolerance <- length(uniqueAllergyIntolerances$id)

sumStuPartAllergyIntoleranceConfirmed <- length(uniqueAllergyIntolerancesConfirmed$id)

sumStuPartAllergyIntoleranceUnconfirmed <- length(uniqueAllergyIntolerancesUnconfirmed$id)

#TODO: filter one day of a month

# definition of a specific timespan (month, pollenspecific period, pollen season in total etc.)
beginDate <- as.Date.character("2018-04-01")

endDate <- as.Date.character("2018-05-31")



date <- beginDate

while(date <= endDate){
  
  uniqueAllergyIntolerancesPast <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$assertedDate<=date)
  
  if(length(uniqueAllergyIntolerancesPast$id)==0)
  {
    date <- date +1
    str("iterate by one day!")
  }
  
  else{
    str("I found an entry in this month")
    uniqueAllergyIntolerancesUnconfirmedPast <- subset(uniqueAllergyIntolerancesUnconfirmed, uniqueAllergyIntolerancesUnconfirmed$assertedDat<=date)
    
    date <- date +30
  }
  
  

  
  # #Get the number of entries
  stuPart_Id_length <- length(activeStuPartSymYes_Id)
  # activeStuPart_Id_length <- length(activeStuPart_Id)
  # 
  # #Save this number in a vector for each day
  # activeStuPartSymYes_IdDate_vector <- c(activeStuPartSymYes_Id_length, date)
  # activeStuPart_IdDate_vector <- c(activeStuPart_Id_length, date)
  # 
  # #Combine the vectors with the predefined dataframes
  # activeStuPartSymYesIdDate_df <- rbind(activeStuPartSymYesIdDate_df, activeStuPartSymYes_IdDate_vector)
  # activeStuPartIdDate_df <- rbind(activeStuPartIdDate_df, activeStuPart_IdDate_vector)
  
}

# Calculation
ratioAllergyIntoleranceUnconfirmedToSumStuPart <- sumStuPartAllergyIntoleranceUnconfirmed / sumStuPart
ratioAllergyIntoleranceToSumStuPart <- sumStuPartAllergyIntolerance / sumStuPart

indicator <- ratioAllergyIntoleranceUnconfirmedToSumStuPart / ratioAllergyIntoleranceToSumStuPart

# Visualization

#nbrOfActiveStuPart <- nrow(StuPartIdDate_df)
months <- c(1:3) #c(activeStuPartIdDate_df$Date)

stuPart <- c(stuPartIdDate_df$sumActiveStuPart-activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]`) #subtraction necessary due to correct visualization
activeStuPartSymYes <-c(activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]`)

values <- c(activeStuPart, activeStuPartSymYes)

type <- c(rep("StuPart", nbrOfStuPart), rep("AllergyIntolerance[Unconfirmed]", nbrOfStuPart), rep("AllergyIntolerance[Unconfirmed|Confirmed]", nbrOfStuPart))
data <- data.frame(days, values)

library(ggplot2)

p <- ggplot(data, aes(days, values))
p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Months") + ylab("Prevalence in %") + 
  ggtitle("Ratio unconfirmed to conf./unconf. allergy sufferers") + labs(fill= "") + geom_hline(yintercept = meanPrevalence, size = 1.5, color="blue") + theme_bw()

# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title
