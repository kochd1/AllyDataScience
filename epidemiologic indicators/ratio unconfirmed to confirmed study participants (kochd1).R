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

# Get all unique allergy to pollen obs. (incl. symptoms value==0)
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


# definition of a specific timespan (month, pollenspecific period, pollen season in total etc.)

# while(date <= endDate){
#   
#   #Build subsets for each day during the timespan
#   uniqueAllergyIntolerancesPast <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$assertedDate<=date)
#   
#   #Get the number of entries
#   activeStuPartSymYes_Id_length <- length(activeStuPartSymYes_Id)
#   activeStuPart_Id_length <- length(activeStuPart_Id)
#   
#   #Save this number in a vector for each day
#   activeStuPartSymYes_IdDate_vector <- c(activeStuPartSymYes_Id_length, date)
#   activeStuPart_IdDate_vector <- c(activeStuPart_Id_length, date)
#   
#   #Combine the vectors with the predefined dataframes
#   activeStuPartSymYesIdDate_df <- rbind(activeStuPartSymYesIdDate_df, activeStuPartSymYes_IdDate_vector)
#   activeStuPartIdDate_df <- rbind(activeStuPartIdDate_df, activeStuPart_IdDate_vector)
#   
#   date <- date + 1
# }

#create specific dataframes for the daily number of "ActiveStuPart" and "ActiveStuPart[SymYes]".
uniqueAllergyIntolerancesPast_df <- data.frame(ncol= 2, byrow = TRUE)
uniqueAllergyIntolerancesUnconfirmedPast_df <- data.frame(ncol= 2, byrow = TRUE)

# create a specific dataframe for the daily number of "StuPart".
stuPartPast_df <- data.frame(ncol= 2, byrow = TRUE)

beginDate <- as.Date.character("2018-04-01")

endDate <- as.Date.character("2018-05-31")


date <- beginDate

while(date <= endDate){
  
  dateXStuPart <- as.Date(uniqueSym$effectiveDateTime, format="%Y-%m-%d")
  
  stuPartPast <- subset(uniqueSym$id, dateXStuPart <= date)
  stuPartPast_length <- length(stuPartPast)
  
  uniqueAllergyIntolerancesPast <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$assertedDate<=date)
  uniqueAllergyIntolerancesPast_length <- length(uniqueAllergyIntolerancesPast$id)
  
  if(uniqueAllergyIntolerancesPast_length==0)
  {
    date <- date +1
    str("iterate by one day!")
  }

  else{
    str("I found an entry in this month")
    
    stuPartPast_vector <- c(stuPartPast_length, as.character(date))
    stuPartPast_df <- rbind(stuPartPast_df, stuPartPast_vector)
    
    uniqueAllergyIntolerancesPast_vector <- c(uniqueAllergyIntolerancesPast_length, as.character(date))
    uniqueAllergyIntolerancesPast_df <- rbind(uniqueAllergyIntolerancesPast_df, uniqueAllergyIntolerancesPast_vector)
    
    uniqueAllergyIntolerancesUnconfirmedPast <- subset(uniqueAllergyIntolerancesUnconfirmed, uniqueAllergyIntolerancesUnconfirmed$assertedDate<=date)
    uniqueAllergyIntolerancesUnconfirmedPast_length <- length(uniqueAllergyIntolerancesUnconfirmedPast$id)
    uniqueAllergyIntolerancesUnconfirmedPast_vector <- c(uniqueAllergyIntolerancesUnconfirmedPast_length, as.character(date))
    uniqueAllergyIntolerancesUnconfirmedPast_df <- rbind(uniqueAllergyIntolerancesUnconfirmedPast_df, uniqueAllergyIntolerancesUnconfirmedPast_vector)

    date <- date +30
  }
  

}

# delete first row in matrix
uniqueAllergyIntolerancesPast_df <- uniqueAllergyIntolerancesPast_df[-1,]
uniqueAllergyIntolerancesUnconfirmedPast_df <- uniqueAllergyIntolerancesUnconfirmedPast_df[-1,]
stuPartPast_df <- stuPartPast_df[-1,]

# name the columns
colnames(stuPartPast_df) <- c("sumStuPart", "Date")
colnames(uniqueAllergyIntolerancesPast_df) <- c("sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]", "Date")
colnames(uniqueAllergyIntolerancesUnconfirmedPast_df) <- c("sumStuPart[AllergyIntolerance[Unconfirmed]", "Date")

# convert the values of the columns to integers
stuPartPast_df$sumStuPart <- as.integer(stuPartPast_df$sumStuPart)
uniqueAllergyIntolerancesPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]` <- as.integer(uniqueAllergyIntolerancesPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`)
uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]` <- as.integer(uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

# Calculation
ratioAllergyIntoleranceUnconfirmedToSumStuPart <- sumStuPartAllergyIntoleranceUnconfirmed / sumStuPart
ratioAllergyIntoleranceToSumStuPart <- sumStuPartAllergyIntolerance / sumStuPart

indicator <- ratioAllergyIntoleranceUnconfirmedToSumStuPart / ratioAllergyIntoleranceToSumStuPart

# Visualization

library(ggplot2)

nbrOfStuPart <- nrow(stuPartPast_df)
months <- c(1:nbrOfStuPart)

#V1
stuPartAllergyIntolerance <-c(uniqueAllergyIntolerancesPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`
                              -uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

stuPartAllergyIntoleranceUnconfirmed <- c(uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

values <- c(stuPartAllergyIntolerance, stuPartAllergyIntoleranceUnconfirmed)

type <- c(rep("StuPart[allergyIntolerance = unconfirmed|confirmed]", nbrOfStuPart), rep("StuPart[allergyIntolerance = unconfirmed]", nbrOfStuPart))
data <- data.frame(months, values)


p <- ggplot(data, aes(months, values))
p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Months") + ylab("StuPart confirmed/unconfirmed %") + 
  ggtitle("Ratio unconfirmed to conf./unconf. allergy sufferers") + labs(fill= "") + theme_bw() + scale_y_continuous(labels=scales::percent)

#V2
stuPart <- c(stuPartPast_df$sumStuPart
             -uniqueAllergyIntolerancesPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`
             -uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

#stuPartAllergyIntolerance <-c(uniqueAllergyIntolerancesPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`
                              #-uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

#stuPartAllergyIntoleranceUnconfirmed <- c(uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

values <- c(stuPart, stuPartAllergyIntolerance, stuPartAllergyIntoleranceUnconfirmed)

type <- c(rep("StuPart", nbrOfStuPart), rep("StuPart[allergyIntolerance = unconfirmed|confirmed]", nbrOfStuPart), rep("StuPart[allergyIntolerance = unconfirmed]", nbrOfStuPart))
data <- data.frame(months, values)


p <- ggplot(data, aes(months, values))
p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Months") + ylab("StuPart unconfirmed/conf.unconf./StuPart %") + 
  ggtitle("Ratio allergy sufferers of all StuPart") + labs(fill= "") + theme_bw() + scale_y_continuous(labels=scales::percent)


# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title
