# Author: Dominik R. Kocher (kochd1)

# Script for the indicator «Ratio unconfirmed to confirmed study participants»

# Load libraries
library(jsonlite)
library(ggplot2)

### 1 - INPUT ###

# Name of the JSON-File (studydata)
inputFile <- "study_fullExport (dummy).json"

# Date of beginning as YYYY-MM-DD
beginDate <- as.Date.character("2018-04-01") # Fill in

# Date of end as YYYY-MM-DD
endDate <- as.Date.character("2018-05-31") # Fill in

### 2 - DATA IMPORT ###

study_df <- data.frame(read_json(inputFile, simplifyDataFrame = TRUE))

### 3 - GET DATA FOR THE INDICATOR ###

# Creation of a subset of all observations
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

# Create specific dataframes for the daily number of "ActiveStuPart" and "ActiveStuPart[SymYes]".
uniqueAllergyIntolerancesByNow_df <- data.frame(ncol= 2, byrow = TRUE)
uniqueAllergyIntolerancesUnconfirmedByNow_df <- data.frame(ncol= 2, byrow = TRUE)

# Create a specific dataframe for the daily number of "StuPart".
stuPartByNow_df <- data.frame(ncol= 2, byrow = TRUE)

date <- beginDate

while(date <= endDate){
  
  dateXStuPart <- as.Date(uniqueSym$effectiveDateTime, format="%Y-%m-%d")
  
  stuPartByNow <- subset(uniqueSym$id, dateXStuPart <= date)
  stuPartByNow_length <- length(stuPartByNow)
  
  uniqueAllergyIntolerancesByNow <- subset(uniqueAllergyIntolerances, uniqueAllergyIntolerances$assertedDate<=date)
  uniqueAllergyIntolerancesByNow_length <- length(uniqueAllergyIntolerancesByNow$id)
  
  if(uniqueAllergyIntolerancesByNow_length==0)
  {
    date <- date +1
    str("iterated by one day!")
  }

  else{
    str("entry found in this month")
    
    stuPartByNow_vector <- c(stuPartByNow_length, as.character(date))
    stuPartByNow_df <- rbind(stuPartByNow_df, stuPartByNow_vector)
    
    uniqueAllergyIntolerancesByNow_vector <- c(uniqueAllergyIntolerancesByNow_length, as.character(date))
    uniqueAllergyIntolerancesByNow_df <- rbind(uniqueAllergyIntolerancesByNow_df, uniqueAllergyIntolerancesByNow_vector)
    
    uniqueAllergyIntolerancesUnconfirmedByNow <- subset(uniqueAllergyIntolerancesUnconfirmed, uniqueAllergyIntolerancesUnconfirmed$assertedDate<=date)
    uniqueAllergyIntolerancesUnconfirmedByNow_length <- length(uniqueAllergyIntolerancesUnconfirmedByNow$id)
    uniqueAllergyIntolerancesUnconfirmedByNow_vector <- c(uniqueAllergyIntolerancesUnconfirmedByNow_length, as.character(date))
    uniqueAllergyIntolerancesUnconfirmedByNow_df <- rbind(uniqueAllergyIntolerancesUnconfirmedByNow_df, uniqueAllergyIntolerancesUnconfirmedByNow_vector)

    date <- date +30
    str(date)
  }
  

}

# delete first row in matrix
uniqueAllergyIntolerancesByNow_df <- uniqueAllergyIntolerancesByNow_df[-1,]
uniqueAllergyIntolerancesUnconfirmedByNow_df <- uniqueAllergyIntolerancesUnconfirmedByNow_df[-1,]
stuPartByNow_df <- stuPartByNow_df[-1,]

# name the columns
colnames(stuPartByNow_df) <- c("sumStuPart", "Date")
colnames(uniqueAllergyIntolerancesByNow_df) <- c("sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]", "Date")
colnames(uniqueAllergyIntolerancesUnconfirmedByNow_df) <- c("sumStuPart[AllergyIntolerance[Unconfirmed]", "Date")

# convert the values of the columns to integers
stuPartByNow_df$sumStuPart <- as.integer(stuPartByNow_df$sumStuPart)
uniqueAllergyIntolerancesByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]` <- as.integer(uniqueAllergyIntolerancesByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`)
uniqueAllergyIntolerancesUnconfirmedByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed]` <- as.integer(uniqueAllergyIntolerancesUnconfirmedByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

### 4 - CALCULATION ###
ratioAllergyIntoleranceUnconfirmedToSumStuPart <- sumStuPartAllergyIntoleranceUnconfirmed / sumStuPart
ratioAllergyIntoleranceToSumStuPart <- sumStuPartAllergyIntolerance / sumStuPart

indicator <- ratioAllergyIntoleranceUnconfirmedToSumStuPart / ratioAllergyIntoleranceToSumStuPart

### 5 - VISUALIZATION ###

nbrOfStuPart <- nrow(stuPartByNow_df)
months <- c(1:nbrOfStuPart)


# beginDateGraph <- as.Date(beginDate, format="%Y-%m")
# endDateGraph <- as.Date(endDate, format="%Y-%m")

#V1
stuPartAllergyIntolerance <-c(uniqueAllergyIntolerancesByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`
                              -uniqueAllergyIntolerancesUnconfirmedByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

stuPartAllergyIntoleranceUnconfirmed <- c(uniqueAllergyIntolerancesUnconfirmedByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

values <- c(stuPartAllergyIntolerance, stuPartAllergyIntoleranceUnconfirmed)

type <- c(rep("StuPart[allergyIntolerance = unconfirmed|confirmed]", nbrOfStuPart), rep("StuPart[allergyIntolerance = unconfirmed]", nbrOfStuPart))
data <- data.frame(months, values)


p <- ggplot(data, aes(months, values))
p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Month(s)") + ylab("StuPart confirmed/unconfirmed %") + 
  ggtitle(paste("Ratio unconfirmed to conf./unconf. allergy sufferers ", "(from ", beginDate, " to ", endDate, ")", sep="")) + labs(fill= "") + theme_bw() + scale_x_continuous(breaks = seq(0, nbrOfStuPart, by = 1)) + scale_y_continuous(labels=scales::percent)

#V2
stuPart <- c(stuPartByNow_df$sumStuPart
             -uniqueAllergyIntolerancesByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`
             -uniqueAllergyIntolerancesUnconfirmedByNow_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

#stuPartAllergyIntolerance <-c(uniqueAllergyIntolerancesPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed|Confirmed]`
                              #-uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

#stuPartAllergyIntoleranceUnconfirmed <- c(uniqueAllergyIntolerancesUnconfirmedPast_df$`sumStuPart[AllergyIntolerance[Unconfirmed]`)

values <- c(stuPart, stuPartAllergyIntolerance, stuPartAllergyIntoleranceUnconfirmed)

type <- c(rep("StuPart", nbrOfStuPart), rep("StuPart[allergyIntolerance = unconfirmed|confirmed]", nbrOfStuPart), rep("StuPart[allergyIntolerance = unconfirmed]", nbrOfStuPart))
data <- data.frame(months, values)


p <- ggplot(data, aes(months, values))
p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Month(s)") + ylab("StuPart unconfirmed/conf.unconf./StuPart %") + 
  ggtitle(paste("Ratio allergy sufferers of all StuPart ", "(from ", beginDate, " to ", endDate, ")", sep="")) + labs(fill= "") + theme_bw() + scale_x_continuous(breaks = seq(0, nbrOfStuPart, by = 1)) + scale_y_continuous(labels=scales::percent)


# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title
