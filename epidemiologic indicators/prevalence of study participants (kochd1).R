#author: Dominik R. Kocher (kochd1)

#script for the indicator «Prevalence of study participants»

# Load library
library(jsonlite)

#Data preparation

study_df <- data.frame(read_json("study_fullExport (dummy).json", simplifyVector= TRUE)) #2x practitioner, 11x patient, 36x observation -> 49 obs.

#-------------------------------------------------------------------------------------------------------------------------------------------------

#a subset of all observations (with and without symptoms) -> 36 obs.
observations <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation')

#auch nach typ der Observation filtern!
ObsType <- observations$code$coding

colObsCoding <- matrix(unlist(sapply(ObsType, as.data.frame)),ncol=3, byrow=T)
# class(colObsCoding) -> matrix

# Get Code an Display
colObsCoding[,2:3]
colObsCoding[,2]

# Subset only allergy-to-pollen Observation with Coding 300910009
allergy_to_pollen_df <- subset(observations, colObsCoding[,2] == 300910009)
#class(allergy_to_pollen_df)
  
#Get all unique allergy to pollen obs. (inlc. symptoms value==0)
uniqueSym <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference))

#Get all unique observations with a syptom (value>0)
uniqueSymYes <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$valueQuantity>0)

#create dataframes from this list of data frames (One data frame per day)
#list2env(listOfDataFrames ,.GlobalEnv)

#create specific dataframes for the daily number of "ActiveStuPart" and "ActiveStuPart[SymYes]".
activeStuPartIdDate_df <- data.frame(ncol= 2, byrow = TRUE)
activeStuPartSymYesIdDate_df <- data.frame(ncol= 2, byrow = TRUE)

# definition of a specific timespan (month, pollenspecific period, pollen season in total etc.)
beginDate <- as.Date.character("2018-04-01")

endDate <- as.Date.character("2018-05-31")

date <- beginDate


while(date <= endDate){
  
  #shorten the effectiveDateTime to a comparable format
  dateXActiveStuPart <- as.Date(uniqueSym$effectiveDateTime, format="%Y-%m-%d")
  dateXActiveStuPartSymYes <- as.Date(uniqueSym$effectiveDateTime, format="%Y-%m-%d")
  
  #Build subsets for each day during the timespan
  activeStuPartSymYes_Id <- subset(uniqueSymYes$id, dateXActiveStuPartSymYes == date)
  activeStuPart_Id <- subset(uniqueSym$id, dateXActiveStuPart == date)
  
  #Get the number of entries
  activeStuPartSymYes_Id_length <- length(activeStuPartSymYes_Id)
  activeStuPart_Id_length <- length(activeStuPart_Id)
  
  #Save this number in a vector for each day
  activeStuPartSymYes_IdDate_vector <- c(activeStuPartSymYes_Id_length, date)
  activeStuPart_IdDate_vector <- c(activeStuPart_Id_length, date)
  
  #Combine the vectors with the predefined dataframes
  activeStuPartSymYesIdDate_df <- rbind(activeStuPartSymYesIdDate_df, activeStuPartSymYes_IdDate_vector)
  activeStuPartIdDate_df <- rbind(activeStuPartIdDate_df, activeStuPart_IdDate_vector)
  
  date <- date + 1
}



# delete first row in matrix
activeStuPartSymYesIdDate_df <- activeStuPartSymYesIdDate_df[-1,]
activeStuPartIdDate_df <- activeStuPartIdDate_df[-1,]

# name the columns of the "ActiveStuPart" and "ActiveStuPart[SymYes]" df
colnames(activeStuPartSymYesIdDate_df) <- c("sumActiveStuPart[SymYes]", "Date")
print(activeStuPartSymYesIdDate_df)

colnames(activeStuPartIdDate_df) <- c("sumActiveStuPart", "Date")
print(activeStuPartIdDate_df)

# make "sumActiveStuPart" and "sumActiveStuPart[SymYes]" to integer
activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]` <- as.integer(activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]`)
activeStuPartIdDate_df$sumActiveStuPart <- as.integer(activeStuPartIdDate_df$sumActiveStuPart)

# build the mean of all unique bodysite observations
MeanActiveStuPartSymYes <- mean(activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]`)
MeanActiveStuPart <- mean(activeStuPartIdDate_df$sumActiveStuPart)


#Calculation

prevalence <- activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]` / activeStuPartIdDate_df$sumActiveStuPart

naNFilter <- as.vector(prevalence)
prevalenceFiltered <- naNFilter[!is.na(naNFilter)]

meanPrevalence <- mean(prevalenceFiltered)
str(meanPrevalence)

#Visualization

nbrOfActiveStuPart <- nrow(activeStuPartIdDate_df)
days <- c(1:nbrOfActiveStuPart) #c(activeStuPartIdDate_df$Date)

activeStuPart <- c(activeStuPartIdDate_df$sumActiveStuPart-activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]`) #subtraction necessary due to correct visualization
activeStuPartSymYes <-c(activeStuPartSymYesIdDate_df$`sumActiveStuPart[SymYes]`)

values <- c(activeStuPart, activeStuPartSymYes)

type <- c(rep("ActiveStuPart", nbrOfActiveStuPart), rep("ActiveStuPart[SymYes]", nbrOfActiveStuPart))
data <- data.frame(days, values)

library(ggplot2)

p <- ggplot(data, aes(days, values))
p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Days") + ylab("Prevalence in %") + 
  ggtitle("Prevalence of the study participants") + labs(fill= "") + geom_hline(yintercept = meanPrevalence, size = 1.5, color="blue") + theme_bw() + scale_y_continuous(labels=scales::percent)

# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title


