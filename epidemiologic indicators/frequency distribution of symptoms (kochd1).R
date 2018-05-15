#author: Dominik R. Kocher (kochd1)

#script for the indicator "frequency distribution of symptoms"

# Load library
library(jsonlite)

#prepare study data

#options(stringsAsFactors=FALSE)

#studyData <- read_json("study_fullExport (dummy).json", simplifyVector = TRUE)

study_df <- data.frame(read_json("study_fullExport (dummy).json", simplifyVector= TRUE)) #2x practitioner, 11x patient, 36x observation -> 49 obs.

#-------------------------------------------------------------

#creation of a subset of all observations
observations <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation')

#Get the coding of this observation
ObsType <- observations$code$coding

#prepare it as a dataframe
colObsCoding <- matrix(unlist(sapply(ObsType, as.data.frame)),ncol=3, byrow=T)
# class(colObsCoding) -> matrix

# Get Code an Display
#colObsCoding[,2:3] #text and code
#only Code
colObsCoding[,2]

# Subset only allergy-to-pollen Observations with Coding 300910009 (Allergy to Pollen)
allergy_to_pollen_df <- subset(observations, colObsCoding[,2] == 300910009)
#class(allergy_to_pollen_df)

#Get all unique allergy to pollen obs. (inlc. symptoms value==0)
uniqueSym <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference))

#Get all unique observations with a syptom (value>0)
uniqueSymYes <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$valueQuantity>0)

#filter all stuPart with nose symptoms

#auch nach typ der Observation filtern!
bodySiteCoding <- uniqueSymYes$bodySite$coding

bodySiteCodes <- matrix(unlist(sapply(bodySiteCoding, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Get Code an Display
#y[,2:3] #text and code
bodySiteCodes[,2]

noseSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 45206002) #& (uniqueSymYes$effectiveDateTime >="2018-04-11" & uniqueSymYes$effectiveDateTime <"2018-04-12"))
#AllStuPartSymYesNose <- length(noseSymptoms_df$id)

eyeSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 81745001) #& (uniqueSymYes$effectiveDateTime >="2018-04-11" & uniqueSymYes$effectiveDateTime <"2018-04-12"))
#AllStuPartSymYesEyes <- length(eyeSymptoms_df$id)

skinSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 39937001) #& (uniqueSymYes$effectiveDateTime >="2018-04-11" & uniqueSymYes$effectiveDateTime <"2018-04-12"))
#AllStuPartSymYesSkin <- length(skinSymptoms_df$id)

lungSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 39607008) #& (uniqueSymYes$effectiveDateTime >="2018-04-11" & uniqueSymYes$effectiveDateTime <"2018-04-12"))
#AllStuPartSymYesLungs <- length(lungSymptoms_df$id)

#create specific dataframes for the amount of unique Observations per bodysite per day
noseObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
eyeObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
skinObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
lungObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)

#create a specific dataframe for the daily number of "ActiveStuPart".
activeStuPartIdDate_df <- data.frame(ncol= 2, byrow = TRUE)


#looping through all days of a month e.g. April

# YYYY-MM
year_month <- "2018-04"

# DD
days_of_month <- 30

for(j in 1:days_of_month) {
  if(j < 10) {
    j <- paste("0",j, sep = "")
  }
  
  date <- paste(year_month, j, sep = "-")
  
  #shorten the effectiveDateTime to a comparable format
  dateXNose<- as.Date(noseSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXEyes<- as.Date(eyeSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXSkin<- as.Date(skinSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXLung<- as.Date(lungSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  
  dateXActiveStuPart <- as.Date(uniqueSym$effectiveDateTime, format="%Y-%m-%d")
  
  #Build subsets for each day during the timespan 
  noseObs_Id <- subset(noseSymptoms_df$id, dateXNose == date)
  eyeObs_Id <- subset(eyeSymptoms_df$id, dateXEyes == date)
  skinObs_Id <- subset(skinSymptoms_df$id, dateXSkin == date)
  lungObs_Id <- subset(lungSymptoms_df$id, dateXLung == date)
  
  activeStuPart_Id <- subset(uniqueSym$id, dateXActiveStuPart == date)
  
  #Get the number of entries
  noseObs_Id_length <- length(noseObs_Id)
  eyeObs_Id_length <- length(eyeObs_Id)
  skinObs_Id_length <- length(skinObs_Id)
  lungObs_Id_length <- length(lungObs_Id)
  
  activeStuPart_Id_length <- length(activeStuPart_Id)
  
  #Save this number in a vector for each day
  noseObs_IdDate_vector <- c(noseObs_Id_length, date)
  eyeObs_IdDate_vector <- c(eyeObs_Id_length, date)
  skinObs_IdDate_vector <- c(skinObs_Id_length, date)
  lungObs_IdDate_vector <- c(lungObs_Id_length, date)
  
  activeStuPart_IdDate_vector <- c(activeStuPart_Id_length, date)
  
  #Combine the vectors with the predefined dataframes
  noseObs_IdDate_df <- rbind(noseObs_IdDate_df, noseObs_IdDate_vector)
  eyeObs_IdDate_df <- rbind(eyeObs_IdDate_df, eyeObs_IdDate_vector)
  skinObs_IdDate_df <- rbind(skinObs_IdDate_df, skinObs_IdDate_vector)
  lungObs_IdDate_df <- rbind(lungObs_IdDate_df, lungObs_IdDate_vector)
  
  activeStuPartIdDate_df <- rbind(activeStuPartIdDate_df, activeStuPart_IdDate_vector)
}

# delete first row in matrix
noseObs_IdDate_df <- noseObs_IdDate_df[-1,]
eyeObs_IdDate_df <- eyeObs_IdDate_df[-1,]
skinObs_IdDate_df <- skinObs_IdDate_df[-1,]
lungObs_IdDate_df <- lungObs_IdDate_df[-1,]

activeStuPartIdDate_df <- activeStuPartIdDate_df[-1,]

# name the columns of the different bodysite df
colnames(noseObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Nose]", "Date")
print(noseObs_IdDate_df)

colnames(eyeObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Eyes]", "Date")
print(eyeObs_IdDate_df)

colnames(skinObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Skin]", "Date")
print(skinObs_IdDate_df)

colnames(lungObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Lungs]", "Date")
print(lungObs_IdDate_df)

colnames(activeStuPartIdDate_df) <- c("sumActiveStuPart", "Date")
print(activeStuPartIdDate_df)

#make sumActiveStuPart[SymYes]["Bodysite"] to integer
noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]` <- as.integer(noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]`)
eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]` <- as.integer(eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]`)
skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]` <- as.integer(skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]`)
lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]` <- as.integer(lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]`)

activeStuPartIdDate_df$sumActiveStuPart <- as.integer(activeStuPartIdDate_df$sumActiveStuPart)

#build the mean of all unique bodysite observations
MeanNoseObs <- mean(noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]`)
MeanEyesObs <- mean(eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]`)
MeanSkinObs <- mean(skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]`)
MeanLungsObs <- mean(lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]`)

#Calculation
noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]` / activeStuPartIdDate_df$sumActiveStuPart
eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]` / activeStuPartIdDate_df$sumActiveStuPart
skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]` / activeStuPartIdDate_df$sumActiveStuPart
lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]` / activeStuPartIdDate_df$sumActiveStuPart

#Visualization

#show a stacked bar chart with the daily symptoms per bodysite


#show a pie chart with the mean values of all bodysites in a timespan of 30 days
meanVector <- c(MeanNoseObs, MeanEyesObs, MeanSkinObs, MeanLungsObs)
pct <- round(meanVector/sum(meanVector)*100)
lbls <- c("Nose", "Eyes", "Skin", "Lungs")
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

#still searching for a simple alternative in ggplot
pie(meanVector, labels = lbls, main="Anteil ActiveStuPart mit Symptomen nach Körperteil (Mittelwert aus 30 Tagen)
", col=rainbow(length(meanVector)))
