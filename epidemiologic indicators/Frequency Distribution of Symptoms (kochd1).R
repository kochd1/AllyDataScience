# Author: Dominik R. Kocher (kochd1)

# Script for the indicator �Frequency Distribution of Symptoms�

# Load libraries
library(jsonlite)
library(ggplot2)

### 1 - INPUT ###


# Name of the JSON-File (studydata)
inputFile <- "study_fullExport (dummy).json"

# Date of beginning as YYYY-MM-DD
beginDate <- as.Date.character("2018-04-01") # Fill in

# Date of end as YYYY-MM-DD
endDate <- as.Date.character("2018-04-30") # Fill in

### 2 - DATA IMPORT ###
# Load JSON and convert to dataframe
study_df <- data.frame(read_json(inputFile, simplifyDataFrame = TRUE))

### 3 - GET DATA FOR THE INDICATOR ###

# Creation of a subset of all observations
observations <- subset(study_df$entry.resource, study_df$entry.resource$resourceType == 'Observation')

# Get the coding of this observation
ObsType <- observations$code$coding

# Prepare it as a dataframe
colObsCoding <- matrix(unlist(sapply(ObsType, as.data.frame)),ncol=3, byrow=T)

# class(colObsCoding) -> matrix

# Get Code an Display
# colObsCoding[,2:3] #text and code

# Only Code
colObsCoding[,2]

# Subset only allergy-to-pollen Observations with Coding 300910009 (Allergy to Pollen)
allergy_to_pollen_df <- subset(observations, colObsCoding[,2] == 300910009)

#Get all symptoms with valueQuantity>0
symYes <- subset(allergy_to_pollen_df, allergy_to_pollen_df$valueQuantity>0)

# Get all unique allergy to pollen obs. (incl. symptoms with valueQuantity == 0)
uniqueSym <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference))

# Get all unique observations with a syptom (valueQuantity>0)
uniqueSymYes <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$valueQuantity>0)

# Get the bodysite coding
bodySiteCoding <- symYes$bodySite$coding

bodySiteCodes <- matrix(unlist(sapply(bodySiteCoding, as.data.frame)),ncol=3, byrow=T)

bodySiteCodes[,2]

# Filter all SymYes by Bodysite (six in total)
noseSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 45206002)

eyeSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 81745001)

mouthThroatSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 312533001)

gastrointestinalTractSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 122865005)

skinSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 39937001)

lungSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 39607008)


# Create specific dataframes for the different bodysites and by visualization variant
noseObs_IdDate_dfV1 <- data.frame(ncol = 2, byrow = TRUE)
eyeObs_IdDate_dfV1 <- data.frame(ncol = 2, byrow = TRUE)
mouthThroatObs_IdDate_dfV1 <- data.frame(ncol = 2, byrow = TRUE)
gastrointestinalTractObs_IdDate_dfV1 <- data.frame(ncol = 2, byrow = TRUE)
skinObs_IdDate_dfV1 <- data.frame(ncol = 2, byrow = TRUE)
lungObs_IdDate_dfV1 <- data.frame(ncol = 2, byrow = TRUE)

noseObs_IdDate_dfV2 <- data.frame(ncol = 2, byrow = TRUE)
eyeObs_IdDate_dfV2 <- data.frame(ncol = 2, byrow = TRUE)
mouthThroatObs_IdDate_dfV2 <- data.frame(ncol = 2, byrow = TRUE)
gastrointestinalTractObs_IdDate_dfV2 <- data.frame(ncol = 2, byrow = TRUE)
skinObs_IdDate_dfV2 <- data.frame(ncol = 2, byrow = TRUE)
lungObs_IdDate_dfV2 <- data.frame(ncol = 2, byrow = TRUE)

# Create a specific dataframe for the daily number of "ActiveStuPart".
activeStuPartIdDate_df <- data.frame(ncol= 2, byrow = TRUE)

# Create a specific dataframe for the daily number of "SymYes".
symYesIdDate_df <- data.frame(ncol= 2, byrow = TRUE)

# Create a specific dataframe for the daily number of "SymYesSubj" (one symptom per StuPart per day and per bodysite)
symYesSubjDate_df <- data.frame(ncol= 2, byrow = TRUE)

date <- beginDate

#test
#date <- as.character("2018-04-22")

while(date <= endDate){

  # shorten the effectiveDateTime to a comparable format
  dateXNose <- as.Date(noseSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXEyes <- as.Date(eyeSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXMouthThroat <- as.Date(mouthThroatSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXGastrointestinalTract <- as.Date(gastrointestinalTractSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXSkin <- as.Date(skinSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXLung <- as.Date(lungSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  
  #V1
  dateXActiveStuPart <- as.Date(allergy_to_pollen_df$effectiveDateTime, format="%Y-%m-%d")
  
  #V2
  dateXSymYes <- as.Date(symYes$effectiveDateTime, format="%Y-%m-%d")
  
  # Build subsets for each day during the predefined timespan
  noseObs_dateX <- subset(noseSymptoms_df, dateXNose == date)
  uniqueNoseObs_Id <- subset(noseObs_dateX$id, !duplicated(noseObs_dateX$subject$reference))
  
  eyeObs_dateX <- subset(eyeSymptoms_df, dateXEyes == date)
  uniqueEyeObs_Id <- subset(eyeObs_dateX$id, !duplicated(eyeObs_dateX$subject$reference))
  
  mouthThroatObs_dateX <- subset(mouthThroatSymptoms_df, dateXMouthThroat == date)
  uniqueMouthThroatObs_Id <- subset(mouthThroatObs_dateX$id, !duplicated(mouthThroatObs_dateX$subject$reference))
  
  gastrointestinalTractObs_dateX <- subset(gastrointestinalTractSymptoms_df, dateXGastrointestinalTract == date)
  uniqueGastrointestinalTractObs_Id <- subset(gastrointestinalTractObs_dateX$id, !duplicated(gastrointestinalTractObs_dateX$subject$reference))
  
  skinObs_dateX <- subset(skinSymptoms_df, dateXSkin == date)
  uniqueSkinObs_Id <- subset(skinObs_dateX$id, !duplicated(skinObs_dateX$subject$reference))
  
  lungObs_dateX <- subset(lungSymptoms_df, dateXLung == date)
  uniqueLungObs_Id <- subset(lungObs_dateX$id, !duplicated(lungObs_dateX$subject$reference))
  
  activeStuPart_dateX <- subset(allergy_to_pollen_df, dateXActiveStuPart == date)
  activeStuPart_Id <- subset(activeStuPart_dateX$id, !duplicated(activeStuPart_dateX$subject$reference))
  
  symYes_dateX <- subset(symYes, dateXSymYes == date)
  
  symYes_Id <- c(uniqueEyeObs_Id, uniqueNoseObs_Id, uniqueMouthThroatObs_Id, uniqueLungObs_Id, uniqueGastrointestinalTractObs_Id, uniqueSkinObs_Id) #subset(symYes_dateX$id, !duplicated(symYes_dateX$subject$reference))  #!duplicated(symYes_dateX[, symYes_dateX$subject$reference:bodySiteCodes_dateX_df$bodySiteCodes_dateX...2.])) #subjReference darf mehrmals vorkommen, jedoch m�ssen die k�rperstellen pro activeStuPart pro Tag jeweils unique sein! -> d. h. zwei Bedingungen in !duplicated()/unique()
  
  symYes_uniqueSubj <- subset(symYes_dateX$subject, !duplicated(symYes_dateX$subject$reference)) #count only one symptom per patient (visualization)
  
  # Get the number of entries
  noseObs_Id_length <- length(uniqueNoseObs_Id)
  eyeObs_Id_length <- length(uniqueEyeObs_Id)
  mouthThroatObs_Id_length <- length(uniqueMouthThroatObs_Id)
  gastrointestinalTractObs_Id_length <- length(uniqueGastrointestinalTractObs_Id)
  skinObs_Id_length <- length(uniqueSkinObs_Id)
  lungObs_Id_length <- length(uniqueLungObs_Id)
  
  activeStuPart_Id_length <- length(activeStuPart_Id)
  symYes_Id_length <- length(symYes_Id)
  symYes_uniqueSubj_length <- length(symYes_uniqueSubj$reference)

  # Save this number in a vector for each day
  noseObs_IdDate_vector <- c(noseObs_Id_length, date)
  eyeObs_IdDate_vector <- c(eyeObs_Id_length, date)
  mouthThroatObs_IdDate_vector <- c(mouthThroatObs_Id_length, date)
  gastrointestinalTractObs_IdDate_vector <- c(gastrointestinalTractObs_Id_length, date)
  skinObs_IdDate_vector <- c(skinObs_Id_length, date)
  lungObs_IdDate_vector <- c(lungObs_Id_length, date)
  
  activeStuPart_IdDate_vector <- c(activeStuPart_Id_length, as.character(date))
  symYes_IdDate_vector <- c(symYes_Id_length, as.character(date))
  symYes_SubjDate_vector <- c(symYes_uniqueSubj_length, as.character(date))

  # Combine the vectors with the predefined dataframes
  noseObs_IdDate_dfV1 <- rbind(noseObs_IdDate_dfV1, noseObs_IdDate_vector)
  eyeObs_IdDate_dfV1 <- rbind(eyeObs_IdDate_dfV1, eyeObs_IdDate_vector)
  mouthThroatObs_IdDate_dfV1 <- rbind(mouthThroatObs_IdDate_dfV1, mouthThroatObs_IdDate_vector)
  gastrointestinalTractObs_IdDate_dfV1 <- rbind(gastrointestinalTractObs_IdDate_dfV1, gastrointestinalTractObs_IdDate_vector)
  skinObs_IdDate_dfV1 <- rbind(skinObs_IdDate_dfV1, skinObs_IdDate_vector)
  lungObs_IdDate_dfV1 <- rbind(lungObs_IdDate_dfV1, lungObs_IdDate_vector)
  
  noseObs_IdDate_dfV2 <- rbind(noseObs_IdDate_dfV2, noseObs_IdDate_vector)
  eyeObs_IdDate_dfV2 <- rbind(eyeObs_IdDate_dfV2, eyeObs_IdDate_vector)
  mouthThroatObs_IdDate_dfV2 <- rbind(mouthThroatObs_IdDate_dfV2, mouthThroatObs_IdDate_vector)
  gastrointestinalTractObs_IdDate_dfV2 <- rbind(gastrointestinalTractObs_IdDate_dfV2, gastrointestinalTractObs_IdDate_vector)
  skinObs_IdDate_dfV2 <- rbind(skinObs_IdDate_dfV2, skinObs_IdDate_vector)
  lungObs_IdDate_dfV2 <- rbind(lungObs_IdDate_dfV2, lungObs_IdDate_vector)
  
  activeStuPartIdDate_df <- rbind(activeStuPartIdDate_df, activeStuPart_IdDate_vector)
  symYesIdDate_df <- rbind(symYesIdDate_df, symYes_IdDate_vector)
  symYesSubjDate_df <- rbind(symYesSubjDate_df, symYes_SubjDate_vector)

    date <- date + 1
    
    }

# delete first row in matrix
noseObs_IdDate_dfV1 <- noseObs_IdDate_dfV1[-1,]
eyeObs_IdDate_dfV1 <- eyeObs_IdDate_dfV1[-1,]
mouthThroatObs_IdDate_dfV1 <- mouthThroatObs_IdDate_dfV1[-1,]
gastrointestinalTractObs_IdDate_dfV1 <- gastrointestinalTractObs_IdDate_dfV1[-1,]
skinObs_IdDate_dfV1 <- skinObs_IdDate_dfV1[-1,]
lungObs_IdDate_dfV1 <- lungObs_IdDate_dfV1[-1,]

noseObs_IdDate_dfV2 <- noseObs_IdDate_dfV2[-1,]
eyeObs_IdDate_dfV2 <- eyeObs_IdDate_dfV2[-1,]
mouthThroatObs_IdDate_dfV2 <- mouthThroatObs_IdDate_dfV2[-1,]
gastrointestinalTractObs_IdDate_dfV2 <- gastrointestinalTractObs_IdDate_dfV2[-1,]
skinObs_IdDate_dfV2 <- skinObs_IdDate_dfV2[-1,]
lungObs_IdDate_dfV2 <- lungObs_IdDate_dfV2[-1,]


activeStuPartIdDate_df <- activeStuPartIdDate_df[-1,]
symYesIdDate_df <- symYesIdDate_df[-1,]
symYesSubjDate_df <- symYesSubjDate_df[-1,]

# name the columns of the different bodysite df
colnames(noseObs_IdDate_dfV1) <- c("sumActiveStuPart[SymYes][Nose]", "Date")
print(noseObs_IdDate_dfV1)

colnames(eyeObs_IdDate_dfV1) <- c("sumActiveStuPart[SymYes][Eyes]", "Date")
print(eyeObs_IdDate_dfV1)

colnames(mouthThroatObs_IdDate_dfV1) <- c("sumActiveStuPart[SymYes][Mouth/Throat]", "Date")
print(mouthThroatObs_IdDate_dfV1)

colnames(gastrointestinalTractObs_IdDate_dfV1) <- c("sumActiveStuPart[SymYes][Gastrointestinal Tract]", "Date")
print(gastrointestinalTractObs_IdDate_dfV1)

colnames(skinObs_IdDate_dfV1) <- c("sumActiveStuPart[SymYes][Skin]", "Date")
print(skinObs_IdDate_dfV1)


colnames(lungObs_IdDate_dfV1) <- c("sumActiveStuPart[SymYes][Lungs]", "Date")
print(lungObs_IdDate_dfV1)


colnames(noseObs_IdDate_dfV2) <- c("sum[SymYes][Nose]", "Date")
print(noseObs_IdDate_dfV2)


colnames(eyeObs_IdDate_dfV2) <- c("sum[SymYes][Eyes]", "Date")
print(eyeObs_IdDate_dfV2)

colnames(mouthThroatObs_IdDate_dfV2) <- c("sum[SymYes][Mouth/Throat]", "Date")
print(mouthThroatObs_IdDate_dfV2)

colnames(gastrointestinalTractObs_IdDate_dfV2) <- c("sum[SymYes][Gastrointestinal Tract]", "Date")
print(gastrointestinalTractObs_IdDate_dfV2)

colnames(skinObs_IdDate_dfV2) <- c("sum[SymYes][Skin]", "Date")
print(skinObs_IdDate_dfV2)


colnames(lungObs_IdDate_dfV2) <- c("sum[SymYes][Lungs]", "Date")
print(lungObs_IdDate_dfV2)

# name the column of the "ActiveStuPart" df
colnames(activeStuPartIdDate_df) <- c("sumActiveStuPart", "Date")
print(activeStuPartIdDate_df)

colnames(symYesIdDate_df) <- c("sumSymYes", "Date")
print(symYesIdDate_df)

colnames(symYesSubjDate_df) <- c("sumSubjSymYes", "Date")

# make sumActiveStuPart[SymYes]["Bodysite"] to integer
noseObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Nose]` <- as.integer(noseObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Nose]`)
eyeObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Eyes]` <- as.integer(eyeObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Eyes]`)
mouthThroatObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Mouth/Throat]` <- as.integer(mouthThroatObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Mouth/Throat]`)
gastrointestinalTractObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Gastrointestinal Tract]` <- as.integer(gastrointestinalTractObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Gastrointestinal Tract]`)
skinObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Skin]` <- as.integer(skinObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Skin]`)
lungObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Lungs]` <- as.integer(lungObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Lungs]`)

# make sum[SymYes]["Bodysite"] to integer
noseObs_IdDate_dfV2$`sum[SymYes][Nose]` <- as.integer(noseObs_IdDate_dfV2$`sum[SymYes][Nose]`)
eyeObs_IdDate_dfV2$`sum[SymYes][Eyes]` <- as.integer(eyeObs_IdDate_dfV2$`sum[SymYes][Eyes]`)
mouthThroatObs_IdDate_dfV2$`sum[SymYes][Mouth/Throat]` <- as.integer(mouthThroatObs_IdDate_dfV2$`sum[SymYes][Mouth/Throat]`)
gastrointestinalTractObs_IdDate_dfV2$`sum[SymYes][Gastrointestinal Tract]` <- as.integer(gastrointestinalTractObs_IdDate_dfV2$`sum[SymYes][Gastrointestinal Tract]`)
skinObs_IdDate_dfV2$`sum[SymYes][Skin]` <- as.integer(skinObs_IdDate_dfV2$`sum[SymYes][Skin]`)
lungObs_IdDate_dfV2$`sum[SymYes][Lungs]` <- as.integer(lungObs_IdDate_dfV2$`sum[SymYes][Lungs]`)

activeStuPartIdDate_df$sumActiveStuPart <- as.integer(activeStuPartIdDate_df$sumActiveStuPart)
symYesIdDate_df$sumSymYes <- as.integer(symYesIdDate_df$sumSymYes)
symYesSubjDate_df$sumSubjSymYes <- as.integer(symYesSubjDate_df$sumSubjSymYes)

#build the mean of all unique bodysite observations
MeanNoseObs <- mean(noseObs_IdDate_dfV2$`sum[SymYes][Nose]`)
MeanEyesObs <- mean(eyeObs_IdDate_dfV2$`sum[SymYes][Eyes]`)
MeanMouthThroatObs <- mean(mouthThroatObs_IdDate_dfV2$`sum[SymYes][Mouth/Throat]`)
MeanGastrointestinalTractObs <- mean(gastrointestinalTractObs_IdDate_dfV2$`sum[SymYes][Gastrointestinal Tract]`)
MeanSkinObs <- mean(skinObs_IdDate_dfV2$`sum[SymYes][Skin]`)
MeanLungsObs <- mean(lungObs_IdDate_dfV2$`sum[SymYes][Lungs]`)


### 4 - CALCULATION ###

# valueset[is.na(valueset)] <- 0 // because of possible NaN-Values (Division by Zero)

#V1
ratioNoseObsToActiveStuPart <- noseObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Nose]` / activeStuPartIdDate_df$sumActiveStuPart
ratioNoseObsToActiveStuPart[is.na(ratioNoseObsToActiveStuPart)] <- 0

ratioEyeObsToActiveStuPart <- eyeObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Eyes]` / activeStuPartIdDate_df$sumActiveStuPart
ratioEyeObsToActiveStuPart[is.na(ratioEyeObsToActiveStuPart)] <- 0

ratioMouthThroatObsToActiveStuPart <- mouthThroatObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Mouth/Throat]` / activeStuPartIdDate_df$sumActiveStuPart
ratioMouthThroatObsToActiveStuPart[is.na(ratioMouthThroatObsToActiveStuPart)] <- 0

ratioGastroIntestinalTractObsToActiveStuPart <- gastrointestinalTractObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Gastrointestinal Tract]` / activeStuPartIdDate_df$sumActiveStuPart
ratioGastroIntestinalTractObsToActiveStuPart[is.na(ratioGastroIntestinalTractObsToActiveStuPart)] <- 0

ratioSkinObsToActiveStuPart <- skinObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Skin]` / activeStuPartIdDate_df$sumActiveStuPart
ratioSkinObsToActiveStuPart[is.na(ratioSkinObsToActiveStuPart)] <- 0

ratioLungObsToActiveStuPart <- lungObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Lungs]` / activeStuPartIdDate_df$sumActiveStuPart
ratioLungObsToActiveStuPart[is.na(ratioLungObsToActiveStuPart)] <- 0

#V2
ratioNoseObsToSymYes <- noseObs_IdDate_dfV2$`sum[SymYes][Nose]` / symYesSubjDate_df$sumSubjSymYes
ratioNoseObsToSymYes[is.na(ratioNoseObsToSymYes)] <- 0

ratioEyeObsToSymYes <- eyeObs_IdDate_dfV2$`sum[SymYes][Eyes]` / symYesSubjDate_df$sumSubjSymYes
ratioEyeObsToSymYes[is.na(ratioEyeObsToSymYes)] <- 0

ratioMouthThroatObsToSymYes <- mouthThroatObs_IdDate_dfV2$`sum[SymYes][Mouth/Throat]` / symYesSubjDate_df$sumSubjSymYes
ratioMouthThroatObsToSymYes[is.na(ratioMouthThroatObsToSymYes)] <- 0

ratioGastroIntestinalObsToSymYes <- gastrointestinalTractObs_IdDate_dfV2$`sum[SymYes][Gastrointestinal Tract]` / symYesSubjDate_df$sumSubjSymYes
ratioGastroIntestinalObsToSymYes[is.na(ratioGastroIntestinalObsToSymYes)] <- 0

ratioSkinObsToSymYes <- skinObs_IdDate_dfV2$`sum[SymYes][Skin]` / symYesSubjDate_df$sumSubjSymYes
ratioSkinObsToSymYes[is.na(ratioSkinObsToSymYes)] <- 0

ratioLungObsToSymYes <- lungObs_IdDate_dfV2$`sum[SymYes][Lungs]` / symYesSubjDate_df$sumSubjSymYes
ratioLungObsToSymYes[is.na(ratioLungObsToSymYes)] <- 0


### 5 - VISUALIZATION ###

#V1: 100% -> Total of all StuPart
  
# show a stacked bar chart with the daily symptoms per bodysite
nbrOfObs <- nrow(activeStuPartIdDate_df)
days <- c(1:nbrOfObs) #c(activeStuPartIdDate_df$Date)

ratioActiveStuPartWithoutSymptoms <- c((activeStuPartIdDate_df$sumActiveStuPart - symYesSubjDate_df$sumSubjSymYes) / activeStuPartIdDate_df$sumActiveStuPart)                           
                        
values <- c(ratioActiveStuPartWithoutSymptoms, ratioNoseObsToActiveStuPart, ratioEyeObsToActiveStuPart, ratioMouthThroatObsToActiveStuPart, ratioGastroIntestinalTractObsToActiveStuPart, ratioSkinObsToActiveStuPart, ratioLungObsToActiveStuPart)

type <- c(rep("No Symptoms", nbrOfObs), rep("SymYes[Nose]", nbrOfObs), rep("SymYes[Eyes]", nbrOfObs)
          ,rep("SymYes[Mouth/Throat]", nbrOfObs), rep("SymYes[Gastrointestinal Tract]", nbrOfObs), rep("SymYes[Skin]", nbrOfObs), rep("SymYes[Lungs]", nbrOfObs))

data <- data.frame(days, values)


p <- ggplot(data, aes(days, values))


p +geom_bar(stat= "identity", position = "stack", aes(fill = type)) + xlab("Days") + ylab("Symptoms per ActiveStuPart by Bodysite %") + 
  #geom_text(aes(label = sprintf("%0.1f%%", percent)), position = position_stack(vjust = 0.5)) +
  ggtitle(paste("ActiveStuPart with Symptoms by Bodysite ", "(from ", beginDate, " to ", endDate, ")", sep="")) + 
  scale_fill_manual(name ="ActiveStuPart with:", values = c("No Symptoms" = "red",
                                   "SymYes[Eyes]" = "deepskyblue3",
                                   "SymYes[Nose]" = "darkgoldenrod2",
                                   "SymYes[Mouth/Throat]" = "seagreen3",
                                   "SymYes[Gastrointestinal Tract]" = "lightslateblue",
                                   "SymYes[Lungs]" = "pink3",
                                   "SymYes[Skin]" = "peachpuff2")) + labs(fill= "ActiveStuPart with:") + theme_bw() + scale_x_continuous(breaks = seq(0, nbrOfObs, by = 5)) + scale_y_continuous(limit = c(0, 6), labels=scales::percent)


# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title
# scale_y... -> set limits for the y-axis

#V2: 100% -> Total of all Symptoms

# show a stacked bar chart with the daily symptoms per bodysite
nbrOfObs <- nrow(activeStuPartIdDate_df)
days <- c(1:nbrOfObs)

values <- c(ratioEyeObsToSymYes, ratioNoseObsToSymYes, ratioMouthThroatObsToSymYes, ratioGastroIntestinalObsToSymYes, ratioSkinObsToSymYes, ratioLungObsToSymYes)


type <- c(rep("SymYes[Eyes]", nbrOfObs), rep("SymYes[Nose]", nbrOfObs), 
          rep("SymYes[Mouth/Throat]", nbrOfObs), rep("SymYes[Gastrointestinal Tract]", nbrOfObs), rep("SymYes[Skin]", nbrOfObs), rep("SymYes[Lungs]", nbrOfObs))

data <- data.frame(days, values)


p <- ggplot(data, aes(days, values))

p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Days") + ylab("Symptoms per Bodysite in %") +
  #geom_text(aes(label = sprintf("%0.1f%%", percent)), position = position_stack(vjust = 0.5)) +
  ggtitle(paste("Frequency of Symptoms by Bodysite ", "(from ", beginDate, " to ", endDate, ")", sep= "")) + labs(fill= "") +
  scale_fill_manual("", values = c("SymYes[Eyes]" = "deepskyblue3",
                                   "SymYes[Nose]" = "darkgoldenrod2",
                                   "SymYes[Mouth/Throat]" = "seagreen3",
                                   "SymYes[Gastrointestinal Tract]" = "lightslateblue",
                                   "SymYes[Lungs]" = "pink3",
                                   "SymYes[Skin]" = "peachpuff2")) + theme_bw() + scale_x_continuous(breaks = seq(0, nbrOfObs, by = 5)) + scale_y_continuous(limit= c(0, 1), labels=scales::percent)


# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title
# scale_y... -> set limits for the y-axis

#show a pie chart with the mean values of all bodysites in a predefined timespan
meanVector <- c(MeanNoseObs, MeanEyesObs, MeanMouthThroatObs, MeanGastrointestinalTractObs, MeanSkinObs, MeanLungsObs)


pct <- round(meanVector/sum(meanVector)*100)


lbls <- c("Nose", "Eyes", "Mouth/Throat", "Gastrointestinal Tract", "Skin", "Lungs")
lbls <- paste(lbls, pct) # add percents to labels

lbls <- paste(lbls,"%",sep="") # ad % to labels


# still searching for a simple alternative in ggplot
pie(meanVector, labels = lbls, main="Frequency of Symptoms by Bodysite"
    , col=rainbow(length(meanVector)))



