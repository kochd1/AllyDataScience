# author: Dominik R. Kocher (kochd1)

# script for the indicator «Frequency distribution of symptoms»

#TODO: Correct graph-axis

# Load library
library(jsonlite)

# Data preparation

study_df <- data.frame(read_json("study_fullExport (dummy).json", simplifyVector= TRUE))

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

# Get all unique observations with a syptom (value>0)
uniqueSymYes <- subset(allergy_to_pollen_df, !duplicated(allergy_to_pollen_df$subject$reference) & allergy_to_pollen_df$valueQuantity>0)

symYes <- subset(allergy_to_pollen_df, allergy_to_pollen_df$valueQuantity>0)


# Get the bodysite coding
bodySiteCoding <- symYes$bodySite$coding

bodySiteCodes <- matrix(unlist(sapply(bodySiteCoding, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Get Code an Display
#y[,2:3] #text and code
bodySiteCodes[,2]

# Filter all stuPart (unique observations) by bodysite (six in total)
noseSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 45206002)
# AllStuPartSymYesNose <- length(noseSymptoms_df$id)

eyeSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 81745001)
# AllStuPartSymYesEyes <- length(eyeSymptoms_df$id)

mouthThroatSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 312533001)
# AllStuPartSymYesEyes <- length(eyeSymptoms_df$id)

gastrointestinalTractSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 122865005)
# AllStuPartSymYesEyes <- length(eyeSymptoms_df$id)

skinSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 39937001)
# AllStuPartSymYesSkin <- length(skinSymptoms_df$id)

lungSymptoms_df <- subset(symYes, bodySiteCodes[,2] == 39607008)
# AllStuPartSymYesLungs <- length(lungSymptoms_df$id)

# create specific dataframes for the amount of unique Observations per bodysite per day
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

# create a specific dataframe for the daily number of "ActiveStuPart".
activeStuPartIdDate_df <- data.frame(ncol= 2, byrow = TRUE)

symYesIdDate_df <- data.frame(ncol= 2, byrow = TRUE)
symYesSubjDate_df <- data.frame(ncol= 2, byrow = TRUE)
# definition of a specific timespan (month, pollenspecific period, pollen season in total etc.)

beginDate <- as.Date.character("2018-04-01")

endDate <- as.Date.character("2018-05-31")

date <- beginDate

#test
#date <- as.character("2018-05-20")

while(date <= endDate){

  # shorten the effectiveDateTime to a comparable format
  dateXNose<- as.Date(noseSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXEyes<- as.Date(eyeSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXMouthThroat<- as.Date(mouthThroatSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXGastrointestinalTract<- as.Date(gastrointestinalTractSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXSkin<- as.Date(skinSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXLung<- as.Date(lungSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  
  #V1
  dateXActiveStuPart <- as.Date(symYes$effectiveDateTime, format="%Y-%m-%d")
  
  #V2
  dateXSymYes <- as.Date(symYes$effectiveDateTime, format="%Y-%m-%d")
  
  # Build subsets for each day during the timespan
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
  
  activeStuPart_dateX <- subset(symYes, dateXActiveStuPart == date)
  activeStuPart_Id <- subset(activeStuPart_dateX$id, !duplicated(activeStuPart_dateX$subject$reference))
  
  symYes_dateX <- subset(symYes, dateXSymYes == date)
  
  symYes_Id <- c(uniqueEyeObs_Id, uniqueNoseObs_Id, uniqueMouthThroatObs_Id, uniqueLungObs_Id, uniqueGastrointestinalTractObs_Id, uniqueSkinObs_Id) #subset(symYes_dateX$id, !duplicated(symYes_dateX$subject$reference))  #!duplicated(symYes_dateX[, symYes_dateX$subject$reference:bodySiteCodes_dateX_df$bodySiteCodes_dateX...2.])) #subjReference darf mehrmals vorkommen, jedoch müssen die körperstellen pro activeStuPart pro Tag jeweils unique sein! -> d. h. zwei Bedingungen in !duplicated()/unique()
  
  symYes_uniqueSubj <- subset(symYes_dateX$subject, !duplicated(symYes_dateX$subject$reference)) #count only one symptom per patient (visualization)
  str(symYes_uniqueSubj)
  
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
  str(symYes_uniqueSubj_length)

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


# Calculation

#V1
ratioNoseObsToActiveStuPartV1 <- noseObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Nose]` / activeStuPartIdDate_df$sumActiveStuPart
ratioEyeObsToActiveStuPartV1 <- eyeObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Eyes]` / activeStuPartIdDate_df$sumActiveStuPart
ratioMouthThroatObsToActiveStuPartV1 <- mouthThroatObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Mouth/Throat]` / activeStuPartIdDate_df$sumActiveStuPart
ratioGastroIntestinalTractObsToActiveStuPartV1 <- gastrointestinalTractObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Gastrointestinal Tract]` / activeStuPartIdDate_df$sumActiveStuPart
ratioSkinObsToActiveStuPartV1 <- skinObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Skin]` / activeStuPartIdDate_df$sumActiveStuPart
ratioLungObsToActiveStuPartV1 <- lungObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Lungs]` / activeStuPartIdDate_df$sumActiveStuPart

#V2
ratioNoseObsToActiveStuPartV2 <- noseObs_IdDate_dfV2$`sum[SymYes][Nose]` / symYesIdDate_df$sumSymYes
ratioEyeObsToActiveStuPartV2 <- eyeObs_IdDate_dfV2$`sum[SymYes][Eyes]` / symYesIdDate_df$sumSymYes
ratioMouthThroatObsToActiveStuPartV2 <- mouthThroatObs_IdDate_dfV2$`sum[SymYes][Mouth/Throat]` / symYesIdDate_df$sumSymYes
ratioGastroIntestinalObsToActiveStuPartV2 <- gastrointestinalTractObs_IdDate_dfV2$`sum[SymYes][Gastrointestinal Tract]` / symYesIdDate_df$sumSymYes
ratioSkinObsToActiveStuPartV2 <- skinObs_IdDate_dfV2$`sum[SymYes][Skin]` / symYesIdDate_df$sumSymYes
ratioLungObsToActiveStuPartV2 <- lungObs_IdDate_dfV2$`sum[SymYes][Lungs]` / symYesIdDate_df$sumSymYes

# Visualization

#TODO: Same for not unique obs.

#V1: 100% -> Total of all StuPart
  
# show a stacked bar chart with the daily symptoms per bodysite
nbrOfObs <- nrow(activeStuPartIdDate_df)
days <- c(1:nbrOfObs) #c(activeStuPartIdDate_df$Date)

activeStuPartWithoutSymptoms <- c(activeStuPartIdDate_df$sumActiveStuPart - symYesSubjDate_df$sumSubjSymYes)
                                  
                        
# activeStuPartSymYesNose <-c(noseObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Nose]`)
# activeStuPartSymYesEyes <-c(eyeObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Eyes]`)
# activeStuPartSymYesMouthThroat <-c(mouthThroatObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Mouth/Throat]`)
# activeStuPartSymYesGastrointestinalTract <-c(gastrointestinalTractObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Gastrointestinal Tract]`)
# activeStuPartSymYesSkin <-c(skinObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Skin]`)
# activeStuPartSymYesLungs <-c(lungObs_IdDate_dfV1$`sumActiveStuPart[SymYes][Lungs]`)


#values <- c(activeStuPart, activeStuPartSymYesNose, activeStuPartSymYesEyes, activeStuPartSymYesMouthThroat, activeStuPartSymYesGastrointestinalTract, activeStuPartSymYesSkin, activeStuPartSymYesLungs)
values <- c(activeStuPartWithoutSymptoms, ratioNoseObsToActiveStuPartV1, ratioEyeObsToActiveStuPartV1, ratioMouthThroatObsToActiveStuPartV1, ratioGastroIntestinalTractObsToActiveStuPartV1, ratioSkinObsToActiveStuPartV1, ratioLungObsToActiveStuPartV1)

type <- c(rep("ActiveStuPart without Symptoms", nbrOfObs), rep("ActiveStuPart[SymYes][Nose]", nbrOfObs), rep("ActiveStuPart[SymYes][Eyes]", nbrOfObs)
          ,rep("ActiveStuPart[SymYes][Mouth/Throat]", nbrOfObs), rep("ActiveStuPart[SymYes][Gastrointestinal Tract]", nbrOfObs), rep("ActiveStuPart[SymYes][Skin]", nbrOfObs), rep("ActiveStuPart[SymYes][Lungs]", nbrOfObs))

data <- data.frame(days, values)


library(ggplot2)

p <- ggplot(data, aes(days, values))


p +geom_bar(stat= "identity", position = "stack", aes(fill = type)) + xlab("Days") + ylab("Symptoms per ActiveStuPart by Bodysite %") + 
  #geom_text(aes(label = sprintf("%0.1f%%", percent)), position = position_stack(vjust = 0.5)) +
  ggtitle("ActiveStuPart Ratio with Symptoms by Bodysite") + labs(fill= "") + 
  scale_fill_manual("", values = c("ActiveStuPart without Symptoms" = "red",
                                   "ActiveStuPart[SymYes][Eyes]" = "deepskyblue3",
                                   "ActiveStuPart[SymYes][Nose]" = "darkgoldenrod2",
                                   "ActiveStuPart[SymYes][Mouth/Throat]" = "seagreen3",
                                   "ActiveStuPart[SymYes][Gastrointestinal Tract]" = "lightslateblue",
                                   "ActiveStuPart[SymYes][Lungs]" = "pink3",
                                   "ActiveStuPart[SymYes][Skin]" = "peachpuff2")) + theme_bw() + scale_y_continuous(limit = c(0, 6), labels=scales::percent)


# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title
# scale_y... -> set limits for the y-axis

#V2: 100% -> Total of all Symptoms

# show a stacked bar chart with the daily symptoms per bodysite
nbrOfObs <- nrow(activeStuPartIdDate_df)
days <- c(1:nbrOfObs) #c(activeStuPartIdDate_df$Date)

symYesNose <-c(noseObs_IdDate_dfV2$`sum[SymYes][Nose]`)
symYesEyes <-c(eyeObs_IdDate_dfV2$`sum[SymYes][Eyes]`)
symYesMouthThroat <-c(mouthThroatObs_IdDate_dfV2$`sum[SymYes][Mouth/Throat]`)
symYesGastrointestinalTract <-c(gastrointestinalTractObs_IdDate_dfV2$`sum[SymYes][Gastrointestinal Tract]`)
symYesSkin <-c(skinObs_IdDate_dfV2$`sum[SymYes][Skin]`)
symYesLungs <-c(lungObs_IdDate_dfV2$`sum[SymYes][Lungs]`)


values <- c(symYesEyes, symYesNose, symYesMouthThroat, symYesGastrointestinalTract, symYesSkin, symYesLungs)


type <- c(rep("SymYes[Eyes]", nbrOfObs), rep("SymYes[Nose]", nbrOfObs), 
          rep("SymYes[Mouth/Throat]", nbrOfObs), rep("SymYes[Gastrointestinal Tract]", nbrOfObs), rep("SymYes[Skin]", nbrOfObs), rep("SymYes[Lungs]", nbrOfObs))

data <- data.frame(days, values)


library(ggplot2)

p <- ggplot(data, aes(days, values))


p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Days") + ylab("Symptoms per Bodysite in %") +
  #geom_text(aes(label = sprintf("%0.1f%%", percent)), position = position_stack(vjust = 0.5)) +
  ggtitle("Frequency of Symptoms by Bodysite") + labs(fill= "") +
  scale_fill_manual("", values = c("SymYes[Eyes]" = "deepskyblue3",
                                   "SymYes[Nose]" = "darkgoldenrod2",
                                   "SymYes[Mouth/Throat]" = "seagreen3",
                                   "SymYes[Gastrointestinal Tract]" = "lightslateblue",
                                   "SymYes[Lungs]" = "pink3",
                                   "SymYes[Skin]" = "peachpuff2")) + theme_bw() + scale_y_continuous(labels=scales::percent)


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

