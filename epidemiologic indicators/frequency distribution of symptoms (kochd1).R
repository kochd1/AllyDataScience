# author: Dominik R. Kocher (kochd1)

# script for the indicator «Frequency distribution of symptoms»

#TODO: uniqueSym does only show one bodysite symptom per stuPart -> more than one pie/bar chart

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
bodySiteCoding <- uniqueSymYes$bodySite$coding
bodySiteCodingV2 <- uniqueSymYes$bodySite$coding

bodySiteCodes <- matrix(unlist(sapply(bodySiteCoding, as.data.frame)),ncol=3, byrow=T)
bodySiteCodesV2 <- matrix(unlist(sapply(bodySiteCodingV2, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Get Code an Display
#y[,2:3] #text and code
bodySiteCodes[,2]
bodySiteCodesV2[,2]

# Filter all stuPart (unique observations) by bodysite (six in total)
noseSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 45206002) #& (uniqueSymYes$effectiveDateTime >="2018-04-11" & uniqueSymYes$effectiveDateTime <"2018-04-12"))
# AllStuPartSymYesNose <- length(noseSymptoms_df$id)

eyeSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 81745001)
# AllStuPartSymYesEyes <- length(eyeSymptoms_df$id)

mouthThroatSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 312533001)
# AllStuPartSymYesEyes <- length(eyeSymptoms_df$id)

gastrointestinalTractSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 122865005)
# AllStuPartSymYesEyes <- length(eyeSymptoms_df$id)

skinSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 39937001)
# AllStuPartSymYesSkin <- length(skinSymptoms_df$id)

lungSymptoms_df <- subset(uniqueSymYes, bodySiteCodes[,2] == 39607008)
# AllStuPartSymYesLungs <- length(lungSymptoms_df$id)

# create specific dataframes for the amount of unique Observations per bodysite per day
noseObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
eyeObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
mouthThroatObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
gastrointestinalTractObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
skinObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)
lungObs_IdDate_df <- data.frame(ncol = 2, byrow = TRUE)

# create a specific dataframe for the daily number of "ActiveStuPart".
activeStuPartIdDate_df <- data.frame(ncol= 2, byrow = TRUE)

# definition of a specific timespan (month, pollenspecific period, pollen season in total etc.)
beginDate <- as.Date.character("2018-04-01")

endDate <- as.Date.character("2018-05-31")

date <- beginDate


while(date <= endDate){


  # shorten the effectiveDateTime to a comparable format
  dateXNose<- as.Date(noseSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXEyes<- as.Date(eyeSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXMouthThroat<- as.Date(mouthThroatSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXGastrointestinalTract<- as.Date(gastrointestinalTractSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXSkin<- as.Date(skinSymptoms_df$effectiveDateTime, format="%Y-%m-%d")
  dateXLung<- as.Date(lungSymptoms_df$effectiveDateTime, format="%Y-%m-%d")

  dateXActiveStuPart <- as.Date(uniqueSym$effectiveDateTime, format="%Y-%m-%d")

  # Build subsets for each day during the timespan
  noseObs_Id <- subset(noseSymptoms_df$id, dateXNose == date)
  eyeObs_Id <- subset(eyeSymptoms_df$id, dateXEyes == date)
  mouthThroatObs_Id <- subset(mouthThroatSymptoms_df$id, dateXMouthThroat == date)
  gastrointestinalTractObs_Id <- subset(gastrointestinalTractSymptoms_df$id, dateXGastrointestinalTract == date)
  skinObs_Id <- subset(skinSymptoms_df$id, dateXSkin == date)
  lungObs_Id <- subset(lungSymptoms_df$id, dateXLung == date)

  activeStuPart_Id <- subset(uniqueSym$id, dateXActiveStuPart == date)

  # Get the number of entries
  noseObs_Id_length <- length(noseObs_Id)
  eyeObs_Id_length <- length(eyeObs_Id)
  mouthThroatObs_Id_length <- length(mouthThroatObs_Id)
  gastrointestinalTractObs_Id_length <- length(gastrointestinalTractObs_Id)
  skinObs_Id_length <- length(skinObs_Id)
  lungObs_Id_length <- length(lungObs_Id)

  activeStuPart_Id_length <- length(activeStuPart_Id)

  # Save this number in a vector for each day
  noseObs_IdDate_vector <- c(noseObs_Id_length, date)
  eyeObs_IdDate_vector <- c(eyeObs_Id_length, date)
  mouthThroatObs_IdDate_vector <- c(mouthThroatObs_Id_length, date)
  gastrointestinalTractObs_IdDate_vector <- c(gastrointestinalTractObs_Id_length, date)
  skinObs_IdDate_vector <- c(skinObs_Id_length, date)
  lungObs_IdDate_vector <- c(lungObs_Id_length, date)

  activeStuPart_IdDate_vector <- c(activeStuPart_Id_length, as.character(date))

  # Combine the vectors with the predefined dataframes
  noseObs_IdDate_df <- rbind(noseObs_IdDate_df, noseObs_IdDate_vector)
  eyeObs_IdDate_df <- rbind(eyeObs_IdDate_df, eyeObs_IdDate_vector)
  mouthThroatObs_IdDate_df <- rbind(mouthThroatObs_IdDate_df, mouthThroatObs_IdDate_vector)
  gastrointestinalTractObs_IdDate_df <- rbind(gastrointestinalTractObs_IdDate_df, gastrointestinalTractObs_IdDate_vector)
  skinObs_IdDate_df <- rbind(skinObs_IdDate_df, skinObs_IdDate_vector)
  lungObs_IdDate_df <- rbind(lungObs_IdDate_df, lungObs_IdDate_vector)

  activeStuPartIdDate_df <- rbind(activeStuPartIdDate_df, activeStuPart_IdDate_vector)

    date <- date + 1
    }

# delete first row in matrix
noseObs_IdDate_df <- noseObs_IdDate_df[-1,]
eyeObs_IdDate_df <- eyeObs_IdDate_df[-1,]
mouthThroatObs_IdDate_df <- mouthThroatObs_IdDate_df[-1,]
gastrointestinalTractObs_IdDate_df <- gastrointestinalTractObs_IdDate_df[-1,]
skinObs_IdDate_df <- skinObs_IdDate_df[-1,]
lungObs_IdDate_df <- lungObs_IdDate_df[-1,]

activeStuPartIdDate_df <- activeStuPartIdDate_df[-1,]

# name the columns of the different bodysite df
colnames(noseObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Nose]", "Date")
print(noseObs_IdDate_df)

colnames(eyeObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Eyes]", "Date")
print(eyeObs_IdDate_df)

colnames(mouthThroatObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Mouth/Throat]", "Date")
print(mouthThroatObs_IdDate_df)

colnames(gastrointestinalTractObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Gastrointestinal Tract]", "Date")
print(gastrointestinalTractObs_IdDate_df)

colnames(skinObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Skin]", "Date")
print(skinObs_IdDate_df)

colnames(lungObs_IdDate_df) <- c("sumActiveStuPart[SymYes][Lungs]", "Date")
print(lungObs_IdDate_df)

# name the column of the "ActiveStuPart" df
colnames(activeStuPartIdDate_df) <- c("sumActiveStuPart", "Date")
print(activeStuPartIdDate_df)

# make sumActiveStuPart[SymYes]["Bodysite"] to integer
noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]` <- as.integer(noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]`)
eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]` <- as.integer(eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]`)
mouthThroatObs_IdDate_df$`sumActiveStuPart[SymYes][Mouth/Throat]` <- as.integer(mouthThroatObs_IdDate_df$`sumActiveStuPart[SymYes][Mouth/Throat]`)
gastrointestinalTractObs_IdDate_df$`sumActiveStuPart[SymYes][Gastrointestinal Tract]` <- as.integer(gastrointestinalTractObs_IdDate_df$`sumActiveStuPart[SymYes][Gastrointestinal Tract]`)
skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]` <- as.integer(skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]`)
lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]` <- as.integer(lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]`)

activeStuPartIdDate_df$sumActiveStuPart <- as.integer(activeStuPartIdDate_df$sumActiveStuPart)

#build the mean of all unique bodysite observations
MeanNoseObs <- mean(noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]`)
MeanEyesObs <- mean(eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]`)
MeanMouthThroatObs <- mean(mouthThroatObs_IdDate_df$`sumActiveStuPart[SymYes][Mouth/Throat]`)
MeanGastrointestinalTractObs <- mean(gastrointestinalTractObs_IdDate_df$`sumActiveStuPart[SymYes][Gastrointestinal Tract]`)
MeanSkinObs <- mean(skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]`)
MeanLungsObs <- mean(lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]`)

# Calculation
noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]` / activeStuPartIdDate_df$sumActiveStuPart
eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]` / activeStuPartIdDate_df$sumActiveStuPart
mouthThroatObs_IdDate_df$`sumActiveStuPart[SymYes][Mouth/Throat]` / activeStuPartIdDate_df$sumActiveStuPart
gastrointestinalTractObs_IdDate_df$`sumActiveStuPart[SymYes][Gastrointestinal Tract]` / activeStuPartIdDate_df$sumActiveStuPart
skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]` / activeStuPartIdDate_df$sumActiveStuPart
lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]` / activeStuPartIdDate_df$sumActiveStuPart

# Visualization

# show a stacked bar chart with the daily symptoms per bodysite
nbrOfObs <- nrow(activeStuPartIdDate_df)
days <- c(1:nbrOfObs) #c(activeStuPartIdDate_df$Date)

activeStuPart <- c(activeStuPartIdDate_df$sumActiveStuPart
                   -noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]`
                   -eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]`
                   -mouthThroatObs_IdDate_df$`sumActiveStuPart[SymYes][Mouth/Throat]`
                   -gastrointestinalTractObs_IdDate_df$`sumActiveStuPart[SymYes][Gastrointestinal Tract]`
                   -skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]`
                   -lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]`) #subtraction necessary due to correct visualization

activeStuPartSymYesNose <-c(noseObs_IdDate_df$`sumActiveStuPart[SymYes][Nose]`)
activeStuPartSymYesEyes <-c(eyeObs_IdDate_df$`sumActiveStuPart[SymYes][Eyes]`)
activeStuPartSymYesMouthThroat <-c(mouthThroatObs_IdDate_df$`sumActiveStuPart[SymYes][Mouth/Throat]`)
activeStuPartSymYesGastrointestinalTract <-c(gastrointestinalTractObs_IdDate_df$`sumActiveStuPart[SymYes][Gastrointestinal Tract]`)
activeStuPartSymYesSkin <-c(skinObs_IdDate_df$`sumActiveStuPart[SymYes][Skin]`)
activeStuPartSymYesLungs <-c(lungObs_IdDate_df$`sumActiveStuPart[SymYes][Lungs]`)

values <- c(activeStuPart, activeStuPartSymYesNose, activeStuPartSymYesEyes, activeStuPartSymYesMouthThroat, activeStuPartSymYesGastrointestinalTract, activeStuPartSymYesSkin, activeStuPartSymYesLungs)

type <- c(rep("ActiveStuPart", nbrOfObs), rep("ActiveStuPart[SymYes][Nose]", nbrOfObs), rep("ActiveStuPart[SymYes][Eyes]", nbrOfObs)
          ,rep("ActiveStuPart[SymYes][Mouth/Throat]", nbrOfObs), rep("ActiveStuPart[SymYes][Gastrointestinal Tract]", nbrOfObs), rep("ActiveStuPart[SymYes][Skin]", nbrOfObs), rep("ActiveStuPart[SymYes][Lungs]", nbrOfObs))

data <- data.frame(days, values)

library(ggplot2)

p <- ggplot(data, aes(days, values))
p +geom_bar(stat= "identity", position = "fill", aes(fill = type)) + xlab("Days") + ylab("Symptoms per Bodysite in %") + 
  ggtitle("Frequency of the symptoms per bodysite") + labs(fill= "") + 
  scale_fill_manual("", values = c("ActiveStuPart" = "red","ActiveStuPart[SymYes][Eyes]" = "deepskyblue3",
                                   "ActiveStuPart[SymYes][Nose]" = "darkgoldenrod2",
                                   "ActiveStuPart[SymYes][Mouth/Throat]" = "indianred2",
                                   "ActiveStuPart[SymYes][Gastrointestinal Tract]" = "lightslateblue",
                                   "ActiveStuPart[SymYes][Lungs]" = "pink3",
                                   "ActiveStuPart[SymYes][Skin]" = "peachpuff2")) + theme_bw()

# notes for plotting: position = "dodge" -> not stacked, position = "fill" -> stacked percent bar plot, labs(fill="") -> removes the legend title

#show a pie chart with the mean values of all bodysites in a timespan of 30 days
meanVector <- c(MeanNoseObs, MeanEyesObs, MeanMouthThroatObs, MeanGastrointestinalTractObs, MeanSkinObs, MeanLungsObs)
pct <- round(meanVector/sum(meanVector)*100)
lbls <- c("Nose", "Eyes", "Mouth/Throat", "Gastrointestinal Tract", "Skin", "Lungs")
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

# still searching for a simple alternative in ggplot
pie(meanVector, labels = lbls, main="Anteil ActiveStuPart mit Symptomen nach Körperteil (Mittelwert April-Mai)
", col=rainbow(length(meanVector)))
