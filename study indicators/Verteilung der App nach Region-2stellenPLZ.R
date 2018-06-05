# author: Thierry
# sript for Nutzung der App
# Load library
library(jsonlite)
library(ggplot2)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

###   1 INPUT ###
# name of the JSON-File (studydata)
inputFile <- "study_fullExport (dummy).json"


# reporting date
date <- "2018-04-30" # FILL IN
date <- as.Date.character(date)

# name and Path of the Excel Table inlcuding the permanent resident population by postcode
zipExcelTable <- "study indicators/ZIP.xlsx"



###   2 DATA IMPORT ###
# Load JSON and convert to dataframe
mydata <- fromJSON(inputFile, simplifyDataFrame = TRUE)

# Load Excel Tab
library(readxl)
zip2 <- read_excel("study indicators/ZIP.xlsx", 
                   sheet = "bakcUp2")
View(zip2)

numOfPLZ <- length(zip2$zip)
numOfPLZ <- numOfPLZ + 9


###   3 GET STUDY PARTICIPANT / POPULATION RATIO AT DATE X DISTRIBUTED BY LIVING LOCATION ###
# Get all Study Participant
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')

# Get Study Participant at time X
# subset all StuPart which has been registrated at the "date" or earlier
stuPart <- subset(stuPart, stuPart$meta$lastUpdated <= date)

# generate data frame
distribution_df <- data.frame(zip = 0, population = 0, stuPart = 0, ratio = 0)


for(i in 10:numOfPLZ) {
  # set y to 0 (stuPart counter)
  stuPartCounter = 0
  ratio = -1
  
  # Get number of people living in i (zip)
  popZip <- subset(zip2$count, zip2$zip == i)
  
  for(j in 1: length(stuPart$id)) {
    # TRUE, if stuPart living in zip i
    x <- substring(stuPart$address[[j]]$postalCode, 0,1) == i
    # Check, because not every StuPart has a living location
    if(length(x) != 0) {
      if(x) {
        stuPartCounter = stuPartCounter + 1
      }      
    }
  }
  
  # calculate ratio
  ratio <- stuPartCounter / popZip
  
  # generate vector
  distribution_vector <- c(i, popZip, stuPartCounter, ratio)
  
  # add to data frame
  distribution_df <- rbind(distribution_df, distribution_vector)
}

# delete first row in data frame
distribution_df <- distribution_df[-1,]

# make zip to factor
distribution_df$zip <- as.factor(distribution_df$zip)

# multiplicat ratio with 1 MIO
distribution_df$ratio <- distribution_df$ratio * 1000000

distribution_df


###   4 VISUALIZATION ###
ggplot(data = distribution_df, aes(x = distribution_df$zip, y = distribution_df$stuPart)) + geom_point()

ggplot(data = distribution_df, aes(x = distribution_df$ratio, y = distribution_df$stuPart, color = distribution_df$zip)) + geom_point()

ggplot(data = distribution_df, aes(x = distribution_df$zip, y = distribution_df$stuPart, size = distribution_df$ratio)) + geom_point()

ggplot(data = distribution_df, aes(x = distribution_df$zip, y = 1, size = distribution_df$ratio)) + geom_point()


ggplot(data = distribution_df, aes(x = distribution_df$population, y = distribution_df$ratio, color = distribution_df$zip)) + geom_point()




