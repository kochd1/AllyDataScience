# Author: Thierry Schmidt
# Verteilung der Studienteilnehmer
# Load library
library(jsonlite)
library(ggplot2)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

###   1 INPUT ###
# Name of the JSON-File (studydata)
inputFile <- "study_fullExport (dummy).json"


# Reporting date
date <- "2018-04-30" # FILL IN
date <- as.Date.character(date)

# Name and Path of the Excel Table inlcuding the permanent resident population by postcode
zipExcelTable <- "study indicators/ZIP.xlsx"



###   2 DATA IMPORT ###
# Load JSON and convert to dataframe
mydata <- fromJSON(inputFile, simplifyDataFrame = TRUE)

# Load Excel Tab
library(readxl)
zip1 <- read_excel(zipExcelTable, 
                   sheet = "1-stelle")
View(zip1)

numOfPLZ <- length(zip1$zip)


###   3 GET STUDY PARTICIPANT / POPULATION RATIO AT DATE X DISTRIBUTED BY LIVING LOCATION ###
# Get all Study Participant
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')

# Get Study Participant at time X
# Subset all StuPart which has been registrated at the "date" or earlier
stuPart <- subset(stuPart, stuPart$meta$lastUpdated <= date)

# Generate data frame
distribution_df <- data.frame(zip = 0, population = 0, stuPart = 0, ratio = 0)


for(i in 1:numOfPLZ) {
  # Set y to 0 (stuPart counter)
  stuPartCounter = 0
  ratio = -1
  
  # Get number of people living in i (zip)
  popZip <- subset(zip1$count, zip1$zip == i)
  
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
  
  # Calculate ratio
  ratio <- stuPartCounter / popZip
  
  # Generate vector
  distribution_vector <- c(i, popZip, stuPartCounter, ratio)
  
  # Add to data frame
  distribution_df <- rbind(distribution_df, distribution_vector)
}

# Delete first row in data frame
distribution_df <- distribution_df[-1,]

# Make zip to factor
distribution_df$zip <- as.factor(distribution_df$zip)

# Multiplicat ratio with 1 MIO
distribution_df$ratio <- distribution_df$ratio * 1000000

mydf <- distribution_df
mydf$stuPart

###   4 VISUALIZATION ###
p <- ggplot(data = mydf, aes(x = mydf$zip, y = mydf$ratio, size = mydf$stuPart)) + geom_point()
p <- p + labs(x = "ZIP", y = "percentage of StuPart", title = "Distribution of study participants")
p + theme_bw()



p <- ggplot(data = distribution_df, aes(x = distribution_df$zip, y = distribution_df$stuPart)) + geom_bar(stat = "identity")
p <- p + labs(x = "ZIP", y = "Number of StuPart", title = "Distribution of study participants")
p + theme_bw()

# ggplot(data = distribution_df, aes(x = distribution_df$ratio, y = distribution_df$stuPart, color = distribution_df$zip)) + geom_point()

# ggplot(data = distribution_df, aes(x = distribution_df$zip, y = distribution_df$stuPart, size = distribution_df$ratio)) + geom_point()

# ggplot(data = distribution_df, aes(x = distribution_df$zip, y = 1, size = distribution_df$ratio)) + geom_point()

# ggplot(data = distribution_df, aes(x = distribution_df$population, y = distribution_df$ratio, color = distribution_df$zip)) + geom_point()




