# Thierry
# Nutzung der App
# Load library
library(jsonlite)
library(ggplot2)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

###   1 INPUT ###
# Date:




###   2 PREPERATION ###
# Load JSON and convert to dataframe
mydata <- fromJSON("study_fullExport (dummy).json", simplifyDataFrame = TRUE)

# Load Excel Tab
library(readxl)
zip1 <- read_excel("study indicators/ZIP.xlsx", 
                   sheet = "1-stelle")
View(zip1)



###   3 GET STUDY PARTICIPANT / POPULATION RATIO AT DATE X DISTRIBUTED BY LIVING LOCATION ###
# Get all Study Participant
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')

# Get Study Participant at time X


# generate data frame
distribution_df <- data.frame(zip = 0, population = 0, stuPart = 0, ratio = 0)


for(i in 1:9) {
  # set y to 0 (stuPart counter)
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


###   4 VISUALIZATION ###
ggplot(data = distribution_df, aes(x = distribution_df$zip, y = distribution_df$stuPart)) + geom_point()

ggplot(data = distribution_df, aes(x = distribution_df$ratio, y = distribution_df$stuPart, color = distribution_df$zip)) + geom_point()

ggplot(data = distribution_df, aes(x = distribution_df$zip, y = distribution_df$stuPart, size = distribution_df$ratio)) + geom_point()

ggplot(data = distribution_df, aes(x = distribution_df$zip, y = 1, size = distribution_df$ratio)) + geom_point()


ggplot(data = distribution_df, aes(x = distribution_df$population, y = distribution_df$ratio, color = distribution_df$zip)) + geom_point()

