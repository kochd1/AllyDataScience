# Author: Thierry Schmidt
# Nutzung der App
# Load library
library(jsonlite)
library(ggplot2)
#Test function "fromJSON"
all.equal(mtcars, fromJSON(toJSON(mtcars)))


###   1 INPUT ###
# Name of the JSON-File (studydata)
inputFile <- "study_fullExport (dummy).json"

# Date of beginning as YYYY-MM-DD
beginDate <- "2018-04-01" # Fill in
beginDate <- as.Date.character(beginDate)

# Date of end as YYYY-MM-DD
endDate <- "2018-04-30" # Fill in
endDate <- as.Date.character(endDate)

# Calculate the number of days
numOfDay <- endDate - (beginDate - 1)
numOfDay <- as.integer(numOfDay)



###   2 DATA IMPORT ###
# Load JSON and convert to dataframe
mydata <- fromJSON(inputFile, simplifyDataFrame = TRUE)



### 3 GET OBSERVATION-RESOURCES  ###
observation <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == "Observation")

# GET ALLERGY-TO-POLLEN OBSERVATION
x <- observation$code$coding # class(x) -> list

y <- matrix(unlist(sapply(x, as.data.frame)),ncol=3, byrow=T) # class(y) -> matrix

# Subset only allergy-to-pollen Observation with Coding 300910009
allergy_to_pollen <- subset(observation, y[,2] == 300910009) # class(allergy_to_pollen) -> data frame



###   4 GET SUM OF STUDY PARTICIPANTS AT MONTH X ###
# Generate Dataframe, attribut =  {sumStuPart, date}
stuPart_IdDate_df <- data.frame(sumStuPart = 0, date = 0) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2

# Loop throught the month
date <- beginDate
while(date <= endDate) {
  ##  GET ALLERGY-TO-POLLEN OBSERVATION until date  
  stuPart_time <- subset(allergy_to_pollen, as.Date(allergy_to_pollen$effectiveDateTime) <= date)
  
  # Get length of allergy_to_pollen
  df_length <- length(stuPart_time$id)
  
  # Generate empty character vector
  patIdVector <- character()
  
  ##  INSERT ACTIVE STUDY PARTICIPANT INTO DATA FRAME
  # If there is any active stuPart at this time, insert into dataframe
  if(df_length > 0) {
    # Fill vector patIdVector with all Patienten References
    for (z in 1:df_length) {
      patIdVector[z] <- stuPart_time$meta$extension[[z]]$extension[[1]]$valueReference$reference[2]
    }
    
    # Eliminate the dublicates
    uniquePatIdVector <- unique(patIdVector)
    
    #Get the length of the active Stu Part
    stuPart_length <- length(uniquePatIdVector)
    
    # Generate vector with activeStuPart_length and date
    stuPart_idDate_vector <- c(stuPart_length, date)
    
    # Add vector to dataframe
    stuPart_IdDate_df <- rbind(stuPart_IdDate_df, stuPart_idDate_vector)
    date <- date + 1
  } else {
    # Generate default vector
    default_vector <- c(0, date)
    
    # Add vector to dataframe
    activeStuPart_IdDate_df <- rbind(stuPart_IdDate_df, default_vector)
    date <- date + 1
  }
}

# Delete the first row 
stuPart_IdDate_df <- stuPart_IdDate_df[-1,]

# Make sumStuPart to integer
stuPart_IdDate_df$sumStuPart <- as.integer(stuPart_IdDate_df$sumStuPart)

# The date is a number as type character
# Make date to the format YYYY-MM-DD
stuPart_IdDate_df$date <- as.integer(stuPart_IdDate_df$date)
stuPart_IdDate_df$date <- as.Date.numeric(stuPart_IdDate_df$date, origin = "1970-01-01")



###   5 GET SUM OF ALL ACTIVE STUDY PARTICIPANTS AT MONTH X ###
# Generate Dataframe, attribut =  {sumActiveStuPart, date}
activeStuPart_IdDate_df <- data.frame(sumActiveStuPart = 0, date = 0) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2

# Loop throught the month
date <- beginDate
while(date <= endDate) {
  ##  GET ALLERGY-TO-POLLEN OBSERVATION FROM ONE SPECIFIC DAY 
  allergy_to_pollen_time <- subset(allergy_to_pollen, as.Date(allergy_to_pollen$effectiveDateTime) == date)
  
  # Get length of allergy_to_pollen
  df_length <- length(allergy_to_pollen_time$id)
  
  # Generate empty character vector
  patIdVector <- character()
  
  ##  INSERT ACTIVE STUDY PARTICIPANT INTO DATA FRAME
  # If there is any active stuPart at this time, insert into dataframe
  if(df_length > 0) {
    # Fill vector patIdVector with all Patienten References
    for (z in 1:df_length) {
      patIdVector[z] <- allergy_to_pollen_time$meta$extension[[z]]$extension[[1]]$valueReference$reference[2]
    }
    
    # Eliminate the dublicates
    uniquePatIdVector <- unique(patIdVector)
    
    #Get the length of the active Stu Part
    activeStuPart_length <- length(uniquePatIdVector)
    
    # Generate vector with activeStuPart_length and date
    activeStuPart_idDate_vector <- c(activeStuPart_length, date)
    
    # Add vector to dataframe
    activeStuPart_IdDate_df <- rbind(activeStuPart_IdDate_df, activeStuPart_idDate_vector)
    date <- date + 1
  } else {
    # Generate default vector
    default_vector <- c(0, date)
    
    # Add vector to dataframe
    activeStuPart_IdDate_df <- rbind(activeStuPart_IdDate_df, default_vector)
    date <- date + 1
  }
}

# Delete the first row 
activeStuPart_IdDate_df <- activeStuPart_IdDate_df[-1,]

# Make sumActiveStuPart to integer
activeStuPart_IdDate_df$sumActiveStuPart <- as.integer(activeStuPart_IdDate_df$sumActiveStuPart)

# The date is a number as type character
# Make date to the format YYYY-MM-DD
activeStuPart_IdDate_df$date <- as.integer(activeStuPart_IdDate_df$date)
activeStuPart_IdDate_df$date <- as.Date.numeric(activeStuPart_IdDate_df$date, origin = "1970-01-01")

# Show dataframe
activeStuPart_IdDate_df
stuPart_IdDate_df

###   6 CALCULATION ###
# Calculate ratio between activeStuPart and StuPart
ratio <- activeStuPart_IdDate_df$sumActiveStuPart / stuPart_IdDate_df$sumStuPart
# Generate joined dataframe
day <- c(1:numOfDay)
mydf <- cbind(stuPart_IdDate_df, activeStuPart_IdDate_df, day, ratio)

mydf <- mydf[,-2]
mydf$day <- as.factor(mydf$day)
mydf$date <- as.Date.character(mydf$date)
mydf

###   7 VISUALIZATION ###
if(numOfDay <= 31) {
  p <- ggplot(data = mydf, aes(x = mydf$day, y = mydf$ratio)) + geom_point()
  p <- p + labs(x = "Day", y = "share of active StuPart", title = "Active use of the app")
  p <- p + theme_bw()
  p + ylim(0,1)
} else {
  p <- ggplot(data = mydf, aes(x = mydf$date, y = mydf$ratio)) + geom_point()
  p <- p + labs(x = "Period of time", y = "share of active StuPart", title = "Active use of the app")
  p <- p + theme_bw()
  p + ylim(0,1)
}


#april <- c(4,5,3,6,2,6,8,6,9,6,8,2,4,7,9,5,7,2)
#month <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)
#abc <- cbind(april, month)
#abc <- as.data.frame(abc)
#str(abc)
#mai <- c(6,8,2,4,7,9,5,7,2)
#abc
#ggplot(data = abc, aes(x = abc$month, y = abc$april)) + geom_smooth()
