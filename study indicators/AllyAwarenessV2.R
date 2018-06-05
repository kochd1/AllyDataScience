# Author: Thierry Schmidt
# MIDATA Registrierungen (Ally Awareness)
# Zeigt, welcher Anteil der Bevölkerung an einem bestimmten Monat x an der Studie teilnimmt. 

###   1 INPUT ###
# name of the JSON-File (studydata)
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
# Ständige Wohnbevölkerung: 8419550 Personen (Stand 2016, bfs Tabelle)
population = 8419550

# Load library
library(jsonlite)
library(ggplot2)
#Test function "fromJSON"
all.equal(mtcars, fromJSON(toJSON(mtcars)))

# Load JSON and convert to dataframe
mydata <- fromJSON(inputFile, simplifyDataFrame = TRUE)



###   3 GET SUM OF STUDY PARTICIPANTS AT MONTH X ###
# Get all patient
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')


# Generate Dataframe, attribut =  {sum_StuPart, Date, day}
stuPart_IdDate_df <- data.frame(sumStuPart = 0, date = 0, day = 0) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2

# Loop throught the month
date <- beginDate
while(date <= endDate) {
  # Subset all StuPart which has been registrated at the date or earlier
  stuPart_Id <- subset(stuPart$id, stuPart$meta$lastUpdated <= date)
  # Get sum of these subseted StuPart
  stuPart_Id_length <- length(stuPart_Id)
  # Get the day of the date
  day <- format(date, format = "%d")
  # Generate vector with attributs = {sumOfStuPart, Date, day}
  dateX <- as.Date(date, origin="1970-01-01")
  stuPart_IdDate_vector <- c(stuPart_Id_length, date, day)
  # Bind vector to dataframe
  stuPart_IdDate_df <- rbind(stuPart_IdDate_df, stuPart_IdDate_vector)
  
  date <- date + 1
}


# Delete first row in matrix
stuPart_IdDate_df <- stuPart_IdDate_df[-1,]

# Make sumStuPart to integer
stuPart_IdDate_df$sumStuPart <- as.integer(stuPart_IdDate_df$sumStuPart)

# Make day to factor
stuPart_IdDate_df$day <- as.factor(stuPart_IdDate_df$day)

# The date is a number as type character
# Make date to the format YYYY-MM-DD
stuPart_IdDate_df$date <- as.integer(stuPart_IdDate_df$date)
stuPart_IdDate_df$date <- as.Date.numeric(stuPart_IdDate_df$date, origin = "1970-01-01")

# Show the dataframe
stuPart_IdDate_df


###   4 CALCULATION ###
allyAwareness_df <- stuPart_IdDate_df
indicator <- stuPart_IdDate_df$sumStuPart / population
allyAwareness_df <- cbind(allyAwareness_df, indicator)

str(allyAwareness_df)


###   5 VISUALIZATION
## Plot
if(numOfDay <= 31) {
# ggplot shows the distribution of the study participants in one month.
  p <- ggplot(data = allyAwareness_df, aes(x = day, y = indicator)) + geom_point()
  p <- p + labs(x = "Day of the month", y = "Sum of StuPart / Population", title = "Ally Awareness")
  p + theme_bw()
} else {
# ggplot shows the distribution of the study participants in a period of time.
  p <- ggplot(data = allyAwareness_df, aes(x = date, y = indicator)) + geom_point()
  p <- p + labs(x = "period of time", y = "Sum of StuPart / Population", title = "Ally Awareness")
  p + theme_bw()
}

## Histogramm
if(numOfDay <= 31) {
  # ggplot shows the distribution of the study participants in one month.
  p <- ggplot(data = allyAwareness_df, aes(allyAwareness_df$sumStuPart)) + geom_histogram()
  p
  p <- p + labs(x = "Day of the month", y = "Sum of StuPart / Population", title = "Ally Awareness")
  p + theme_bw()
} else {
  # ggplot shows the distribution of the study participants in a period of time.
  p <- ggplot(data = allyAwareness_df, aes(x = date, y = indicator)) + geom_histogram()
  p <- p + labs(x = "period of time", y = "Sum of StuPart / Population", title = "Ally Awareness")
  p + theme_bw()
}