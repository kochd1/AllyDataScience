# Thierry
# Nutzung der App
# Load library
library(jsonlite)
library(ggplot2)
#Test function "fromJSON"
all.equal(mtcars, fromJSON(toJSON(mtcars)))


###   1 INPUT ###
# name of the JSON-File (studydata)
inputFile <- "study_fullExport (dummy).json"

# Date of beginning as YYYY-MM-DD
beginDate <- "2018-04-01" # Fill in
beginDate <- as.Date.character(beginDate)

# Date of end as YYYY-MM-DD
endDate <- "2018-04-30" # Fill in
endDate <- as.Date.character(endDate)

# calculate the number of days
numOfDay <- endDate - (beginDate - 1)
numOfDay <- as.integer(numOfDay)



###   2 PREPERATION ###
# Load JSON and convert to dataframe
mydata <- fromJSON(inputFile, simplifyDataFrame = TRUE)



###   3 GET SUM OF STUDY PARTICIPANTS AT MONTH X ###
# Get all patient
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')
length(stuPart$resourceType)

# Generate Dataframe, attribut =  {sum_StuPart, Date}
stuPart_IdDate_df <- data.frame(sumStuPart = 0, date = 0, day = 0) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2

# Loop throught the month
date <- beginDate
while(date <= endDate) {
  # subset all StuPart which has been registrated at the date or earlier
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

# delete first row in matrix
stuPart_IdDate_df <- stuPart_IdDate_df[-1,]

# make sumStuPart to integer
stuPart_IdDate_df$sumStuPart <- as.integer(stuPart_IdDate_df$sumStuPart)

# make day to factor
stuPart_IdDate_df$day <- as.factor(stuPart_IdDate_df$day)

# The date is a number as type character
# make date to the format YYYY-MM-DD
stuPart_IdDate_df$date <- as.integer(stuPart_IdDate_df$date)
stuPart_IdDate_df$date <- as.Date.numeric(stuPart_IdDate_df$date, origin = "1970-01-01")

# show the dataframe
stuPart_IdDate_df



###   4 GET SUM OF ALL ACTIVE STUDY PARTICIPANTS AT MONTH X ###
# Get Observation-Ressources
observation <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == "Observation")

# GET ALLERGY-TO-POLLEN OBSERVATION
x <- observation$code$coding # class(x) -> list

y <- matrix(unlist(sapply(x, as.data.frame)),ncol=3, byrow=T) # class(y) -> matrix

# Subset only allergy-to-pollen Observation with Coding 300910009
allergy_to_pollen <- subset(observation, y[,2] == 300910009) # class(allergy_to_pollen) -> data frame

# Generate Dataframe, attribut =  {sum_activeStuPart, Date}
activeStuPart_IdDate_df <- data.frame(activeStuPart = 0, date = 0) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2

# Loop throught the month
date <- beginDate
while(date <= endDate) {
  #  PREPARE THE DATE
  date_min <- date
  date_max <- as.Date(date_min) + 1

  ##  GET ALLERGY-TO-POLLEN OBSERVATION FROM ONE SPECIFIC DAY 
  allergy_to_pollen_time <- subset(allergy_to_pollen, allergy_to_pollen$effectiveDateTime >= date_min & allergy_to_pollen$effectiveDateTime < date_max)
  
  # Get length of allergy_to_pollen
  df_length <- length(allergy_to_pollen_time$id)
  
  # generate empty character vector
  patIdVector <- character()
  
  ##  INSERT ACTIVE STUDY PARTICIPANT INTO DATA FRAME
  # if there is any active stuPart at this time, insert into dataframe
  if(df_length > 0) {
    # fill vector patIdVector with all Patienten References
    for (z in 1:df_length) {
      patIdVector[z] <- allergy_to_pollen_time$meta$extension[[z]]$extension[[1]]$valueReference$reference[2]
    }
    
    # eliminate the dublicates
    uniquePatIdVector <- unique(patIdVector)
    
    #get the length of the active Stu Part
    activeStuPart_length <- length(uniquePatIdVector)
    
    # Generate vector with activeStuPart_length and date
    activeStuPart_idDate_vector <- c(activeStuPart_length, date_min)
    
    # Add vector to dataframe
    activeStuPart_IdDate_df <- rbind(activeStuPart_IdDate_df, activeStuPart_idDate_vector)
    date <- date + 1
  } else {
    # generate default vector
    default_vector <- c(0, date_min)
    
    # add vector to dataframe
    activeStuPart_IdDate_df <- rbind(activeStuPart_IdDate_df, default_vector)
    date <- date + 1
  }
}

# delete the first row 
activeStuPart_IdDate_df <- activeStuPart_IdDate_df[-1,]

# make sumActiveStuPart to integer
activeStuPart_IdDate_df$sumActiveStuPart <- as.integer(activeStuPart_IdDate_df$sumActiveStuPart)

# The date is a number as type character
# make date to the format YYYY-MM-DD
activeStuPart_IdDate_df$date <- as.integer(activeStuPart_IdDate_df$date)
activeStuPart_IdDate_df$date <- as.Date.numeric(activeStuPart_IdDate_df$date, origin = "1970-01-01")

# show dataframe
activeStuPart_IdDate_df

###   5 CALCULATION ###
# calculate ratio between activeStuPart and StuPart
ratio <- activeStuPart_IdDate_df$sumActiveStuPart / stuPart_IdDate_df$sumStuPart
# generate joined dataframe
day <- c(1:numOfDay)
df <- cbind(stuPart_IdDate_df, activeStuPart_IdDate_df, day, ratio)
df <- df[,-2]
df$day <- as.factor(df$day)
df$Date <- as.Date.character(df$Date)

###   6 VISUALIZATION ###
if(days_of_month <= 31) {
  p <- ggplot(data = df, aes(x = df$day, y = df$sumActiveStuPart)) + geom_point()
  p <- p + labs(x = "Day", y = "sum of active StuPart", title = "Share of active StuPart")
  p <- p + theme_bw()
  p
} else {
  p <- ggplot(data = df, aes(x = df$Date, y = df$sumActiveStuPart)) + geom_point()
  p <- p + labs(x = "Period of time", y = "sum of active StuPart", title = "Share of active StuPart")
  p <- p + theme_bw()
  p
}
# create a dataset
specie=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
value=abs(rnorm(12 , 0 , 15))
data=data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar( stat="identity")


# Stacked Percent
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar( stat="identity", position="fill")



