# Thierry
# Nutzung der App
# Load library
library(jsonlite)
library(ggplot2)
all.equal(mtcars, fromJSON(toJSON(mtcars)))


###   1 INPUT ###
# name of the JSON-File (studydata)
inputFile <- "study_fullExport (dummy).json"

# YYYY-MM
year_month <- "2018-04"

# DD
days_of_month <- 30



###   2 PREPERATION ###
# Load JSON and convert to dataframe
mydata <- fromJSON(inputFile, simplifyDataFrame = TRUE)



###   3 GET SUM OF STUDY PARTICIPANTS AT MONTH X ###
# Get all patient
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')
length(stuPart$resourceType)

# Generate Dataframe, attribut =  {sum_StuPart, Date}
stuPart_IdDate_df <- data.frame(ncol = 2, byrow = TRUE) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2

# Loop throught the month
for(j in 1:days_of_month) {
  if(j < 10) {
    j <- paste("0",j, sep = "")
  }
  date <- paste(year_month, j, sep = "-")
  
  stuPart_Id <- subset(stuPart$id, stuPart$meta$lastUpdated <= date)
  stuPart_Id_length <- length(stuPart_Id)
  stuPart_IdDate_vector <- c(stuPart_Id_length, date)
  
  stuPart_IdDate_df <- rbind(stuPart_IdDate_df, stuPart_IdDate_vector)
}

# delete first row in data frame
stuPart_IdDate_df <- stuPart_IdDate_df[-1,]

# name the columns
colnames(stuPart_IdDate_df) <- c("sumStuPart", "Date")
print(stuPart_IdDate_df)

# make sumStuPart to integer
stuPart_IdDate_df$sumStuPart <- as.integer(stuPart_IdDate_df$sumStuPart)




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
for(i in 1:days_of_month) {
  ##  PREPARE THE DATE
  if(i < 10) {
    i <- paste("0",i, sep = "")
  }
  date_min <- paste(year_month, i, sep = "-")
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
  } else {
    # generate default vector
    default_vector <- c(0, date_min)
    
    # add vector to dataframe
    activeStuPart_IdDate_df <- rbind(activeStuPart_IdDate_df, default_vector)
  }
}



print(activeStuPart_IdDate_df)

# delete the first row 
activeStuPart_IdDate_df <- activeStuPart_IdDate_df[-1,]

# name the column
colnames(activeStuPart_IdDate_df) <- c("sumActiveStuPart", "Date")

# make sumActiveStuPart to integer
activeStuPart_IdDate_df$sumActiveStuPat <- as.integer(activeStuPart_IdDate_df$sumActiveStuPat)



###   5 CALCULATION ###

activeStuPart_IdDate_df
stuPart_IdDate_df
ratio <- activeStuPart_IdDate_df$sumActiveStuPat / stuPart_IdDate_df$sumStuPart
day <- c(1:days_of_month)

df <- cbind(stuPart_IdDate_df, activeStuPart_IdDate_df, day, ratio)
df <- df[,-2]
df


###   6 VISUALIZATION ###
ggplot(data = df, aes(x = df$day, y = df$ratio)) + geom_point()



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




### search Query to get the patientID in an observation
# first 1 has to change to i --> 1:36 for example
allergy_to_pollen$meta$extension[[1]]$extension[[1]]$valueReference$reference[2]



