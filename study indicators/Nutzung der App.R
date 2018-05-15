# Thierry
# Nutzung der App
# Load library
library(jsonlite)
library(ggplot2)
all.equal(mtcars, fromJSON(toJSON(mtcars)))


###   1 INPUT ###
# YYYY-MM
year_month <- "2018-04"

# DD
days_of_month <- 30



###   2 PREPERATION ###
# Load JSON and convert to dataframe
mydata <- fromJSON("study_fullExport (dummy).json", simplifyDataFrame = TRUE)



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

# delete first row in matrix
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
activeStuPart_IdDate_df <- data.frame(ncol = 2, byrow = TRUE) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2


# Loop throught the month
for(i in 1:days_of_month) {
  if(i < 10) {
    i <- paste("0",i, sep = "")
  }
  date_min <- paste(year_month, i, sep = "-")
  date_max <- as.Date(date_min) + 1
  
  # subset allergy-to-pollen Observation form one specific day 
  allergy_to_pollen_time <- subset(allergy_to_pollen, allergy_to_pollen$effectiveDateTime >= date_min & allergy_to_pollen$effectiveDateTime < date_max)
  
  # Get length of allergy_to_pollen
  df_length <- length(allergy_to_pollen_time$id)
  
  # generate empty character vector
  patIdVector <- character()
  
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
  activeStuPart_idDate_vector <- c(activeStuPart_length, date)
  
  # Add vector to dataframe
  activeStuPart_IdDate_df <- rbind(activeStuPart_IdDate_df, activeStuPart_idDate_vector)
  } else {
    # generate derault vector
    default_vector <- c(0, date)
    
    # add vector to dataframe
    activeStuPart_IdDate_df <- rbind(activeStuPart_IdDate_df, default_vector)
  }
}



print(activeStuPart_IdDate_df)

# delete the first row 
activeStuPart_IdDate_df <- activeStuPart_IdDate_df[-1,]

# name the column
colnames(activeStuPart_IdDate_df) <- c("sumActiveStuPat", "Date")

# make sumActiveStuPart to integer
activeStuPart_IdDate_df$sumActiveStuPat <- as.integer(activeStuPart_IdDate_df$sumActiveStuPat)



activeStuPart_IdDate_df



###   5 CALCULATION ###
indicator <- activeStuPart_IdDate_df$sumActiveStuPat / stuPart_IdDate_df$sumStuPart
indicator



###   6 VISUALIZATION ###
ReverseIndicator <- (1 - indicator)
visData <- c(indicator, ReverseIndicator)
visData_matrix <- matrix(visData)
m <- matrix(1:4, byrow = TRUE, nrow = 2)
names(m) <- c("x", "y")
m



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




















sumOfActiveStudyParticipants
sumOfStudyParticipants



### search Query to get the patientID in an observation
# first 1 has to change to i --> 1:36 for example
allergy_to_pollen$meta$extension[[1]]$extension[[1]]$valueReference$reference[2]



