### Thierry
# Nutzung der App
# Load library
library(jsonlite)
library(ggplot2)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

# Load JSON and convert to dataframe
mydata <- fromJSON("study_fullExport (dummy).json", simplifyDataFrame = TRUE)

#####
# Get all study participants
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')
sumOfStudyParticipants <- length(stuPart$resourceType)

#####
# Get all active study participants
# Get Observation - Ressources
observation <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == "Observation")

### Get allergy to pollen observation
x <- observation$code$coding
# class(x) -> list

y <- matrix(unlist(sapply(x, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Subset only allergy-to-pollen Observation with Coding 300910009
allergy_to_pollen <- subset(observation, y[,2] == 300910009)
# class(allergy_to_pollen) -> data frame

# Get length of allergy_to_pollen
df_length <- length(allergy_to_pollen$resourceType)

# generate empty character vector
patIdVector <- character()

# fill vector padIdVector with all Patienten References
for (i in 1:df_length) {
  patIdVector[i] <- allergy_to_pollen$meta$extension[[i]]$extension[[1]]$valueReference$reference[2]
}

# eliminate the dublicates
uniquePatIdVector <- unique(patIdVector)
sumOfActiveStudyParticipants <- length(uniquePatIdVector)

#####
# Calculate the indicator
indicator <- sumOfActiveStudyParticipants / sumOfStudyParticipants

### Visualization the indicator
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



