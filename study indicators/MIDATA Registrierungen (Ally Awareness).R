# Thierry
### MIDATA Registrierungen (Ally Awareness)
# Zeigt, welcher anteil der Bevölkerung an der Studie teilnimmt.
# Ständige Wohnbevölkerung: 8419550 Personen (Stand 2016, bfs Tabelle)
population = 8419550

# Load library
library(jsonlite)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

# Load JSON and convert to dataframe
mydata <- fromJSON("study_fullExport (dummy).json", simplifyDataFrame = TRUE)


#####
# Get sum of all Study Participants at mounth x in a matrix
# Get all patient
stuPart <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')
length(stuPart$resourceType)

# Generate Dataframe, attribut =  {sum_StuPart, Date}
stuPart_IdDate_df <- data.frame(ncol = 2, byrow = TRUE) # Achtung, es gibt eine Matrix mit NA, NA -> length = 2


# YYYY-MM
year_mounth <- "2018-04"

# DD
days_of_mount <- 30

for(j in 1:days_of_mount) {
  if(j < 10) {
    j <- paste("0",j, sep = "")
  }
  date <- paste(year_mounth, j, sep = "-")
  
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


typeof(stuPart_Id_length)


### Berechnung
anzahlStudienteilnehmer / population

class(stuPart_IdDate_df$sumStuPart)






### Visualisierung
x <- c(4,6,9,3,7)
barplot(x)
lines(x)


















# Get all patient, which where registratet bevor 2018-04-22 (lastUpdated)
# lastUpdated: Einzige Möglichkeit, um eine Zeitangabe zu bekommen.
# falls jemand Profil via Website ändert, ist lastUpdated != Registration Daten

mydate <- "2018-04-10"

patientApril <- subset(stuPart, stuPart$meta$lastUpdated <= mydate)
length(patientApril$resourceType)

# count the quantity of the patient by his id
anzahlStudienteilnehmer <- length(stuPart$id)
anzahlStudienteilnehmer
# str(patient) --> 11 obj. à 23 variables. variables: patient features


