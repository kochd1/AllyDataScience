# Thierry
# MIDATA Registrierungen (Ally Awareness)
# Zeigt, welcher Anteil der Bevölkerung an einem bestimmten Monat x an der Studie teilnimmt. 

###   1 INPUT ###
# YYYY-MM
year_month <- "2018-04"

# DD
days_of_month <- 30



###   2 PREPERATION ###
# Ständige Wohnbevölkerung: 8419550 Personen (Stand 2016, bfs Tabelle)
population = 8419550

# Load library
library(jsonlite)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

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



###   4 CALCULATION ###
stuPart_IdDate_df$sumStuPart / population



###   5 VISUALIZATION
barplot(stuPart_IdDate_df$sumStuPart)

stuPart_IdDate_df$sumStuPart <- stuPart_IdDate_df$sumStuPart / population

stuPart_IdDate_df$sumStuPart

ggplot(data = stuPart_IdDate_df, aes(x = stuPart_IdDate_df$Date, y = sumStuPart)) + geom_point()















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


