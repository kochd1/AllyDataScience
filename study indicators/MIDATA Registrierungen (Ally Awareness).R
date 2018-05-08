### MIDATA Registrierungen (Ally Awareness)
# Zeigt, welcher anteil der Bevölkerung an der Studie teilnimmt.
# Ständige Wohnbevölkerung: 8419550 Personen (Stand 2016, bfs Tabelle)
population = 8419550

# Load library
library(jsonlite)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

# Load JSON and convert to dataframe
mydata <- fromJSON("study_fullExport (dummy).json", simplifyDataFrame = TRUE)

# Get all patient
patient <- subset(mydata$entry$resource, mydata$entry$resource$resourceType == 'Patient')
length(patient$resourceType)

# Get all patient, which where registratet bevor 2018-04-22 (lastUpdated)
# lastUpdated: Einzige Möglichkeit, um eine Zeitangabe zu bekommen.
# falls jemand Profil via Website ändert, ist lastUpdated != Registration Daten 
patientApril <- subset(patient, patient$meta$lastUpdated <= "2018-04-23")
length(patientApril$resourceType)

# count the quantity of the patient by his id
anzahlStudienteilnehmer <- length(patient$id)
anzahlStudienteilnehmer
# str(patient) --> 11 obj. à 23 variables. variables: patient features

### Berechnung
anzahlStudienteilnehmer / population




### Visualisierung
x <- c(4,6,9,3,7)
barplot(x)
lines(x)

