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

# count the quantity of the patient by his id
anzahlStudienteilnehmer <- length(patient$id)
anzahlStudienteilnehmer
# str(patient) --> 11 obj. à 23 variables. variables: patient features

### Berechnung
anzahlStudienteilnehmer / population


