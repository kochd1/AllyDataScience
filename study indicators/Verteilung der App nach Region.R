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

# Load Excel Tab
library(readxl)
zip1 <- read_excel("study indicators/ZIP.xlsx", 
                   sheet = "1-stelle")
View(zip1)


