### SARIAR
# Load library
library(jsonlite)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

res <- fromJSON("study_fullExport (dummy).json", simplifyDataFrame = TRUE) # simplifyDataFrame = true is default

# subset auf Observation
observation <- subset(res$entry$resource, res$entry$resource$resourceType == "Observation")
observation$resourceType


x <- observation$code$coding
# class(x) -> list


y <- matrix(unlist(sapply(x, as.data.frame)),ncol=3, byrow=T)
# class(y) -> matrix

# Get Code an Display
y[,2:3]
y[,2]

# Subset only allergy-to-pollen Observation with Coding 300910009
allergy_to_pollen <- subset(observation, y[,2] == 300910009)
class(allergy_to_pollen)


