### SARIAR
# Load library
library(jsonlite)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

res <- fromJSON("study_fullExport (dummy).json", simplifyDataFrame = TRUE) # simplifyDataFrame = true is default

# subset auf Observation
observation <- subset(res$entry$resource, res$entry$resource$resourceType == "Observation")
observation$resourceType


x <- observation$code$coding

class(x)
class(unlist(x))

y <- matrix(unlist(sapply(x, as.data.frame)),ncol=3, byrow=T)

y[,3]
observation$code$coding

allergyIntolerance <- subset(observation, observation$code$coding == 300910009)


