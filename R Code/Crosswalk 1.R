setwd("~/Arcadio/")

# Import CSV
vtdtract <-
  read.csv("vtdtract.csv", stringsAsFactors = FALSE)

# Induce blank VTD to NA
vtdtract$vtd <- as.numeric(vtdtract$vtd)

# Fill out missing values in long-format
for (i in 1:nrow(vtdtract)) {
  if (is.na(vtdtract$vtd[i])) {
    vtdtract$vtd[i] <- vtdtract$vtd[i - 1]
    vtdtract$county[i] <- vtdtract$county[i - 1]
    vtdtract$cntyname[i] <- vtdtract$cntyname[i - 1]
    vtdtract$Voting.District.Name[i] <-
      vtdtract$Voting.District.Name[i - 1]
  }
}

# Create counter variable
vtdtract$numtract <- 1
for (i in 2:nrow(vtdtract)) {
  if (vtdtract$vtd[i] == vtdtract$vtd[i - 1]) {
    vtdtract$numtract[i] <- vtdtract$numtract[i - 1] + 1
  }
}

# Create a unique numerical id for each precinct
vtdtract$uid <- paste(vtdtract$county, vtdtract$vtd, sep = "-")

# Reshape the dataframe 
tractvtd <-
  reshape(
    vtdtract,
    idvar = "uid",
    v.names = c("X2010.Tract", "X2010.census.pop", "alloc.factor"),
    timevar = "numtract",
    direction = "wide",
    sep = "-"
  )




