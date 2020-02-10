setwd("~/Arcadio/")

# Import CSV
library(readr)
vtdblockgroup <-
  read_csv("GeoCorr Data/VTD to Block Group.csv", skip = 1)

# Create unique IDs for each precinct
vtdblockgroup$precid <-
  paste(vtdblockgroup$county, vtdblockgroup$vtd, sep = "-")

# Convert the given block group variables to the 12-digit FIPS code
vtdblockgroup$fips <-
  paste(
    vtdblockgroup$county,
    gsub(".", "", vtdblockgroup$`2010 Tract`, fixed = TRUE),
    vtdblockgroup$bg,
    sep = ""
  )

