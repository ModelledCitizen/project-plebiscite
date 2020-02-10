setwd("~/Arcadio/")

######################## Import & Prepare Data ########################

# Import GeoCorr Crosswalk File
vtdblock <-
  read.csv("GeoCorr Data/vtdblock2.csv", stringsAsFactors = FALSE)

# Create a unique ID for each precinct
vtdblock$precid <-
  paste(vtdblock$county, vtdblock$vtd, sep = "-")

# Coerce the tract number into a consistent format
vtdblock$tractr <- NA
vtdblock$tractr <-
  ifelse(
    stringr::str_detect(as.character(vtdblock$tract), stringr::fixed(".")),
    gsub(".", "", vtdblock$tract, fixed = TRUE),
    paste(vtdblock$tract, "00", sep = "")
  )
vtdblock$tractr <- sprintf("%06s", vtdblock$tractr)

# Combine the given ID variables into a 12-digit FIPS code
vtdblock$fips <-
  paste(vtdblock$county,
        vtdblock$tractr,
        vtdblock$block,
        sep = "")
