setwd("~/Arcadio/")

######################## Import & Prepare Data ########################

# Import the ADDRFEAT file
phlshp <-
  rgdal::readOGR(dsn = "Census Data/TIGER2017/ADDRFEAT/tl_2017_42101_addrfeat/tl_2017_42101_addrfeat.shp", stringsAsFactors = FALSE)

# Import the FACES file
phlfaces <-
  rgdal::readOGR(dsn = "Census Data/TIGER2017/FACES/tl_2017_42101_faces/tl_2017_42101_faces.shp", stringsAsFactors = FALSE)

# Import the Philadelphia voter file
philadelphiafve <-
  read.delim(
    "PA FVE/Statewide/PHILADELPHIA FVE 20170710.txt",
    header = FALSE,
    stringsAsFactors = FALSE
  )

# Label the voter file
FVE.Labels <-
  read.delim("FVE Labels.csv", stringsAsFactors = FALSE)

names(philadelphiafve) <- FVE.Labels$Field.Description

rm(FVE.Labels)

# Create a summarized street address
philadelphiafve$streetrecode <-
  paste(philadelphiafve$`House Number`,
        philadelphiafve$`Street Name`,
        sep = " ")

# Set all GEOIDs to NA
#philadelphiafve$geoid <- NA
#philadelphiafve$method <- NA

######################## Alternatively, Load Existing File ########################

load("phl-fve.Rdata")

######################## Load Packages & Functions ########################

# Packages used throughought:
# install.packages(c("sp", "crayon", "stringdist", "RCurl", "RJSONIO", "ggmap"))

# Get GEOIDs from the Census Shapefiles
blockmatcher <- function(house, street, zip) {
  zips <-
    subset(phlshp, phlshp@data$ZIPL == zip |
             phlshp@data$ZIPR == zip)
  
  left <-
    subset(
      zips,
      as.integer(zips@data$LFROMHN) <= as.integer(house) &
        as.integer(zips@data$LTOHN) >= as.integer(house)
    )
  
  left <-
    subset(left,
           as.integer(left@data$LFROMHN) %% 2 == as.integer(house) %% 2)
  
  subleft <-
    subset(left, toupper(left@data$FULLNAME) == street)
  
  leftit <- 0
  
  if (length(subleft) == 0) {
    subleft <-
      subset(left, (stringdist::stringdist(
        street, toupper(left@data$FULLNAME), method = "lv"
      ) <= 1))
    leftit <- 1
  }
  
  if (length(subleft) == 0) {
    subleft <-
      subset(left, (stringdist::stringdist(
        street, toupper(left@data$FULLNAME), method = "lv"
      ) <= 2))
    leftit <- 2
  }
  
  if (length(subleft) == 0) {
    subleft <-
      subset(left, (stringdist::stringdist(
        street, toupper(left@data$FULLNAME), method = "lv"
      ) <= 3))
    leftit <- 3
  }
  
  if (length(subleft) == 0) {
    subleft <-
      subset(left, (stringdist::stringdist(
        street, toupper(left@data$FULLNAME), method = "lv"
      ) <= 4))
    leftit <- 4
  }
  
  right <-
    subset(
      zips,
      as.integer(zips@data$RFROMHN) <= as.integer(house) &
        as.integer(zips@data$RTOHN) >= as.integer(house)
    )
  
  right <-
    subset(right,
           as.integer(right@data$RFROMHN) %% 2 == as.integer(house) %% 2)
  
  subright <-
    subset(right, toupper(right@data$FULLNAME) == street)
  
  rightit <- 0
  
  if (length(subright) == 0) {
    subright <-
      subset(right, (stringdist::stringdist(
        street, toupper(right@data$FULLNAME), method = "lv"
      ) <= 1))
    rightit <- 1
  }
  
  if (length(subright) == 0) {
    subright <-
      subset(right, (stringdist::stringdist(
        street, toupper(right@data$FULLNAME), method = "lv"
      ) <= 2))
    rightit <- 2
  }
  
  if (length(subright) == 0) {
    subright <-
      subset(right, (stringdist::stringdist(
        street, toupper(right@data$FULLNAME), method = "lv"
      ) <= 3))
    rightit <- 3
  }
  
  if (length(subright) == 0) {
    subright <-
      subset(right, (stringdist::stringdist(
        street, toupper(right@data$FULLNAME), method = "lv"
      ) <= 4))
    rightit <- 4
  }
  
  if (length(subleft) == 1) {
    entity <- phlfaces[phlfaces@data$TFID ==  subleft@data$TFIDL,]
    fips <-
      paste(
        entity@data$STATEFP10,
        entity@data$COUNTYFP10,
        entity@data$TRACTCE10,
        entity@data$BLOCKCE10,
        sep = ""
      )
  } else if (length(subright) == 1) {
    entity <- phlfaces[phlfaces@data$TFID ==  subright@data$TFIDR,]
    fips <-
      paste(
        entity@data$STATEFP10,
        entity@data$COUNTYFP10,
        entity@data$TRACTCE10,
        entity@data$BLOCKCE10,
        sep = ""
      )
  } else {
    cat(crayon::red("Manual: Error in voter at", house, street, zip, "\n"))
    cat(
      paste(
        "Diagnostics: Left -",
        leftit,
        "-",
        length(subleft),
        "| Right -",
        rightit,
        "-",
        length(subright),
        "\n"
      )
    )
    fips <- NA
  }
  return(fips)
}

# Get GEOIDs from the Census API
censusdiag <- function(street, city, state, zip) {
  url <-
    "https://geocoding.geo.census.gov/geocoder/geographies/address?street=%s&city=%s&state=%s&zip=%s&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&format=json"
  url <- sprintf(url, gsub(" ", "+", street), city, state, zip)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  value <- json$result$addressMatches
  if (length(value) >= 1) {
    geoid <- value[[1]]$geographies$`Census Blocks`[[1]]$GEOID
  }
  if (length(value) == 0) {
    cat(crayon::red("API: Error in voter at", street, zip, "\n"))
    #print(paste("Diagnostics: ", json, sep = " "))
    geoid <- NA
  }
  return(geoid)
}

# Get GEOIDs from the FCC API
fccfips <- function(latitude, longitude) {
  url <-
    "https://geo.fcc.gov/api/census/block/find?latitude=%f&longitude=%f&showall=false&format=json"
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  if (is.list(json)) {
    geoid <- json$Block$FIPS
  } else {
    geoid <- NA
  }
  return(geoid)
}

######################## Perform Geocoding in 3 Stages ########################

for (i in 1:nrow(philadelphiafve)) {
  if (is.na(philadelphiafve$geoid[i])) {
    manual <- NA
    api <- NA
    fcc <- NA
    cat(crayon::blue$bold("Voter", philadelphiafve$`ID Number`[i], "\n"))
    cat("Trying manual...\n")
    manual <- blockmatcher(
      philadelphiafve$`House Number`[i],
      philadelphiafve$`Street Name`[i],
      philadelphiafve$Zip[i]
    )
    philadelphiafve$geoid[i] <- manual
    philadelphiafve$method[i] <- "manual"
    if (is.na(manual)) {
      cat("Trying Census API...\n")
      api <- censusdiag(
        philadelphiafve$streetrecode[i],
        philadelphiafve$City[i],
        philadelphiafve$State[i],
        philadelphiafve$Zip[i]
      )
      philadelphiafve$geoid[i] <- api
      philadelphiafve$method[i] <- "api"
    }
    if (is.na(api) & is.na(manual)) {
      cat("Trying Google/DSK + FCC...\n")
      location <- paste(
        philadelphiafve$streetrecode[i],
        philadelphiafve$City[i],
        philadelphiafve$State[i],
        philadelphiafve$Zip[i],
        sep = " "
      )
      if (ggmap::geocodeQueryCheck() > 0) {
        latlon <-
          ggmap::geocode(location, output = "latlon", source = "google")
        philadelphiafve$method[i] <- "google/fcc"
      } else {
        latlon <-
          ggmap::geocode(location, output = "latlon", source = "dsk")
        philadelphiafve$method[i] <- "dsk/fcc"
      }
      if (!is.na(latlon[1, 1])) {
        fcc <- fccfips(latlon[1, 2], latlon[1, 1])
        philadelphiafve$geoid[i] <- fcc
      }
    }
    if (!is.na(philadelphiafve$geoid[i])) {
      cat(
        crayon::green(
          "Success!",
          sum(is.na(philadelphiafve$geoid)),
          "remaining.\n",
          100 * round(sum(!is.na(
            philadelphiafve$geoid
          )) / length(philadelphiafve$geoid), digits = 2),
          "percent complete.\n"
        )
      )
    } else {
      cat(crayon::red(
        "Failed to geocode voter",
        philadelphiafve$`ID Number`[i],
        "\n"
      ))
    }
  }
}

######################## Clean Up, Check and Save Progress ########################

rm(manual, api, fcc, location, latlon, i)

philadelphiafve$method[is.na(philadelphiafve$geoid)] <- NA
summary(as.factor(philadelphiafve$method))

save(philadelphiafve, phlshp, phlfaces, file = "phl-fve.Rdata")

######################## Correct Error in Voter File & Code ########################

philadelphiafve$Zip[philadelphiafve$`ID Number` == "016078586-51"] <- "19103"

philadelphiafve$geoid[philadelphiafve$`ID Number` == "016078586-51"] <-
  blockmatcher(
    philadelphiafve$`House Number`[philadelphiafve$`ID Number` == "016078586-51"],
    philadelphiafve$`Street Name`[philadelphiafve$`ID Number` == "016078586-51"],
    philadelphiafve$Zip[philadelphiafve$`ID Number` == "016078586-51"]
  )


######################## Update Entries Coded by DSK ########################
failed <- c()
for (i in 1:nrow(philadelphiafve)) {
  if (philadelphiafve$method[i] == "dsk/fcc") {
    cat(crayon::blue$bold("Voter", philadelphiafve$`ID Number`[i], "\n"))
    cat("Trying manual...\n")
    address <-
      c(
        philadelphiafve$`House Number`[i],
        philadelphiafve$`Street Name`[i],
        philadelphiafve$streetrecode[i],
        philadelphiafve$City[i],
        philadelphiafve$State[i],
        philadelphiafve$Zip[i]
      )
    api <- NA
    fcc <- NA
    manual <- blockmatcher(address[1], address[2], address[6])
    if (is.na(manual)) {
      cat("Trying Census API...\n")
      api <-
        censusdiag(address[3], address[4], address[5], address[6])
      if (is.na(api)) {
        latlon <-
          ggmap::geocode(
            paste(address[3], address[4], address[5], address[6]),
            output = "latlon",
            source = "google"
          )
        fcc <- fccfips(latlon[1, 2], latlon[1, 1])
        if (is.na(fcc)) {
          failed <- c(failed, philadelphiafve$`ID Number`[i])
          cat(crayon::red(
            "Failed to geocode voter",
            philadelphiafve$`ID Number`[i],
            "\n"
          ))
        } else {
          philadelphiafve$method[i] <- "google/fcc"
          philadelphiafve$geoid[i] <- fcc
          cat(crayon::green("Success!\n"))
        }
      } else {
        philadelphiafve$method[i] <- "api"
        philadelphiafve$geoid[i] <- api
        cat(crayon::green("Success!\n"))
      }
    } else {
      philadelphiafve$method[i] <- "manual"
      philadelphiafve$geoid[i] <- manual
      cat(crayon::green("Success!\n"))
    }
  }
}

######################## Convert to data.table for Stage II ########################

phlfve <- as.data.table(philadelphiafve)
