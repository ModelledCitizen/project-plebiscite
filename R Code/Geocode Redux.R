######################## Import & Prepare Data ########################

# Import the ADDRFEAT file
phlshp <-
  rgdal::readOGR(dsn = "Census Data/TIGER2017/ADDRFEAT/tl_2017_42101_addrfeat/tl_2017_42101_addrfeat.shp", stringsAsFactors = FALSE)

phlshp <- data.table::as.data.table(phlshp@data)

# Import the FACES file
phlfaces <-
  rgdal::readOGR(dsn = "Census Data/TIGER2017/FACES/tl_2017_42101_faces/tl_2017_42101_faces.shp", stringsAsFactors = FALSE)

phlfaces <- data.table::as.data.table(phlfaces@data)

# Import the Philadelphia voter file
philadelphiafve <- data.table::fread("PA FVE/Statewide/PHILADELPHIA FVE 20170710.txt")

FVE.Labels <- read.delim("FVE Labels.csv", stringsAsFactors = FALSE)

names(philadelphiafve) <- FVE.Labels$Field.Description

rm(FVE.Labels)

######################## Load Functions ########################

# Get GEOIDs from the Census Shapefiles
blockmatcher <- function(house, street, zip) {
  
  zips <- phlshp[ZIPL == zip | ZIPR == zip]
  
  left <- zips[as.integer(LFROMHN) <= as.integer(house) & as.integer(LTOHN) >= as.integer(house)]
  left <- left[as.integer(LFROMHN) %% 2 == as.integer(house) %% 2]
  subleft <- left[toupper(FULLNAME) == street]
   
  right <- zips[as.integer(RFROMHN) <= as.integer(house) & as.integer(RTOHN) >= as.integer(house)]
  right <- right[as.integer(RFROMHN) %% 2 == as.integer(house) %% 2]
  subright <- right[toupper(FULLNAME) == street] 
  
  if (length(subleft) == 0 & length(subright) == 0) {
    subleft <- 
      left[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 1]
    if (length(subleft) == 0) {
      subright <- 
        right[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 1]
      if (length(subright) == 0) {
        subleft <- 
          left[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 2]
        if (length(subleft) == 0) {
          subright <- 
            right[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 2]
          if (length(subright) == 0) {
            subleft <- 
              left[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 3]
            if (length(subleft) == 0) {
              subright <- 
                right[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 3]
              if (length(subright) == 0) {
                subleft <- 
                  left[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 4]
                if (length(subleft) == 0) {
                  subright <- 
                    right[stringdist::stringdist(street, toupper(FULLNAME), method = "lv") <= 4]
                }
              }
            }
          }
        }
      }
    }
  }

  if (length(subleft) == 1) {
    feature <- subleft$TFIDL
    entity <- phlfaces[TFID == feature]
    fips <-
      paste(
        entity$STATEFP10,
        entity$COUNTYFP10,
        entity$TRACTCE10,
        entity$BLOCKCE10,
        sep = ""
      )
  } else if (length(subright) == 1) {
    feature <- subleft$TFIDR
    entity <- phlfaces[TFID == feature]
    fips <-
      paste(
        entity$STATEFP10,
        entity$COUNTYFP10,
        entity$TRACTCE10,
        entity$BLOCKCE10,
        sep = ""
      )
  } else {
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

######################## Main Routine ########################

for (i in 1:nrow(philadelphiafve)) {
  if (philadelphiafve$county[i] == "091") {
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

rm(manual, api, fcc, location, latlon, i)

######################## Testing ########################
philadelphiafve$geoid <- NA

philadelphiafve2 <- philadelphiafve[sample(.N, 100)]

for (i in 1:nrow(philadelphiafve2)) {
  philadelphiafve2$geoid[i] <-
    blockmatcher(
      philadelphiafve2$`House Number`[i],
      philadelphiafve2$`Street Name`[i],
      philadelphiafve2$Zip[i]
    )
}

philadelphiafve2$geoid[2] <-
  blockmatcher(
    philadelphiafve2$`House Number`[2],
    philadelphiafve2$`Street Name`[2],
    philadelphiafve2$Zip[2]
  )



philadelphiafve2$geoid <-
  mapply(function(x, y, z) blockmatcher(x, y, z),
    x = philadelphiafve2$`House Number`,
    y = philadelphiafve2$`Street Name`,
    z = philadelphiafve2$Zip
  )
 
 ifelse(
    is.na(philadelphiafve2$geoid),
    blockmatcher(
      philadelphiafve2$`House Number`,
      philadelphiafve2$`Street Name`,
      philadelphiafve2$Zip
    ),
    philadelphiafve2$geoid
  )


