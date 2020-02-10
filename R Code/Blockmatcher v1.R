blockmatcher <- function(house, street, zip) {
  zips <-
    subset(phlshp, phlshp@data$ZIPL == zip |
             phlshp@data$ZIPR == zip)
  
  streets <-
    subset(zips, (stringdist::stringdist(
      street, toupper(zips@data$FULLNAME), method = "lv"
    ) <= 1))
  
  left <-
    subset(
      streets,
      as.integer(streets@data$LFROMHN) <= as.integer(house) &
        as.integer(streets@data$LTOHN) >= as.integer(house)
    )
  left <-
    subset(left,
           as.integer(left@data$LFROMHN) %% 2 == as.integer(house) %% 2)
  
  right <-
    subset(
      streets,
      as.integer(streets@data$RFROMHN) <= as.integer(house) &
        as.integer(streets@data$RTOHN) >= as.integer(house)
    )
  right <-
    subset(right,
           as.integer(right@data$RFROMHN) %% 2 == as.integer(house) %% 2)
  
  if (length(right) > 1) {
    right <- subset(right, toupper(right@data$FULLNAME) == street)
  } 
  
  if (length(left) > 1) {
    left <- subset(left, toupper(left@data$FULLNAME) == street)
  }
  
  if (length(left) == 1) {
    houses <- left
    entity <- phlfaces[phlfaces@data$TFID ==  houses@data$TFIDL, ]
    fips <-
      paste(
        entity@data$STATEFP10,
        entity@data$COUNTYFP10,
        entity@data$TRACTCE10,
        entity@data$BLOCKCE10,
        sep = ""
      )
  } else if (length(right) == 1) {
    houses <- right
    entity <- phlfaces[phlfaces@data$TFID ==  houses@data$TFIDR, ]
    fips <-
      paste(
        entity@data$STATEFP10,
        entity@data$COUNTYFP10,
        entity@data$TRACTCE10,
        entity@data$BLOCKCE10,
        sep = ""
      )
  } else {
    print(paste("Manual: Error in voter at", house, street, zip, sep = " "))
    print(paste(
      "Diagnostics: Left -",
      length(left),
      "| Right -",
      length(right),
      sep = " "
    ))
    fips <- NA
  }
  return(fips)
}
