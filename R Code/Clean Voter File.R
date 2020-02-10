library(data.table)

##### Clean the Geocoded Voter File #####

#load("phl-fve.Rdata")
#phlfve <- as.data.table(philadelphiafve)
#rm(philadelphiafve, phlfaces, phlshp)
#save(phlfve, file = "phl-fve-all.Rdata")

load("phl-fve-all.Rdata")

# Updating Incorrectly Geocoded Entries
phlfve[`House Number` == "3900" & `Street Name` == "CITY AVE", geoid := "421010122031002"]
phlfve[`House Number` == "3901" & `Street Name` == "CITY AVE", geoid := "421010122031000"]
phlfve[`House Number` == "3903" & `Street Name` == "CITY AVE", geoid := "421010122031000"]
phlfve[`House Number` == "210" & `Street Name` == "W CHELTENHAM AVE", geoid := "421010271001004"]
phlfve[`House Number` == "382" & `Street Name` == "W CHELTENHAM AVE", geoid := "421010271001003"]
phlfve[`House Number` == "9217" & `Street Name` == "PINE RD", geoid := "421010344004021"]
phlfve[`House Number` == "9219" & `Street Name` == "PINE RD", geoid := "421010344004018"]
phlfve[`House Number` == "842" & `Street Name` == "NORTHWESTERN AVE", geoid := "421010384001000"]
phlfve[`House Number` == "844" & `Street Name` == "NORTHWESTERN AVE", geoid := "421010384001000"]
phlfve[`House Number` == "848" & `Street Name` == "NORTHWESTERN AVE", geoid := "421010384001000"]

# Restrict to necessary variables
columns <-
  c(1:12, 151, 26, 29, 155, grep("Vote Method", colnames(phlfve))[c(15, 18, 22, 27, 32, 39)])
phlfve <- phlfve[, columns, with = FALSE]

rm(columns)

# Convert dates from characters to date objects
phlfve[, DOB := as.Date(DOB, "%m/%d/%Y")]
phlfve[, `Registration Date` := as.Date(`Registration Date`, "%m/%d/%Y")]
phlfve[, `Status Change Date` := as.Date(`Status Change Date`, "%m/%d/%Y")]
phlfve[, `Last Vote Date` := as.Date(`Last Vote Date`, "%m/%d/%Y")]
phlfve[, `Date Last Changed` := as.Date(`Date Last Changed`, "%m/%d/%Y")]

# WARNING: Dropping 52403 Voters labeled "Inactive"
phlfve <- phlfve[`Voter Status` == "A"]

# WARNING: Dropping One Voter with no DOB
phlfve <- phlfve[!is.na(DOB)]

# WARNING: Dropping 372 Voters with Incorrect Registration Dates
phlfve <- phlfve[`Registration Date` > DOB]

# WARNING: Dropping 20160 Voters Registered After E-Day 2016
phlfve <- phlfve[`Registration Date` <= as.Date("2016-11-08")]

# WARNING: Dropping 70285 Voters Who Changed Their Registration After E-Day
phlfve <- phlfve[`Date Last Changed` <= as.Date("2016-11-08")]

# WARNING: Dropping One Voter Who Was Underage
phlfve <- phlfve[`DOB` <= as.Date("1998-11-08")]

# WARNING: Dropping 9 strange cases (these people don't live in Philadelphia...)
phlfve[, county := substr(geoid, 3, 5)]
phlfve <- phlfve[county == "101",]
phlfve[, county := NULL]

##### Recode Variables #####
age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1,
    age
  )
}

phlfve[, Age2016 := age(from = DOB, to = as.Date("2016-11-08"))]

phlfve[, RegistrationAge := age(from = DOB, to = `Registration Date`)]

phlfve[, RegistrationLength := difftime(as.Date("2016-11-08"), `Registration Date`, units = "days")]
phlfve[, RegistrationLength := as.numeric(RegistrationLength)]

phlfve[, LastChangeLength := difftime(as.Date("2016-11-08"), `Date Last Changed`, units = "days")]
phlfve[, LastChangeLength := as.numeric(LastChangeLength)]

phlfve[, Age2016S := scale(Age2016)]
phlfve[, RegistrationAgeS := scale(RegistrationAge)]
phlfve[, RegistrationLengthS := scale(RegistrationLength)]
phlfve[, LastChangeLengthS := scale(LastChangeLength)]


phlfve[, Democrat := as.numeric(`Party Code` == "D")]
phlfve[, Republican := as.numeric(`Party Code` == "R")]
phlfve[, Independent := as.numeric(`Party Code` == "I")]

phlfve[, Female := as.numeric(`Gender` == "F")]
phlfve[, Male := as.numeric(`Gender` == "M")]


phlfve[, AreaCode := ifelse(substr(phlfve$`Home Phone`, 1, 1) == "1",
                            as.numeric(substr(phlfve$`Home Phone`, 2, 4)),
                            as.numeric(substr(phlfve$`Home Phone`, 1, 3)))]

phlfve[, HasPhone := as.numeric(!is.na(AreaCode))]

AreaCodes <- read.csv("AreaCodes.csv")

phlfve[, RealPhone := as.numeric(AreaCode %in% AreaCodes$Area.Code)]

phlfve[, NonPhilaAreaCode := as.numeric(!AreaCode %in% c(215, 267))]

pacodes <-
  c(215, 223, 267, 272, 412, 484, 570, 610, 717, 724, 814, 878)

phlfve[, NonPennAreaCode := as.numeric(!AreaCode %in% pacodes)]

rm(AreaCodes, pacodes)

votemethods <- grep("Vote Method", colnames(phlfve))

for (i in votemethods) {
  phlfve[, i] <-
    ifelse(phlfve[, i, with = FALSE] == "AP" |
             phlfve[, i, with = FALSE] == "AB" |
             phlfve[, i, with = FALSE] == "P", 1, 0)
}

colnames(phlfve)[votemethods] <-
  paste0("Vote", c(2006, 2008, 2010, 2012, 2014, 2016))

rm(age, votemethods, i)

phlfve[`Registration Date` <= as.Date("11/07/2006", "%m/%d/%Y"), VoteFraction := rowSums(.SD) / 5, .SDcols = paste0("Vote", c(2006, 2008, 2010, 2012, 2014))]

phlfve[`Registration Date` > as.Date("11/07/2006", "%m/%d/%Y"), VoteFraction := rowSums(.SD) / 4, .SDcols = paste0("Vote", c(2008, 2010, 2012, 2014))]

phlfve[`Registration Date` > as.Date("11/04/2008", "%m/%d/%Y"), VoteFraction := rowSums(.SD) / 3, .SDcols = paste0("Vote", c(2010, 2012, 2014))]

phlfve[`Registration Date` > as.Date("11/02/2010", "%m/%d/%Y"), VoteFraction := rowSums(.SD) / 2, .SDcols = paste0("Vote", c(2012, 2014))]

phlfve[`Registration Date` > as.Date("11/06/2012", "%m/%d/%Y"), VoteFraction := rowSums(.SD) / 1, .SDcols = paste0("Vote", c(2014))]

phlfve[, VoteFractionS := scale(VoteFraction)]

save(phlfve, file = "phl-fve-clean.Rdata")
