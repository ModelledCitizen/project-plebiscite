library(tidyverse)
library(tidycensus)
library(data.table)

census_api_key("****************************************")
sf1var <- load_variables(year = 2010, dataset = "sf1", cache = TRUE)

##### Race #####

# White alone
# Black or African American alone
# American Indian and Alaska Native alone
# Asian alone
# Native Hawaiian and Other Pacific Islander alone 
# Some Other Race alone
# Two or More Races

p3 <- get_decennial(
  geography = "block",
  year = 2010,
  state = 42,
  county = 101,
  geometry = FALSE,
  table = "P003",
  summary_var = "P0030001"
)

p3 <- filter(p3, variable != "P0030001")

p3$value.prop <- p3$value / p3$summary_value

p3$value.prop[p3$summary_value == 0] <- 0

p3 <- p3[c(1, 3, 6)]

p3 <- spread(p3, key = variable, value = value.prop)

p3 <- ungroup(p3)

##### Sex by Age #####

p12 <- get_decennial(
  geography = "block",
  year = 2010,
  state = 42,
  county = 101,
  geometry = FALSE,
  table = "P012",
  summary_var = "P0120001"
)

p12 <- filter(p12, variable != "P0120001")

p12$value.prop <- p12$value / p12$summary_value

p12$value.prop[p12$summary_value == 0] <- 0

p12 <- p12[c(1, 3, 6)]

p12 <- spread(p12, key = variable, value = value.prop)

p12 <- ungroup(p12)

##### Average Household Size by Age #####

p17 <- get_decennial(
  geography = "block",
  year = 2010,
  state = 42,
  county = 101,
  geometry = FALSE,
  table = "P017",
  summary_var = "P0170001"
)

p17 <- filter(p17, variable == "P0170001")

p17 <- spread(p17, key = variable, value = value)

p17 <- p17[c(1, 4)]

p17 <- ungroup(p17)

##### Household Type #####

# Family households:
#   Husband-wife family 
#   Other family:
#     Male householder, no wife present
#     Female householder, no husband present
# Nonfamily households:
#   Householder living alone 
#   Householder not living alone

p18 <- get_decennial(
  geography = "block",
  year = 2010,
  state = 42,
  county = 101,
  geometry = FALSE,
  table = "P018",
  summary_var = "P0180001"
)

p18 <- filter(p18, variable != "P0180001")

p18$value.prop <- p18$value / p18$summary_value

p18$value.prop[p18$summary_value == 0] <- 0

p18 <- p18[c(1, 3, 6)]

p18 <- spread(p18, key = variable, value = value.prop)

p18 <- ungroup(p18)

##### Average Family Size by Age #####

p37 <- get_decennial(
  geography = "block",
  year = 2010,
  state = 42,
  county = 101,
  geometry = FALSE,
  table = "P037",
  summary_var = "P0370001"
)

p37 <- filter(p37, variable == "P0370001")

p37 <- spread(p37, key = variable, value = value)

p37 <- p37[c(1, 4)]

p37 <- ungroup(p37)

##### Occupancy Status #####

# Occupied
# Vacant

h3 <- get_decennial(
  geography = "block",
  year = 2010,
  state = 42,
  county = 101,
  geometry = FALSE,
  table = "H003",
  summary_var = "H0030001"
)

h3 <- filter(h3, variable != "H0030001")

h3$value.prop <- h3$value / h3$summary_value

h3$value.prop[h3$summary_value == 0] <- 0

h3 <- h3[c(1, 3, 6)]

h3 <- spread(h3, key = variable, value = value.prop)

h3 <- ungroup(h3)

##### Tenure #####

# Owned with a mortgage or a loan 
# Owned free and clear 
# Renter-occupied

h4 <- get_decennial(
  geography = "block",
  year = 2010,
  state = 42,
  county = 101,
  geometry = FALSE,
  table = "H004",
  summary_var = "H0040001"
)

h4 <- filter(h4, variable != "H0040001")

h4$value.prop <- h4$value / h4$summary_value

h4$value.prop[h4$summary_value == 0] <- 0

h4 <- h4[c(1, 3, 6)]

h4 <- spread(h4, key = variable, value = value.prop)

h4 <- ungroup(h4)

##### Join Data #####

joined <-
  p3 %>% full_join(p12, by = "GEOID") %>% full_join(p17, by = "GEOID") %>% full_join(p18, by = "GEOID") %>% full_join(p37, by = "GEOID") %>% full_join(h3, by = "GEOID") %>% full_join(h4, by = "GEOID")

rm(p3, p12, p17, p18, p37, h3, h4)

##### Convert and Export #####

phlcensus <- as.data.table(joined)

save(phlcensus, file = "phl-census-all.Rdata")

rm(joined, sf1var)


