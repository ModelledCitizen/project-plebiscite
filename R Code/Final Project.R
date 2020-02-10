##### Load Packages & Functions #####

library(data.table)
library(caTools)
library(neuralnet)
library(ggplot2)
library(caret)
library(stargazer)
library(InformationValue)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


##### Plot Variable Names #####

FVE.Labels <- read.delim("~/Arcadio/FVE Labels.csv")

stargazer(
  FVE.Labels["Field.Description"],
  type = "text",
  title = "Original Variables in the Full Voter Export",
  summary = FALSE,
  colnames = FALSE,
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/fvevariables.tex",
  header = FALSE,
  label = "fvevariables",
  font.size = "small",
  float = TRUE
)

rm(FVE.Labels)

##### Load and Subset (Voter File) #####
load("phl-fve-clean.Rdata")

phlfve.unscaled <-
  phlfve[, c("Age2016",
             "RegistrationAge",
             "RegistrationLength",
             "LastChangeLength",
             "VoteFraction")]

phlfve <-
  phlfve[, c(
    "ID Number",
    "geoid",
    "Vote2016",
    "Age2016S",
    "RegistrationAgeS",
    "RegistrationLengthS",
    "LastChangeLengthS",
    "VoteFractionS",
    "Female",
    "Male",
    "Democrat",
    "Republican",
    "Independent",
    "HasPhone",
    "RealPhone",
    "NonPennAreaCode",
    "NonPhilaAreaCode"
  )]


  
##### Descriptive Statistics (Voter File) #####
stargazer(
  phlfve.unscaled,
  type = "text",
  title = "Descriptive Statistics of Continuous Variables Before Standardization",
  covariate.labels = c(
    "Age on Election Day in 2016",
    "Age when Registered to Vote",
    "Days Registered by Election Day",
    "Days Between Last Update and E-Day",
    "Participation in General Elections"
  ),
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/fveunscaledstats.tex",
  header = FALSE,
  label = "fveunscaledstats",
  font.size = "small",
  align = FALSE
)

stargazer(
  phlfve,
  type = "text",
  title = "Descriptive Statistics of Variables from Voter File",
  covariate.labels = c(
    "Participation in the 2016 General Election",
    "Age on Election Day in 2016*",
    "Age when Registered to Vote*",
    "Days Registered by Election Day*",
    "Days Between Last Update and E-Day*",
    "Participation in General Elections*",
    "Registered as Female",
    "Registered as Male",
    "Registered as Democrat",
    "Registered as Republican",
    "Registered as Independent",
    "Phone Number in Registration",
    "Phone Number Validated",
    "Out of State Area Code",
    "Out of City Area Code"
  ),
  notes = "*Standardized to $\\mu = 0$ and $\\sigma = 1$",
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/fvestats.tex",
  header = FALSE,
  label = "fvestats",
  font.size = "small",
  align = FALSE
)



##### Load and Subset (Census Data) #####
load("phl-census-all.Rdata")

phlcensus <- phlcensus[, c(1, 57, 66, 2:8, 10:32, 34:56, 59, 61:62, 64:65, 67:71)]

##### Descriptive Statistics (Census Data) #####
stargazer(
  phlcensus,
  type = "text",
  title = "Descriptive Statistics for Census Data (All Blocks, Unweighted)",
  covariate.labels = c(
    "Average Household Size",
    "Average Family Size",
    "Race: White alone",
    "Race: Black or African American alone",
    "Race: American Indian / Alaska Native alone",
    "Race: Asian alone",
    "Race: Native Hawaiian / Pacific Islander alone",
    "Race: Some Other Race alone",
    "Race: Two or More Races",
    "Male: Under 5 years",
    "Male: 5 to 9 years",
    "Male: 10 to 14 years",
    "Male: 15 to 17 years",
    "Male: 18 and 19 years",
    "Male: 20 years",
    "Male: 21 years",
    "Male: 22 to 24",
    "Male: 25 to 29",
    "Male: 30 to 34",
    "Male: 35 to 39",
    "Male: 40 to 44",
    "Male: 45 to 49",
    "Male: 50 to 54",
    "Male: 55 to 59",
    "Male: 60 and 61 years",
    "Male: 62 to 64 years",
    "Male: 65 and 66 years",
    "Male: 67 to 69 years",
    "Male: 70 to 74 years",
    "Male: 75 to 79 years",
    "Male: 80 to 84 years",
    "Male: 85 years and over",
    "Female: Under 5 years",
    "Female: 5 to 9 years",
    "Female: 10 to 14 years",
    "Female: 15 to 17 years",
    "Female: 18 and 19 years",
    "Female: 20 years",
    "Female: 21 years",
    "Female: 22 to 24",
    "Female: 25 to 29",
    "Female: 30 to 34",
    "Female: 35 to 39",
    "Female: 40 to 44",
    "Female: 45 to 49",
    "Female: 50 to 54",
    "Female: 55 to 59",
    "Female: 60 and 61 years",
    "Female: 62 to 64 years",
    "Female: 65 and 66 years",
    "Female: 67 to 69 years",
    "Female: 70 to 74 years",
    "Female: 75 to 79 years",
    "Female: 80 to 84 years",
    "Female: 85 years and over",
    "Household Type: Husband-Wife Family",
    "Household Type: Single Male Family",
    "Household Type: Single Female Family",
    "Household Type: Householder Alone",
    "Household Type: Householder Not Alone",
    "Occupancy Status: Occupied",
    "Occupancy Status: Vacant",
    "Tenure: Owned with mortgage or loan",
    "Tenure: Owned free and clear",
    "Tenure: Renter-occupied"
  ),
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/censusstats.tex",
  header = FALSE,
  label = "censusstats",
  font.size = "small",
  float = FALSE,
  align = FALSE
)

##### Join Voter File & Census Data #####
phljoin <- merge(phlfve, phlcensus, by.x = "geoid", by.y = "GEOID", all.x = TRUE)

save(phljoin, file = "phl-join.Rdata")

rm(phlfve, phlfve.unscaled, phlcensus)

##### Split into Training and Test Data #####
set.seed(42)
phljoin[, split := sample.split(phljoin$Vote2016, SplitRatio = 2/3)]

phl.train <- phljoin[split == TRUE, c(1:82)]
phl.test <- phljoin[split == FALSE, c(1:82)]

save(phl.train, phl.test, file = "phl-train-test.Rdata")

rm(phljoin)


##### Create Fomulas for Models #####
fve.formula <-
  as.formula(paste0("Vote2016 ~ ", paste(names(phl.train)[4:17], collapse = " + ")))

census.formula <-
  as.formula(paste0("Vote2016 ~ ", paste(names(phl.train)[18:82], collapse = " + ")))

ensemble.formula <-
  as.formula(paste0("Vote2016 ~ ", paste(names(phl.train)[4:82], collapse = " + ")))


##### Train Logistic Regression Model #####

# FVE Data Only
fve.logistic <-
  glm(fve.formula,
      family = binomial(link = "probit"),
      data = phl.train[, c(3:17)])

save(fve.logistic, file = "fve.logistic.Rdata")

# Census Data Only
census.logistic <-
  glm(census.formula,
      family = binomial(link = "probit"),
      data = phl.train[, c(3, 18:82)])

save(census.logistic, file = "census.logistic.Rdata")

# Combined Data
ensemble.logistic <-
  glm(ensemble.formula,
      family = binomial(link = "probit"),
      data = phl.train[, c(3:82)])

save(ensemble.logistic, file = "ensemble.logistic.Rdata")


##### Train Artificial Neural Net #####

# FVE Data Only
fve.neural <-
  neuralnet(
    fve.formula,
    data = phl.train[, c(3:17)],
    hidden = c(7),
    rep = 1,
    act.fct = "logistic",
    linear.output = FALSE,
    lifesign = "full",
    lifesign.step = 100
  )

save(fve.neural, file = "fve.neural.Rdata")

# Census Data Only
census.neural <-
  neuralnet(
    census.formula,
    data = phl.train[, c(3, 18:82)],
    hidden = c(32),
    rep = 1,
    act.fct = "logistic",
    linear.output = FALSE,
    lifesign = "full",
    lifesign.step = 100
  )

save(census.neural, file = "census.neural.Rdata")

# Combined Data
ensemble.neural <-
  neuralnet(
    ensemble.formula,
    data = phl.train[, c(3:82)],
    hidden = c(39),
    rep = 1,
    err.fct = "sse",
    act.fct = "logistic",
    linear.output = FALSE,
    lifesign = "full",
    lifesign.step = 100
  )

save(ensemble.neural, file = "ensemble.neural.Rdata")


##### Describe Logistic Regression Model #####
stargazer(
  fve.logistic,
  census.logistic,
  ensemble.logistic,
  type = "text",
  title = "Logistic Regression Coefficients",
  dep.var.labels = "Participation in the 2016 General Election",
  dep.var.labels.include = TRUE,
  column.labels = c("FVE Only", "Census Only", "Combined"),
  covariate.labels = c(
    "Age on Election Day in 2016",
    "Age when Registered to Vote",
    "Days Registered by Election Day",
    "Days Between Last Update and E-Day",
    "Participation in General Elections",
    "Registered as Female",
    "Registered as Male",
    "Registered as Democrat",
    "Registered as Republican",
    "Registered as Independent",
    "Phone Number in Registration",
    "Phone Number Validated",
    "Out of State Area Code",
    "Out of City Area Code",
    "Average Household Size",
    "Average Family Size",
    "Race: White alone",
    "Race: Black or African American alone",
    "Race: American Indian / Alaska Native alone",
    "Race: Asian alone",
    "Race: Native Hawaiian / Pacific Islander alone",
    "Race: Some Other Race alone",
    "Race: Two or More Races",
    "Male: Under 5 years",
    "Male: 5 to 9 years",
    "Male: 10 to 14 years",
    "Male: 15 to 17 years",
    "Male: 18 and 19 years",
    "Male: 20 years",
    "Male: 21 years",
    "Male: 22 to 24",
    "Male: 25 to 29",
    "Male: 30 to 34",
    "Male: 35 to 39",
    "Male: 40 to 44",
    "Male: 45 to 49",
    "Male: 50 to 54",
    "Male: 55 to 59",
    "Male: 60 and 61 years",
    "Male: 62 to 64 years",
    "Male: 65 and 66 years",
    "Male: 67 to 69 years",
    "Male: 70 to 74 years",
    "Male: 75 to 79 years",
    "Male: 80 to 84 years",
    "Male: 85 years and over",
    "Female: Under 5 years",
    "Female: 5 to 9 years",
    "Female: 10 to 14 years",
    "Female: 15 to 17 years",
    "Female: 18 and 19 years",
    "Female: 20 years",
    "Female: 21 years",
    "Female: 22 to 24",
    "Female: 25 to 29",
    "Female: 30 to 34",
    "Female: 35 to 39",
    "Female: 40 to 44",
    "Female: 45 to 49",
    "Female: 50 to 54",
    "Female: 55 to 59",
    "Female: 60 and 61 years",
    "Female: 62 to 64 years",
    "Female: 65 and 66 years",
    "Female: 67 to 69 years",
    "Female: 70 to 74 years",
    "Female: 75 to 79 years",
    "Female: 80 to 84 years",
    "Female: 85 years and over",
    "Household Type: Husband-Wife Family",
    "Household Type: Single Male Family",
    "Household Type: Single Female Family",
    "Household Type: Householder Alone",
    "Household Type: Householder Not Alone",
    "Occupancy Status: Occupied",
    "Occupancy Status: Vacant",
    "Tenure: Owned with mortgage or loan",
    "Tenure: Owned free and clear",
    "Tenure: Renter-occupied"
  ),
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/logistic.tex",
  header = FALSE,
  label = "logistic",
  font.size = "small",
  float = FALSE,
  no.space = TRUE,
  align = FALSE
)

##### Evaluate Logistic Regression Model #####
fve.preds <-
  round(predict(fve.logistic, phl.test[, 4:17], type = "response"), 0) == 1
fve.real <- phl.test$Vote2016 == 1

fve.con <- table(as.factor(fve.preds), as.factor(fve.real))
fve.con <- caret::confusionMatrix(fve.con, positive = "TRUE")

fve.AUROC <- AUROC(as.numeric(fve.real), as.numeric(fve.preds))


census.preds <-
  round(predict(census.logistic, phl.test[, 18:82], type = "response"), 0) == 1
census.real <- phl.test$Vote2016 == 1

census.con <- table(as.factor(census.preds), as.factor(census.real))
census.con <- caret::confusionMatrix(census.con, positive = "TRUE")

census.AUROC <- AUROC(as.numeric(census.real), as.numeric(census.preds))


ensemble.preds <-
  round(predict(ensemble.logistic, phl.test[, 4:82], type = "response"), 0) == 1
ensemble.real <- phl.test$Vote2016 == 1

ensemble.con <-
  table(as.factor(ensemble.preds), as.factor(ensemble.real))
ensemble.con <-
  caret::confusionMatrix(ensemble.con, positive = "TRUE")

ensemble.AUROC <- AUROC(as.numeric(ensemble.real), as.numeric(ensemble.preds))


rm(fve.preds, fve.real, census.preds, census.real, ensemble.preds, ensemble.real)


eval.log <-
  cbind(
    rbind(fve.con$overall, census.con$overall, ensemble.con$overall),
    data.frame(
      "AUROC" = c(fve.AUROC, census.AUROC, ensemble.AUROC)
    ),
    rbind(fve.con$byClass, census.con$byClass, ensemble.con$byClass)
  )

rownames(eval.log) <- c("FVE Only", "Census Only", "Combined")

rm(fve.AUROC, census.AUROC, ensemble.AUROC)


stargazer(
  eval.log,
  type = "text",
  title = "Logistic Regression Evaluations",
  summary = FALSE,
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/logeval.tex",
  header = FALSE,
  label = "logeval",
  font.size = "small",
  flip = TRUE
)


##### Evaluate Artificial Neural Net #####
fve.preds <-
  round(predict(fve.logistic, phl.test[, 4:17], type = "response"), 0) == 1
fve.real <- phl.test$Vote2016 == 1

fve.con <- table(as.factor(fve.preds), as.factor(fve.real))
fve.con <- caret::confusionMatrix(fve.con, positive = "TRUE")

fve.AUROC <- AUROC(as.numeric(fve.real), as.numeric(fve.preds))


census.preds <-
  round(predict(census.logistic, phl.test[, 18:82], type = "response"), 0) == 1
census.real <- phl.test$Vote2016 == 1

census.con <- table(as.factor(census.preds), as.factor(census.real))
census.con <- caret::confusionMatrix(census.con, positive = "TRUE")

census.AUROC <- AUROC(as.numeric(census.real), as.numeric(census.preds))


ensemble.preds <-
  round(predict(ensemble.logistic, phl.test[, 4:82], type = "response"), 0) == 1
ensemble.real <- phl.test$Vote2016 == 1

ensemble.con <-
  table(as.factor(ensemble.preds), as.factor(ensemble.real))
ensemble.con <-
  caret::confusionMatrix(ensemble.con, positive = "TRUE")

ensemble.AUROC <- AUROC(as.numeric(ensemble.real), as.numeric(ensemble.preds))


rm(fve.preds, fve.real, census.preds, census.real, ensemble.preds, ensemble.real)


eval.neural <-
  cbind(
    rbind(fve.con$overall, census.con$overall, ensemble.con$overall),
    data.frame(
      "AUROC" = c(fve.AUROC, census.AUROC, ensemble.AUROC)
    ),
    rbind(fve.con$byClass, census.con$byClass, ensemble.con$byClass)
  )

rownames(eval.log) <- c("FVE Only", "Census Only", "Combined")

rm(fve.AUROC, census.AUROC, ensemble.AUROC)


stargazer(
  eval.log,
  type = "text",
  title = "Artificial Neural Net Evaluations",
  summary = FALSE,
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/neuraleval.tex",
  header = FALSE,
  label = "neuraleval",
  font.size = "small",
  flip = TRUE
)





rm(census.logistic, ensemble.logistic, FVE.Labels, fve.logistic, phl.test, phl.train, phlcensus, phlfve, phlfve.unscaled, census.f, ensemble.f, fve.f)
