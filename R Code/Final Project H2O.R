library(h2o)
library(texreg)
library(stargazer)
library(ggplot2)

h2o.init(ip = "18.188.39.149", port = 54321)

phl.train.h2o <- as.h2o(phl.train)
phl.test.h2o <- as.h2o(phl.test)

phl.train.h2o$Vote2016 <- as.factor(phl.train.h2o$Vote2016)
phl.test.h2o$Vote2016 <- as.factor(phl.test.h2o$Vote2016)


save(phl.train.h2o, phl.test.h2o, file = "phl-train-test.h2o.Rdata")


##### Train Generalized Lineear Models #####

fve.logistic <-
  h2o.glm(
    x = 4:17,
    y = "Vote2016",
    training_frame = phl.train.h2o,
    validation_frame = phl.test.h2o,
    family = "binomial",
    link = "logit"
  )


census.logistic <-
  h2o.glm(
    x = 18:82,
    y = "Vote2016",
    training_frame = phl.train.h2o,
    validation_frame = phl.test.h2o,
    family = "binomial",
    link = "logit"
  )


ensemble.logistic <-
  h2o.glm(
    x = 4:82,
    y = "Vote2016",
    training_frame = phl.train.h2o,
    validation_frame = phl.test.h2o,
    family = "binomial",
    link = "logit"
  )


##### Train Deep Learning Algorithms #####

fve.neural <-
  h2o.deeplearning(
    x = 4:17,
    y = "Vote2016",
    training_frame = phl.train.h2o,
    validation_frame = phl.test.h2o,
    distribution = "bernoulli"
  )


census.neural <-
  h2o.deeplearning(
    x = 18:82,
    y = "Vote2016",
    training_frame = phl.train.h2o,
    validation_frame = phl.test.h2o,
    distribution = "bernoulli"
  )


ensemble.neural <-
  h2o.deeplearning(
    x = 4:82,
    y = "Vote2016",
    training_frame = phl.train.h2o,
    validation_frame = phl.test.h2o,
    distribution = "bernoulli"
  )


save(
  fve.logistic,
  census.logistic,
  ensemble.logistic,
  fve.neural,
  census.neural,
  ensemble.neural,
  file = "h2o.models.Rdata"
)


##### Evaluate Performance of Models #####

fve.log.perf <-
  h2o.performance(fve.logistic, newdata = phl.test.h2o)

cen.log.perf <-
  h2o.performance(census.logistic, newdata = phl.test.h2o)

ens.log.perf <-
  h2o.performance(ensemble.logistic, newdata = phl.test.h2o)


fve.net.perf <- h2o.performance(fve.neural, newdata = phl.test.h2o)

cen.net.perf <-
  h2o.performance(census.neural, newdata = phl.test.h2o)

ens.net.perf <-
  h2o.performance(ensemble.neural, newdata = phl.test.h2o)


save(
  fve.log.perf,
  cen.log.perf,
  ens.log.perf,
  fve.net.perf,
  cen.net.perf,
  ens.net.perf,
  file = "h2o.perf.Rdata"
)


texreg(
  l = list(fve.logistic, census.logistic, ensemble.logistic),
  file = "~/Desktop/PSCI 338/Final Project/Report/glm.tex",
  label = "glm",
  caption = "Coefficients for Generalized Linear Models",
  fontsize = "small",
  longtable = TRUE,
  custom.model.names = c("FVE Only",
                         "Census Only",
                         "Combined"),
  custom.coef.names = c(
    "Constant",
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
  standardized = FALSE,
  include.mse = FALSE,
  include.rsquared = FALSE,
  include.logloss = FALSE,
  include.meanerror = FALSE,
  include.auc = FALSE,
  include.gini = FALSE,
  include.deviance = FALSE,
  include.aic = FALSE,
  single.row = TRUE,
  custom.note = "",
  use.packages = FALSE,
  caption.above = TRUE,
  dcolumn = TRUE,
  digits = 3
)

eval.1 <- data.frame(
  "MSE" = c(
    fve.log.perf@metrics$MSE,
    cen.log.perf@metrics$MSE,
    ens.log.perf@metrics$MSE,
    fve.net.perf@metrics$MSE,
    cen.net.perf@metrics$MSE,
    ens.net.perf@metrics$MSE
  ),
  "RMSE" = c(
    fve.log.perf@metrics$RMSE,
    cen.log.perf@metrics$RMSE,
    ens.log.perf@metrics$RMSE,
    fve.net.perf@metrics$RMSE,
    cen.net.perf@metrics$RMSE,
    ens.net.perf@metrics$RMSE
  ),
  "LogLoss" = c(
    fve.log.perf@metrics$logloss,
    cen.log.perf@metrics$logloss,
    ens.log.perf@metrics$logloss,
    fve.net.perf@metrics$logloss,
    cen.net.perf@metrics$logloss,
    ens.net.perf@metrics$logloss
  ),
  "MeanPCE" = c(
    fve.log.perf@metrics$mean_per_class_error,
    cen.log.perf@metrics$mean_per_class_error,
    ens.log.perf@metrics$mean_per_class_error,
    fve.net.perf@metrics$mean_per_class_error,
    cen.net.perf@metrics$mean_per_class_error,
    ens.net.perf@metrics$mean_per_class_error
  ),
  "AUC" = c(
    fve.log.perf@metrics$AUC,
    cen.log.perf@metrics$AUC,
    ens.log.perf@metrics$AUC,
    fve.net.perf@metrics$AUC,
    cen.net.perf@metrics$AUC,
    ens.net.perf@metrics$AUC
  ),
  "Gini" = c(
    fve.log.perf@metrics$Gini,
    cen.log.perf@metrics$Gini,
    ens.log.perf@metrics$Gini,
    fve.net.perf@metrics$Gini,
    cen.net.perf@metrics$Gini,
    ens.net.perf@metrics$Gini
  )
)

eval.2 <- rbind(
  fve.log.perf@metrics$max_criteria_and_metric_scores$value, 
  cen.log.perf@metrics$max_criteria_and_metric_scores$value,
  ens.log.perf@metrics$max_criteria_and_metric_scores$value,
  fve.net.perf@metrics$max_criteria_and_metric_scores$value,
  cen.net.perf@metrics$max_criteria_and_metric_scores$value,
  ens.net.perf@metrics$max_criteria_and_metric_scores$value
)

colnames(eval.2) <- fve.log.perf@metrics$max_criteria_and_metric_scores$metric

mod.eval <- cbind(eval.1, eval.2)

rm(eval.1, eval.2)


names(mod.eval) <- c(
  "Mean Squared Error",
  "Root Mean Squared Error",
  "Logarithmic Loss",
  "Mean Per Class Error",
  "Area Under ROC Curve",
  "Gini Coefficient",
  "F1",
  "F2",
  "F0.5",
  "Accuracy",
  "Precision",
  "Recall",
  "Specificity",
  "Matthews Correlation Coefficient",
  "Minimum Per Class Accuracy",
  "Mean Per Class Accuracy"
)

stargazer(
  mod.eval,
  type = "text",
  title = "Model Evaluation Metrics",
  summary = FALSE,
  covariate.labels = c(
    "",
    "FVE Only",
    "Census Only",
    "Combined",
    "FVE Only",
    "Census Only",
    "Combined"
  ),
  digits = 3,
  out = "~/Desktop/PSCI 338/Final Project/Report/modeval.tex",
  header = FALSE,
  label = "modeval",
  font.size = "small",
  flip = TRUE
)

##### Plot Results to Compare #####

mod.plot <-
  cbind(
    mod.eval,
    data.frame(
      "ModelID" = 1:6,
      "ModelName" = c(paste(
        "GLM:", c("FVE Only", "Census Only", "Combined")
      ), paste(
        "ANN:", c("FVE Only", "Census Only", "Combined")
      )),
      "Subset" = factor(rep(c(
        "FVE Only", "Census Only", "Combined"
      ), 2), levels = c("FVE Only", "Census Only", "Combined")),
      "Technique" = as.factor(c(
        rep("Generalized Linear Model (GLM)", 3),
        rep("Deep Learning Model (ANN)", 3)
      )),
      stringsAsFactors = FALSE
    )
  )

mod.plot <- mod.plot[order(mod.plot$Subset, mod.plot$Technique), ]
row.names(mod.plot) <- 1:6
mod.plot$ModelName <- factor(mod.plot$ModelName, levels = mod.plot$ModelName)

plot(mod.eval$`Matthews Correlation Coefficient`)

ggplot(data = mod.plot,
       aes(x = Subset, y = `Matthews Correlation Coefficient`, fill = ModelName)) + 
  geom_col(position = "dodge") + 
  theme_minimal() + 
  theme(legend.position = "bottom", panel.grid.major.x = element_blank()) + 
  scale_fill_brewer(palette = "Paired")

ggplot(data = mod.plot,
       aes(x = Technique, y = `Matthews Correlation Coefficient`, fill = Subset)) + 
  geom_col(position = "dodge") + 
  theme_minimal() + 
  theme(legend.position = "bottom", panel.grid.major.x = element_blank()) + 
  scale_fill_grey()


pdf(
  file = "~/Desktop/PSCI 338/Final Project/Report/Graphic%03d.pdf",
  width = 5.25,
  height = 5.25,
  onefile = FALSE,
  paper = "special",
  pointsize = 12
)

ggplot(data = mod.plot,
       aes(x = Subset, y = `Matthews Correlation Coefficient`, fill = Technique)) + 
  geom_col(position = "dodge") + 
  theme_minimal() + 
  theme(legend.position = "bottom", panel.grid.major.x = element_blank()) + 
  scale_fill_grey()

ggplot(data = mod.plot,
       aes(x = Subset, y = `Area Under ROC Curve`, fill = Technique)) + 
  geom_col(position = "dodge") + 
  theme_minimal() + 
  theme(legend.position = "bottom", panel.grid.major.x = element_blank()) + 
  scale_fill_grey()

ggplot(data = mod.plot,
       aes(x = Subset, y = `Accuracy`, fill = Technique)) + 
  geom_col(position = "dodge") + 
  theme_minimal() + 
  theme(legend.position = "bottom", panel.grid.major.x = element_blank()) + 
  scale_fill_grey()

dev.off()

##### Cite Packages Used #####
sink("~/Desktop/R.bib")
toBibtex(citation("base"))
toBibtex(citation("caret"))
toBibtex(citation("caTools"))
toBibtex(citation("data.table"))
toBibtex(citation("h2o"))
toBibtex(citation("InformationValue"))
toBibtex(citation("neuralnet"))
toBibtex(citation("RCurl"))
toBibtex(citation("rgdal"))
toBibtex(citation("RJSONIO"))
toBibtex(citation("sp"))
toBibtex(citation("stargazer"))
toBibtex(citation("stringdist"))
toBibtex(citation("texreg"))
toBibtex(citation("tidycensus"))
toBibtex(citation("tidyverse"))
sink()
