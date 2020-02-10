library(data.table)
library(colorspace)

load("phl-fve.Rdata")
load("phl-census2.Rdata")

rm(phlshp, phlfaces)

##### Convert to data.table for join #####
phlfve <- as.data.table(philadelphiafve)
phlcensus <- as.data.table(phl)

rm(philadelphiafve, phl)

phljoin <- merge(phlfve, phlcensus, by = "geoid", all.x = TRUE)

phljoin[, `:=`(DOB2 = as.Date(phljoin$DOB, "%m/%d/%Y"))]   

# WARNING: Dropping One Voter with no DOB
phljoin <- phljoin[!is.na(DOB2)]

electionday <- as.Date("2016-11-08")

age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

phljoin[, `:=`(Age2016 = age(from = DOB2, to = electionday))]

rm(electionday)

##### Hold Age Fixed and Plot Density #####
cairo_ps("Histogram.eps", width = 10, height = 5, onefile = FALSE, bg = "white", pointsize = 12, fallback_resolution = 600)

par(mfrow = c(2,4))

#layout(matrix(c(1,2,3,4,5,6,7,8), nrow = 2, byrow = TRUE))

hist(phljoin$censusage,
     col = rgb(1, 0, 0, 1 / 2),
     xlim = c(18, 90),
     ylim = c(0, 0.1),
     main = "Overlayed Histograms:\nAge and Predicted Age",
     xlab = "Age in Years",
     freq = FALSE)
hist(
  phljoin$Age2016,
  col = rgb(0, 0, 1, 1 / 2),
  xlim = c(18, 90),
  freq = FALSE,
  add = TRUE)

legend(x = "right", c("Age (Voter File)", "Predicted Age\n(Census Data)"), fill = c(rgb(0, 0, 1, 1 / 2), rgb(1, 0, 0, 1 / 2)), cex = 0.8, bty = "n", xpd = TRUE)

for (i in c(18, 27, 36, 45, 54, 63, 72)) {
  hist(
    phljoin[Age2016 == i]$censusage,
    freq = F,
    xlim = c(20, 90),
    ylim = c(0, 0.1),
    main = paste("Histogram of Predicted Ages\nfor Voters Aged", i),
    xlab = "Predicted Age in Years",
    col = rainbow_hcl(7)[which(test == i)[1]]
  )
  abline(v = i)
}

dev.off()
graphics.off()

##### Voting by Age Line Graph #####

phljoin$Vote2016 <- as.factor(phljoin$`Election 39 Vote Method`)

phljoin$Age2016 <-
  round(as.numeric(difftime(as.Date("2016-11-08"), phljoin$DOB, units = "days") / 365))

votebyage <-
  as.data.frame.matrix(table(phljoin$Age2016, phljoin$Vote2016, useNA = "always"))

votebyage <- votebyage[3:85, 1:4]
votebyage$prop <- rowSums(votebyage[2:4]) / rowSums(votebyage[1:4])
votebyage$age <- rownames(votebyage)

votebyage2 <- as.data.frame.matrix(table(phljoin$age, phljoin$Vote2016, useNA = "always"))
votebyage2 <- votebyage2[9:74, 1:4]
votebyage2$prop <- rowSums(votebyage2[2:4]) / rowSums(votebyage2[1:4])
votebyage2$age <- rownames(votebyage2)

votebyage3 <-
  as.data.frame.matrix(table(round((
    phljoin$age + phljoin$Age2016
  ) / 2), phljoin$Vote2016, useNA = "always"))
votebyage3 <- votebyage3[5:76, 1:4]
votebyage3$prop <- rowSums(votebyage3[2:4]) / rowSums(votebyage3[1:4])
votebyage3$age <- rownames(votebyage3)

plot(votebyage3$age, votebyage3$prop, type = "l", main = "Voter Participation in the 2016 General Election by Age\nPhiladelphia, PA", xlab = "Voter Age", ylab = "Participation Proportion")

points(votebyage$age, votebyage$prop, type = "l", lty = "dashed", col = "red")

points(votebyage2$age, votebyage2$prop, type = "l", lty = "dotted", col = "blue")

legend(x = "bottomleft", c("Combined", "Voter File", "Census Data"), col = c("black", "red", "blue"), lty = c("solid", "dashed", "dotted"), cex = 0.8)



