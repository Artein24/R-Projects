Sys.setenv(CENSUS_KEY="9a4e8ccb152e282a17be24afe4a64d35a311e963")
Sys.getenv("CENSUS_KEY")
library(censusapi)
library(dplyr)
library(vtable)
library(sjPlot)
pums21 <- getCensus (
  name = "acs/acs1/pums",
  vintage = 2021,
  vars = c(
    "TYPEHUGQ",
    "BLD",
    "TEN",
    "HHL",
    "SERIALNO",
    "SPORDER",
    "AGEP",
    "SEX",
    "CIT",
    "RAC1P",
    "COW",
    "SCHL",
    "OCCP",
    "WKHP",
    "PERNP",
    "JWMNP",
    "JWTRNS",
    "DIS",
    "HISPEED",
    "NOC",
    "NR"
  ),
  region = "public use microdata area:*",
  regionin = "state:06")
pums19 <- getCensus (
  name = "acs/acs1/pums",
  vintage = 2019,
  vars = c(
    "TYPE",
    "BLD",
    "TEN",
    "HHL",
    "SERIALNO",
    "SPORDER",
    "AGEP",
    "SEX",
    "CIT",
    "RAC1P",
    "COW",
    "SCHL",
    "OCCP",
    "WKHP",
    "PERNP",
    "JWMNP",
    "JWTRNS",
    "DIS",
    "HISPEED",
    "NOC",
    "NR"
  ),
  region = "public use microdata area:*",
  regionin = "state:06")
pums21 <- mutate(pums21, OCCP = na_if(OCCP,'9'))
pums21 <- mutate(pums21, NOC = na_if(NOC,'-1'))
pums19 <- mutate(pums19, NOC = na_if(NOC,'-1'))
pums19 <- mutate(pums19, OCCP = na_if(OCCP,'9'))
pums21$OCCP <- as.numeric(pums21$OCCP)
pums21$NOC <- as.numeric(pums21$NOC)
pums21$AGEP <- as.numeric(pums21$AGEP)
pums21$JWMNP <- as.numeric(pums21$JWMNP)
pums21$WKHP <- as.numeric(pums21$WKHP)
pums21$PERNP <- as.numeric(pums21$PERNP)
pums21$SCHL <- as.numeric(pums21$SCHL)
pums19$OCCP <- as.numeric(pums19$OCCP)
pums19$NOC <- as.numeric(pums19$NOC)
pums19$AGEP <- as.numeric(pums19$AGEP)
pums19$JWMNP <- as.numeric(pums19$JWMNP)
pums19$WKHP <- as.numeric(pums19$WKHP)
pums19$PERNP <- as.numeric(pums19$PERNP)
pums19$SCHL <- as.numeric(pums19$SCHL)
pums21$DIS <- ifelse(pums21$DIS == '1', 1, 0)
pums19$DIS <- ifelse(pums19$DIS == '1', 1, 0)
pums21$NR <- ifelse(pums21$NR == '1', 1, 0)
pums19$NR <- ifelse(pums19$NR == '1', 1, 0)
pums21$HISPEED <- ifelse(pums21$HISPEED == '1', 1, 0)
pums19$HISPEED <- ifelse(pums19$HISPEED == '1', 1, 0)
pums21$Owner <- ifelse(pums21$TEN %in% c("1","2"), 1, 0)
pums19$Owner <- ifelse(pums19$TEN %in% c("1","2"), 1, 0)
pums21$SFR <- ifelse(pums21$BLD %in% c("2","3"), 1, 0)
pums19$SFR <- ifelse(pums19$BLD %in% c("2","3"), 1, 0)
pums21$BachorHigher <- ifelse(pums21$SCHL > 20, 1, 0)
pums19$BachorHigher <- ifelse(pums19$SCHL > 20, 1, 0)
pums21$SEX <- as.factor(pums21$SEX)
pums19$SEX <- as.factor(pums19$SEX)
pums21$Group1 <- ifelse(pums21$OCCP < 1000, 1, 0)
pums19$Group1 <- ifelse(pums19$OCCP < 1000, 1, 0)
pums21$Group2 <- ifelse(pums21$OCCP < 1300 & pums21$OCCP < 2000, 1, 0)
pums19$Group2 <- ifelse(pums19$OCCP < 1300 & pums19$OCCP < 2000, 1, 0)
pums21$Group3 <- ifelse(pums21$OCCP >= 2600 & pums21$OCCP < 3000, 1, 0)
pums19$Group3 <- ifelse(pums19$OCCP >= 2600 & pums19$OCCP < 3000, 1, 0)
pums21$Group4 <- ifelse(pums21$OCCP >= 3000 & pums21$OCCP < 3700, 1, 0)
pums19$Group4 <- ifelse(pums19$OCCP >= 3000 & pums19$OCCP < 3700, 1, 0)
pums21$Group5 <- ifelse(pums21$OCCP >= 4000 & pums21$OCCP < 4200, 1, 0)
pums19$Group5 <- ifelse(pums19$OCCP >= 4000 & pums19$OCCP < 4200, 1, 0)
pums21$Group6 <- ifelse(pums21$OCCP >= 6200 & pums21$OCCP < 6800, 1, 0)
pums19$Group6 <- ifelse(pums19$OCCP >= 6200 & pums19$OCCP < 6800, 1, 0)
pums21$WFH <- ifelse(pums21$JWTRNS == '11', 1, 0)
pums19$WFH <- ifelse(pums19$JWTRNS == '11', 1, 0)
workers21 <- pums21[pums21$COW != '0' &
                      pums21$PERNP > 0 &
                      pums21$TYPEHUGQ == '1' &
                      pums21$JWTRNS != '0',]
workers19 <- pums19[pums19$COW != '0' &
                      pums19$PERNP > 0 &
                      pums19$TYPE == '1' &
                      pums19$JWTRNS != '0',]

#Summary Statistics Tables

sumtable(pums21)
sumtable(pums19)
sumtable(workers21)
sumtable(workers19)

#Histogram JWMNP vs PERNP

attach(pums19)
hist(JWMNP, breaks=20)
hist(PERNP, breaks=20)

attach(pums21)
hist(JWMNP, breaks=20)
hist(PERNP, breaks=20)

#Scatterplot

plot(PERNP, JWMNP, xlab = "Earnings",
     ylab = "Travel Time to Work")

#Ordinary Least Squares Models

OLS21 <- lm(JWMNP ~ JWTRNS + NOC + BLD + DIS, data = workers21)
tab_model(OLS21)

OLS19 <- lm(JWMNP ~ JWTRNS + NOC + BLD + DIS, data = workers19)
tab_model(OLS19)

#Crosstab

tab_xtab(var.row = workers21$JWMNP,
         var.col = workers21$SEX,
         title = "worker21: Crosstab Means of Transportation to Work & SEX",
         show.row.prc = TRUE)

tab_xtab(var.row = workers19$JWMNP,
         var.col = workers19$SEX,
         title = "worker19: Crosstab Means of Transportation to Work & SEX",
         show.row.prc = TRUE)

#Logistic Regressions

Logit19 <- glm(WFH ~ AGEP + COW + SCHL + HISPEED, family = "binomial" , data = workers19)
tab_model(Logit19)

Logit21 <- glm(WFH ~ AGEP + COW + SCHL + HISPEED, family = "binomial" , data = workers21)
tab_model(Logit21)
