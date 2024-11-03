#install.packages("sjplot")
library(dplyr)
library(vtable)
library(sjplot)
Ex2_S2403 <- ACSST5Y2021_S2403_Data[ , c(1, 2, 3, 7, 23, 27, 31, 47, 55, 71, 107)]
Ex2_S1501 <- ACSST5Y2021_S1501_Data[ , c(1, 2, 287, 291, 295, 299, 303, 307, 311, 315, 235)]
Ex2_1 <- merge(Ex2_S2403, Ex2_S1501, by = c("GEO_ID", "NAME"))
library(dplyr)
Ex2_2 <- mutate (Ex2_1, 
                pct_002 = 100*C01_002E/S2403_C01_001E,
                pct_006 = 100*C01_006E/S2403_C01_001E,
                pct_007 = 100*C01_007E/S2403_C01_001E,
                pct_008 = 100*C01_008E/S2403_C01_001E,
                pct_012 = 100*C01_012E/S2403_C01_001E,
                pct_014 = 100*C01_014E/S2403_C01_001E,
                pct_018 = 100*C01_018E/S2403_C01_001E,
                pct_027 = 100*C01_027E/S2403_C01_001E)
names(Ex2_2) [names(Ex2_2) == "S1501_C01_059E"] <- "MedEarnings"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_008E"] <- "pct_9_12th"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_009E"] <- "pct_HS"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_010E"] <- "pct_somecollege"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_011E"] <- "pct_AssocDeg"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_012E"] <- "pct_BachDeg"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_013E"] <- "pct_GradDeg"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_014E"] <- "pct_HSorHigher"
names(Ex2_2) [names(Ex2_2) == "S1501_C02_015E"] <- "pct_BachorHigher"
library(vtable)
sumtable(Ex2_2)
library(vtable)
sumtable(Ex2_2)
regA <- lm(MedEarnings ~ pct_BachorHigher , data = my.data)
regA <- lm(MedEarnings ~ pct_BachorHigher , data = Ex2_2)
regA <- lm(MedEarnings ~ pct_BachorHigher , data =MedEarningsandBachorHigher)
regA <- lm(MedEarnings ~ pct_BachorHigher , data = Ex2_2)
library(dpylr)
Ex2_2 <- mutate (Ex2_1, 
                 pct_002 = 100*C01_002E/S2403_C01_001E,
                 pct_006 = 100*C01_006E/S2403_C01_001E,
                 pct_007 = 100*C01_007E/S2403_C01_001E,
                 pct_008 = 100*C01_008E/S2403_C01_001E,
                 pct_012 = 100*C01_012E/S2403_C01_001E,
                 pct_014 = 100*C01_014E/S2403_C01_001E,
                 pct_018 = 100*C01_018E/S2403_C01_001E,
                 pct_027 = 100*C01_027E/S2403_C01_001E)
library(dplyr)
Ex2_2 <- mutate(Ex2_1,
                pct_002 = 100*C01_002E/C01_001E,
                pct_006 = 100*C01_006E/C01_001E, 
                pct_007 = 100*C01_007E/C01_001E, 
                pct_008 = 100*C01_008E/C01_001E,
                pct_012 = 100*C01_012E/C01_001E,
                pct_014 = 100*C01_014E/C01_001E,
                pct_018 = 100*C01_018E/C01_001E,
                pct_027 = 100*C01_027E/C01_001E)
regA <- lm(MedEarnings ~ pct_BachorHigher , data = Ex2_2)
regB <- lm(MedEarnings ~ pct_008 , data = Ex2_2)
tab_model(regB)
library("sjplot")
library(Sjplot)
install.packages("sjplot")
library(Sjplot)
install.packages("dplyr")
library(dplyr)
Ex2_2 <- mutate(Ex2_1, 
                pct_002 = 100*C01_002E/C01_001E,
                pct_006 = 100*C01_006E/C01_001E,
                pct_007 = 100*C01_007E/C01_001E,
                pct_008 = 100*C01_008E/C01_001E,
                pct_012 = 100*C01_012E/C01_001E,
                pct_014 = 100*C01_014E/C01_001E,
                pct_018 = 100*C01_018E/C01_001E,
                pct_027 = 100*C01_027E/C01_001E)
install.packages("vtable")
library(vtable)
sumtable(Ex2_2)
RegA <- lm(MedEarnings ~ pct_BachorHigher, data= Ex2_2)
tab_model(RegA)
library(sjplot)
library(Sjplot)
install.packages("Sjplot")
install.packages("sjPlot")
library(sjplot)
library(sjPlot)
tab_model(RegA)
RegB <- lm(MedEarnings ~ pct_008, data= Ex2_2)
tab_model(RegB)
REgC <- lm(MedEarnings ~ pct_9_12th + pct_HS + pct_BachDeg + pct_GradDeg, data= Ex2_2)
RegC <- lm(MedEarnings ~ pct_9_12th + pct_HS + pct_BachDeg + pct_GradDeg, data= Ex2_2)
tab_model(RegC)
RegD <- lm(MedEarnings ~ pct_002 + pct_006 + pct_008 + pct_012 + pct_014 + pct_018, data= Ex2_2)
tab_model(RegD)
RegE <- lm(MedEarnings ~ pct_006 + pct_008 + pct_018 + pct_9_12th + pct_somecollege + pct_BachorHigher, data= Ex2_2)
tab_model(RegE)
