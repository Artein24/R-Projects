install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("tseries")
install.packages("urca")
install.packages("aTSA")
install.packages("vtable")
install.packages("sjPlot")
install.packages("ggplotly")
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM.R")
n <- 1000
alpha <- .05
phi1 <- 1
phi2 <- 0
phi3 <- 0
sd <- 1.5
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM2.R")
install.packages("ggplotlyExtra")
library(plotly)
library(ggplot2)
library(dplyr)
ggplotly(p)

#Creating Series 1
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM.R")
n <- 1000
alpha <- 0
phi1 <- 1
phi2 <- 0
phi3 <- 0
sd <- 1
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM2.R")
ggplotly(p)

#Copy of Series 1
series1 <- SIM

#Creating Series 2
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM.R")
n <- 1000
alpha <- .08
phi1 <- 0.6
phi2 <- 0.3
phi3 <- 0.1
sd <- 1.5
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM2.R")
ggplotly(p)

#Copy of Series 2
series2 <- SIM

#Creating Series 3
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM.R")
n <- 1000
alpha <- .4
phi1 <- 0.6
phi2 <- 0.2
phi3 <- 0
sd <- 1.5
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM2.R")
ggplotly(p)

#Copy of Series 3
series3 <- SIM

#Creating Series 4
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM.R")
n <- 1000
alpha <- .8
phi1 <- 0.5
phi2 <- 0.2
phi3 <- 0.1
sd <- 1
source("C:/Users/artei/AppData/Local/Temp/Temp1_AR3_SIM (2).zip/AR3_SIM/SIM2.R")
ggplotly(p)

#Copy of Series 4
series4 <- SIM

#Change column names
names(series1)[names(series1) == "Yt"] <- "series1"
names(series2)[names(series2) == "Yt"] <- "series2"
names(series3)[names(series3) == "Yt"] <- "series3"
names(series4)[names(series4) == "Yt"] <- "series4"

#Merge
Ex4_1 <- merge(series1,series2, by = "t")
Ex4_1 <- merge(Ex4_1,series3, by = "t")
Ex4_1 <- merge(Ex4_1,series4, by = "t")

#Autocorrelation Function
acf(Ex4_1$series1, lag.max = 8)
acf(Ex4_1$series2, lag.max = 8)
acf(Ex4_1$series3, lag.max = 8)
acf(Ex4_1$series4, lag.max = 8)

#Augmented Dickey-Fuller Test
install.packages("aTSA")
library(aTSA)
adf.test(Ex4_1$series1)
adf.test(Ex4_1$series2)
adf.test(Ex4_1$series3)
adf.test(Ex4_1$series4)
library(ggplot2)
library(plotly)
#Plotting Series1 and Series3 together
plot1 <- ggplot(data=Ex4_1, aes(x = t)) +
  geom_line(aes(y = series1), color="#996600") +
  geom_line(aes(y = series3), color="#FFFF66") +
  ylab("Y") + theme_grey(base_size = 10)
plot1

plot1 <- ggplot(data=Ex4_1, aes(x = t)) +
  geom_line(aes(y = series2), color="#FF6699") +
  geom_line(aes(y = series4), color="#0000FF") +
  ylab("Y") + theme_grey(base_size = 10)
plot1

#ACF for Semiconductor Prices
acf(ticker$NVDA, lag.max = 8)
acf(ticker$INTC, lag.max = 8)
acf(ticker$AMD, lag.max = 8)
acf(ticker$TXN, lag.max = 8)

#ADF for Semiconductor Industry
library(aTSA)
adf.test(ticker$NVDA)
adf.test(ticker$INTC)
adf.test(ticker$AMD)
adf.test(ticker$TXN)
library(ggplot2)
library(plotly)
library(dplyr)

#Plot of Stock Prices
plot1 <- ggplot(data=ticker, aes(x = date)) +
  geom_line(aes(y = NVDA), color="#FF3300") +
  geom_line(aes(y = INTC), color="#66FF00") +
  geom_line(aes(y = AMD), color="#0066FF") +
  geom_line(aes(y = TXN), color="#CC0003")
ylab("Y") + theme_grey(base_size = 10)
plot1

#cointegration tests for Series 1 & Series 2
library(aTSA)
coint.test(Ex4_1$series1 , Ex4_1$series2)
coint.test(Ex4_1$series1 , Ex4_1$series3)
coint.test(ticker$INTC , ticker$NVDA)
coint.test(ticker$INTC , ticker$AMD)
coint.test(ticker$AMD , ticker$TXN)

#table of cointegration tests
data= matrix(c(0.01,"No",0.01,"No",0.01,"No",0.01,"No",0.01,"No"), ncol=2, byrow=TRUE)

colnames(data) = c('P-value','Cointegrated')
rownames(data) <- c('series 1 & 2','Series1 & 3','INTC & NVDA','INTC & AMD','AMD & TXN')

Cointegration=as.table(data)

Cointegration   

#new column that numbers observation dates
ticker$t <- as.numeric(row.names(ticker))

#merge Ex4_1 and ticker
Ex4_2 <- merge(Ex4_1, ticker, by="t")

install.packages("dynlm")
library(dynlm)


#Regression in difference form
library(sjPlot)
library(vtable)
library(dplyr)
adl1 <- dynlm(Ex4_2$series1 ~ L(Ex4_2$AMD) + L(Ex4_2$AMD, 1))
adl1
tab_model(adl1)

adl1 <- dynlm(Ex4_2$TXN ~ L(Ex4_2$series2) + L(Ex4_2$series2, 1))
adl1
tab_model(adl1)

adl1 <- dynlm(Ex4_2$INTC ~ L(Ex4_2$NVDA) + L(Ex4_2$NVDA, 1))
adl1
tab_model(adl1)

adl1 <- dynlm(Ex4_2$NVDA ~ L(Ex4_2$AMD) + L(Ex4_2$AMD, 1))
adl1
tab_model(adl1)

adl1 <- dynlm(Ex4_2$INTC ~ L(Ex4_2$TXN) + L(Ex4_2$TXN, 1))
adl1
tab_model(adl1)