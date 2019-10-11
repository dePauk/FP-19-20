library(dplyr)
library(readr)
library(ggplot2)

# 1. NALOGA

#UVOZ

eur15a <- as.data.frame(read.csv('hist_EURIBOR_2015.csv', row.names = "X"))
eur15prvi <- eur15a[,c(1,22,42,64,84,104,126,149,170,192,214,235)]

eur16a <- as.data.frame(read.csv('hist_EURIBOR_2016.csv', row.names = "X"))
eur16prvi <- eur16a[,c(1,21,42,63,84,106,128,149,172,194,215,237)]

eur17a <- as.data.frame(read.csv('hist_EURIBOR_2017.csv', row.names = "X"))
eur17prvi <- eur17a[,c(1,23,43,66,84,106,128,149,172,193,215,237)]

euribor15 <- t(eur15prvi)
euribor16 <- t(eur16prvi)
euribor17 <- t(eur17prvi)

#tsplot

casovna_vrsta_15 <- euribor15[,c("6m","12m")]
casovna_vrsta_16 <- euribor16[,c("6m","12m")]
casovna_vrsta_17 <- euribor17[,c("6m","12m")]


zdruzeni_euribor_3leta <- rbind(casovna_vrsta_15,casovna_vrsta_16,casovna_vrsta_17)

casovna_vrsta_6m <- ts(data = zdruzeni_euribor_3leta[,c("6m")], frequency = 12, start = c(2015,1))
casovna_vrsta_12m <- ts(data = zdruzeni_euribor_3leta[,c("12m")], frequency = 12, start = c(2015,1))
ts.plot(casovna_vrsta_6m, casovna_vrsta_12m, col= c("red","blue"), main= "Euribor", ylab = "%", lwd = 1)
legend("topright", c("6m","12m"), col=c("red", "blue"), lwd = 1)
                     

# 2. NALOGA

datum1 <- euribor15[12,]  #  1.12.2015
datum2 <- euribor16[3,]  #  1.3.2016
datum3 <- euribor17[1,]  #  2.1.2017


cas <- c(0.25, 0.5, 1, 2, 3, 6, 9, 12)
plot(cas, datum1, type="l", col="black")
lines(cas, datum2, col="red")
lines(cas, datum3, col="blue")








