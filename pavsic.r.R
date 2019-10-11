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


zdruzeni_euribor_3leta <- as.data.frame(rbind(casovna_vrsta_15,casovna_vrsta_16,casovna_vrsta_17))

casovna_vrsta_6m <- ts(data = zdruzeni_euribor_3leta[,c("6m")], frequency = 12, start = c(2015,1))
casovna_vrsta_12m <- ts(data = zdruzeni_euribor_3leta[,c("12m")], frequency = 12, start = c(2015,1))
ts.plot(casovna_vrsta_6m, casovna_vrsta_12m, col= c("red","blue"), main= "Euribor", ylab = "%", lwd = 1)
legend("topright", c("6m","12m"), col=c("red", "blue"), lwd = 1)
                     

# 2. NALOGA

datum1 <- euribor15[12,]  #  1.12.2015
datum2 <- euribor16[3,]  #  1.3.2016
datum3 <- euribor17[1,]  #  2.1.2017


cas <- c(0.25, 0.5, 1, 2, 3, 6, 9, 12)
plot(cas, datum1, type="o", col="black", xlab = "Dospetje [mesec]", ylab = "%", ylim=c(-0.4, 0.1), main="Èasovna struktura Euribor")
lines(cas, datum2, col="red", type="o")
lines(cas, datum3, col="blue", type ="o")
text(10,0.05, "1.12.2015", col="black")
text(10, -0.02, "1.3.2016", col="red")
text(10,-0.090, "2.1.2017", col="blue")



# 3. NALOGA

terminske <- (1/6) * ((1+12*zdruzeni_euribor_3leta[2])/(1+6*zdruzeni_euribor_3leta[1])-1)
terminske <- rbind(NA,NA,NA,NA,NA,NA, terminske)
zdruzeni_euribor_3leta["terminske"] <- head(terminske,36)
colnames(zdruzeni_euribor_3leta) <- c("Euribor6m","Euribor12m", "Napoved6m")


tabela_napovedi_6m <- zdruzeni_euribor_3leta[,c("Euribor6m","Napoved6m")]

# Razsevni graf

leto_1 <- tabela_napovedi_6m[1:12,]
leto_2 <- tabela_napovedi_6m[13:24,]
leto_3 <- tabela_napovedi_6m[25:36,]
dejanski_6m <- as.numeric(tabela_napovedi_6m[, c("Euribor6m")])
napovedan_6m <- as.numeric(tabela_napovedi_6m[, c("Napoved6m")])
model <- lm(napovedan_6m ~ dejanski_6m)

razsevni_graf <- plot(leto_1, main = "6m Euribor 2015-2017", ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "red", xlim=c(-1,1), ylim=c(-1,1))
points(leto_2, y = NULL, col = "blue", pch = 19)
points(leto_3, y = NULL, col = "green", pch = 19)
abline(model)
abline(0, 1, lty = 'dashed')
legend("topleft", c("2015", "2016", "2017"), col=c('red','blue', 'green'), pch=c(19, 19, 19), bty = "n")


# Posamezni grafi


dejanski_6m_2015 <- dejanski_6m[1:12]
napovedan_6m_2015 <- napovedan_6m[1:12]
model_2015 <- glm(napovedan_6m_2015 ~ dejanski_6m_2015)

graf1 <- plot(leto_1, main = "6m Euribor 2015", ylab = "Opazovano", xlab = "Napoved", pch = 19, col = "red", xlim=c(-0.5,0.5), ylim=c(-0.5,0.5))
abline(model_2015, col = "red")
abline(0,1, lty = 'dashed')



dejanski_6m_2016 <- dejanski_6m[13:24]
napovedan_6m_2016 <- napovedan_6m[13:24]
model_2016 <- lm(napovedan_6m_2016 ~ dejanski_6m_2016)

graf2 <- plot(leto_2, main = "6m Euribor 2016", ylab = "Opazovano", xlab = "Napoved", pch = 19, col = "red", xlim=c(-0.5,0.5), ylim=c(-0.5,0.5))
abline(model_2016, col = "red")
abline(0,1, lty = 'dashed')



dejanski_6m_2017 <- dejanski_6m[25:36]
napovedan_6m_2017 <- napovedan_6m[25:36]
model_2017 <- lm(napovedan_6m_2017 ~ dejanski_6m_2017)

graf3 <- plot(leto_3, main = "6m Euribor 2017", ylab = "Opazovano", xlab = "Napoved", pch = 19, col = "blue", xlim=c(-0.5,0.5), ylim=c(-0.5,0.5))
abline(model_2017, col = "blue")
abline(0,1, lty = 'dashed')



# Napovedi so slabe, èe bi bile boljše, bi toèke ležale na oziroma bližje èrtkani èrti.





