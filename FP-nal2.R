library(actuar)

# NALOGA 1

# a)

vzorec <- scan("vzorec1.txt")
graf_a <- hist(vzorec, col = "lightskyblue1", xlab = "Višina odškodnine", main="Histogram odškodnin")

# Pareto se ne začne v 0, tako kot moji podatki -> izberem to porazdelitev


# b)

modelirana_porazdelitev <- mde(vzorec,ppareto1,start=list(shape=1,min=3), measure="CvM")
shape1 <- modelirana_porazdelitev$estimate[1]
min1 <- modelirana_porazdelitev$estimate[2]


# c)

# freq=FALSE, da se ne bo krivulja izgledala, kot da je na x-osi zaradi različnih skal

graf_c <- hist(vzorec, freq=FALSE, col = "lightskyblue1", xlab = "Višina odškodnine", main="Histogram odškodnin")
graf_c
curve(dpareto1(x, shape1, min1), from=min1, to=10, add = TRUE, col="blue", lwd=2) # add=TRUE doda v prejšnji graf namesto risanja novega
legend("topright", "Pareto porazdelitev", col="blue", lwd=2)


plot(ecdf(vzorec),xlab="Višina odškodnine", ylab="Porazdelitvena funkcija", main="Porazdelitvena fukncija odškodnin")
curve(ppareto1(x,shape1,min1), from=min1, to=11, add= TRUE, col="blue", lwd=2)
legend("bottomright", c("Pareto porazdelitev", "Empirična porazdelitev"), col= c("blue", "black"), lwd = 2)


# d)

n <- 20
p <- 0.5

upanjeY <- (shape1*min1)/(shape1 -1)
upanjeN <- n*p

VarY <- ((min1)/(shape1 - 1))^2 * shape1/(shape1 - 2)
VarN <- n*p*(1-p)

upanjeS <- upanjeY * upanjeN
VarS <- upanjeN * VarY + (upanjeY^2)*VarN


# NALOGA 2

diskretizirano <- diffinv(discretize(ppareto1(x, shape1, min1), step=0.25, from=0, to=10, method = "rounding"))
plot(stepfun(seq(0,9.75,0.25), diskretizirano), do.points = FALSE, col="orange", lwd=2, ylab="Porazdelitvena funkcija", xlab= "x", main="Pareto porazdelitev")
curve(ppareto1(x, shape1, min1), from=0, to=10, add=TRUE, lwd=1)






















