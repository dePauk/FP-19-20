library(actuar)

# 1. NALOGA


vzorec <- scan("vzorec1.txt")
graf_a <- hist(vzorec, col = "lightskyblue1", xlab = "Višina odškodnine", main="Histogram odškodnin")

# Pareto se ne začne v 0, tako kot moji podatki -> izberem to porazdelitev


modelirana_porazdelitev <- mde(vzorec,ppareto1,start=list(shape=1,min=3), measure="CvM")
shape1 <- modelirana_porazdelitev$estimate[1]
min1 <- modelirana_porazdelitev$estimate[2]


# freq=FALSE, da se ne bo krivulja izgledala, kot da je na x-osi zaradi različnih skal

graf_c <- hist(vzorec, freq=FALSE, col = "lightskyblue1", xlab = "Višina odškodnine", main="Histogram odškodnin")
graf_c
curve(dpareto1(x, shape1, min1), from=min1, to=10, add = TRUE, col="blue", lwd=2) # add=TRUE doda v prejšnji graf namesto risanja novega
legend("topright", "Pareto porazdelitev", col="blue", lwd=2)


plot(ecdf(vzorec),xlab="Višina odškodnine", ylab="Porazdelitvena funkcija", main="Porazdelitvena fukncija odškodnin")
curve(ppareto1(x,shape1,min1), from=min1, to=11, add= TRUE, col="blue", lwd=2)
legend("bottomright", c("Pareto porazdelitev", "Empirična porazdelitev"), col= c("blue", "black"), lwd = 2)

n <- 20
p <- 0.5

upanjeY <- (shape1*min1)/(shape1 -1)
upanjeN <- n*p

VarY <- ((min1)/(shape1 - 1))^2 * shape1/(shape1 - 2)
VarN <- n*p*(1-p)

upanjeS <- upanjeY * upanjeN
VarS <- upanjeN * VarY + (upanjeY^2)*VarN


# 2.NALOGA

h = 0.25
n = 80

diskr <- discretize(ppareto1(x, shape1, min1), from=0, to=n*h, step=h, method = "rounding")


plot(stepfun(seq(0,(n-1)*h,h), diffinv(diskr)), do.points = FALSE, col="orange", lwd=2, ylab="Porazdelitvena funkcija", xlab= "x", main="Pareto porazdelitev")
curve(ppareto1(x, shape1, min1), from=0, to=10, add=TRUE, lwd=1)

j <- aggregateDist(method = "recursive", model.freq = "binom", size=20, prob=0.5, 
                   model.sev=diskr, maxit=1000000, tol=0.002, convolve=0, x.scale=h)

s_upanje <- sum(knots(j)*diff(j))
s_upanje_kvadrat  <- sum(knots(j)^2 * diff(j))
s_varianca <- s_upanje_kvadrat - s_upanje^2


# 3. NALOGA

sim_N <- rbinom(10000, 20, 0.5)
sim_S <- c()

for (n in sim_N){
  sim_S <- c(sim_S, sum(rpareto1(n, shape1, min1)))
}

E_simS <- mean(sim_S)
var_simS <- var(sim_S)

graf_simulacija <- plot(j)
plot(ecdf(sim_S), add = TRUE, col = "red")
legend("bottomright", legend = c("Panjerjev algoritem", "Monte Carlo simulacija"), box.lty=0,
    col = c("black", "red"), lwd = 2:2)




















