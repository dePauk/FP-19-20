library(combinat)
library(Rlab)

#1. naloga OK
#1.a OK
S_0 <- 50 
u <- 1.05
d <- 0.95
t <- 5
R <- 0.03
W <- c(1,2,3,4,5,6)

pot1 <- c(50.00, 52.50, 49.88, 47.38, 45.01, 47.26)
pot2 <- c(50.00, 52.50, 55.12, 57.88, 60.78, 63.81)
pot3 <- c(50.00, 47.50, 49.88, 47.38, 45.01, 42.76)
pot4 <- c(50.00, 47.50, 45.12, 47.38, 45.01, 47.26)
pot5 <- c(50.00, 52.50, 49.88, 52.37, 54.99, 52.24)

# Izplačila

izplacilo.x1 <- izplacilo(pot1,W, "call")
izplacilo.x2 <- izplacilo(pot2,W, "call")
izplacilo.x3 <- izplacilo(pot3,W, "call")
izplacilo.x4 <- izplacilo(pot4,W, "call")
izplacilo.x5 <- izplacilo(pot5,W, "call")

#x <- c(izplacilo.x1, izplacilo.x2, izplacilo.x3, izplacilo.x4, izplacilo.x5)

izplacilo.y1 <- izplacilo(pot1,W, "put")
izplacilo.y2 <- izplacilo(pot2,W, "put")
izplacilo.y3 <- izplacilo(pot3,W, "put")
izplacilo.y4 <- izplacilo(pot4,W, "put")
izplacilo.y5 <- izplacilo(pot5,W, "put")


#1.b OK

izplacilo <- function(vrsta, W, type){
  utezeno_povprecje <- sum(W * vrsta) / sum(W)
  if (type == "call")
  {izplacilo <- max(vrsta[length(vrsta)] - utezeno_povprecje, 0)}
  else
  {izplacilo <- max(0, utezeno_povprecje - vrsta[length(vrsta)])}
  return (izplacilo)
}


#2. naloga
#2.a
binomski <- function(S_0,u,d,R,t,W,type){
  q = (1+R-d) / (u-d)
  kocka <- hcube(rep(2,t)) - 1
  vektor_S_0 <- rep(S_0, 2^(t))
  nova_kocka <- cbind(vektor_S_0,u^kocka * d^(1-kocka))
  produkt <- t(apply(nova_kocka,1, cumprod))
  izplacilo_po_vrsticah <- apply(produkt, 1, izplacilo, W = W, type = type)
  stevilo_u <- rowSums(kocka)
  stevilo_d <- t - stevilo_u
  Q <- q^stevilo_u * (1-q)^stevilo_d
  E_Q <- sum(izplacilo_po_vrsticah * Q)
  premija <- E_Q / (1+R)^t
  return (premija)
}

binomski(S_0,u,d,R,t,W,"call")
binomski(S_0,u,d,R,t,W,"put")

#2.b
monte <- function(S_0,u,d,R,t,W,type, N){
  q = (1+R-d) / (u-d)
  binomska <- matrix(rbinom(N*t,1,q),nrow = N, ncol = t)
  vektor_S_0 <- rep(S_0, N)
  nova_binomska <- cbind(vektor_S_0,u^binomska * d^(1-binomska))
  produkt <- t(apply(nova_binomska,1, cumprod))
  izplacilo_po_vrsticah <- apply(produkt, 1, izplacilo, W = W, type = type)
  stevilo_u <- rowSums(binomska)
  stevilo_d <- t - stevilo_u
  Q <- q^stevilo_u * (1-q)^stevilo_d
  E_Q <- sum(izplacilo_po_vrsticah) / N
  premija <- E_Q / (1+R)^t
  return (premija)
}

S_0 <- 60
u <- 1.05
d <- 0.95
R <- 0.01
t <- 15
W <- rep(1,16)
type <- "put"
N1 <- 10
N2 <- 100
N3 <- 1000

monte(S_0,u,d,R,t,W,type, N1)
monte(S_0,u,d,R,t,W,type, N2)
monte(S_0,u,d,R,t,W,type, N3)

b <- binomski(S_0,u,d,R,t,W,type)

#3. naloga
#3.a
M = 100
simulacija_1 <- c()
for(i in 1:M ){
  simulacija_1 <- c(simulacija_1,monte(S_0,u,d,R,t,W,type, N1))
}

simulacija_2 <- c()
for(i in 1:M ){
  simulacija_2 <- c(simulacija_2,monte(S_0,u,d,R,t,W,type, N2))
}

simulacija_3 <- c()
for(i in 1:M ){
  simulacija_3 <- c(simulacija_3,monte(S_0,u,d,R,t,W,type, N3))
}


#3.b
graf1 <- hist(simulacija_1, main = ("Monte carlo: N = 10"), col = "deepskyblue2", xlab = "Premija", 
              xlim = c(0,5))
abline(v = c(mean(simulacija_1),b ),col = c("green", "red"), lty = c(1, 3), lwd = 2)
arrows(mean(simulacija_1) , 0, mean(simulacija_1) + sd(simulacija_1),0, col = "green", lwd=2,
       length = 0.1)
arrows(mean(simulacija_1) , 0, mean(simulacija_1) - sd(simulacija_1),0, col = "green", lwd=2,
       length =  0.1)
legend("topright", legend = c("Monte Carlo", "analiza modela"),  box.lty = 0, col = c("green","red"), 
       lty = c(1, 3), lwd=2)

graf2 <- hist(simulacija_2, main = ("Monte carlo: N = 100"), col = "deepskyblue2", xlab = "Premija", 
              xlim = c(0,5))
abline(v = c(mean(simulacija_2),b ),col = c("green", "red"), lty = c(1, 3), lwd = 2)
arrows(mean(simulacija_2) , 0, mean(simulacija_2) + sd(simulacija_2),0, col = "green", lwd=2,
       length = 0.1)
arrows(mean(simulacija_2) , 0, mean(simulacija_2) - sd(simulacija_2),0, col = "green", lwd=2,
       length =  0.1)
legend("topright", legend = c("Monte Carlo", "analiza modela"),  box.lty = 0, col = c("green","red"), 
       lty = c(1, 3), lwd=2)

graf3 <- hist(simulacija_3, main = ("Monte carlo: N = 1000"), col = "deepskyblue2", xlab = "Premija", 
              xlim = c(0,5))
abline(v = c(mean(simulacija_3),b ),col = c("green", "red"), lty = c(1, 3), lwd = 2)
arrows(mean(simulacija_3) , 0, mean(simulacija_3) + sd(simulacija_3),0, col = "green", lwd=2,
       length = 0.1)
arrows(mean(simulacija_3) , 0, mean(simulacija_3) - sd(simulacija_3),0, col = "green", lwd=2,
       length = 0.1)
legend("topright", legend = c("Monte Carlo", "analiza modela"),  box.lty = 0, col = c("green","red"), 
       lty = c(1, 3), lwd=2)

