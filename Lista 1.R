########### Aula 1 - Ex 1 ##########

#3.3 exp(x + x^2) entre -2 e 2

#função transformada
h = function(y) {
  x = 4*y - 2
  exp(x + x^2)
} 

#integral
4*mean(h(runif(10000)))

#3.7 exp(-x^2) entre -inf e inf

#função transformada
h = function(y) {
  x = -log((1-y)/y)
  
  exp(-(x)^2)*((1+exp(-x))^2)/exp(-x)
  
}

mean(h(runif(10000)))

#3.9 exp(-x-y) 0 < y < x, 0 < x < inf

#função transformada
h = function(k, l) {
  x = l/(1-l)
  y = k*x
  
  exp(-(x+y))*x*(1+x)^2
}

mean(h(runif(100000), runif(100000)))


########### Aula 1 - Ex 2 - Q2 ##########

fross5.2 = function(x)
{
  z = (x == 0)*max(Re(polyroot(c(4-runif(1),-4,1)))) +
    (x == 1)*min(Re(polyroot(c(-3-runif(1),4/3,-1/9))))
  
  return(z)
}  

rross5.2 = function(n)
{
  r = rbinom(n,1,3/4)
  return(sapply(r,fross5.2))
}

hist(rross5.2(50000),freq = F)
lines(seq(2,6,0.001),
      c( (seq(2,3,0.001)-2)/2, 
         (2 - seq(3.001,6,0.001)/3)/2 ))

########### Aula 1 - Ex 2 - Q4 ##########

fross5.4 = function(a,b){return((log(1-runif(1))/-a)^(1/b))}

rross5.4 = function(n,a,b)
{
  return(replicate(n,fross5.4(a,b)))
}

hist(rross5.4(50000,1,2),freq = F)

########### Aula 1 - Ex 2 - Q15 ##########

## Método da rejeição a partir de dist. exp(1/2) com alpha = 1/4

n <- 100000
rejq15 = function(n)
{
  k <- 0 #counter for accepted
  j <- 0 #iterations
  y <- numeric(n)
  while (k < n) {
    u <- runif(1)
    j<-j+1
    x <- rexp(1,50/100) #random variate from g
    if (0.5*x * exp(-0.5*x) > u) {
      #we accept x
      k<-k+1
      y[k] <- x
    }
  }
  return(y)
}
hist(rejq15(n),freq = FALSE,breaks = 80,ylim = c(0,0.45), xlim=c(0,8))
lines(seq(0,8,0.01),seq(0,8,0.01)*exp(-seq(0,8,0.01)))

## Método SIR a com proposta exp(-1/2)

Pesos1 <- function(y){
  df <- y*exp(-y)
  dg <- dexp(y,1/2)
  p <- df/dg
  pesos <- p/sum(p)
  return(pesos)
}

rxex_sir <- function(n, m){
  y <- rexp(m,1/2)
  pesos <- Pesos1(y)
  amostra <- sample(x = y, size = n, replace = T, prob = pesos)
  return(amostra)
}

n = 100000
m = 100000

amostra <- rxex_sir(n, m)
hist(amostra, freq = FALSE,breaks = 80, ylim = c(0,0.45),xlim=c(0,8))
lines(seq(0,8,0.01),seq(0,8,0.01)*exp(-seq(0,8,0.01)))

######## Comparação de eficiência #########

install.packages("microbenchmark")
require(microbenchmark)

if(!require(microbenchmark)){
  install.packages("microbenchmark")
  require(microbenchmark)}

# Análise de desempenho

n = 1000
microbenchmark(
  rxex_sir(n,1000),
  rejq15(n),
  times = 1000)

# O algoritmo da rejeição dado os parâmetros usados se mostrou muito superior
# em tempo computacional.

########### Aula 1 - Ex 3 ##########
#3.11
set.seed(10)
################
rmixp = function(n, p) {
  u = rbinom(n, prob = p, size=1)
  x = rnorm(n)
  y = rnorm(n, 3, 1)
  c(x[u==1], y[u==0])
}

dmixp = function(x, p) {
  p*dnorm(x) + (1-p)*dnorm(x, 3, 1)
}


pl = function(p, n=1000) {
  amostra = rmixp(n, p)
  x = seq(min(amostra)-1, max(amostra)+1, 0.001)
  hist(amostra, freq = F, main="Histrograma da amostra simulada")
  lines(x, dmixp(x, p), col='red')
  
}


pl(p=0.75)
pl(p=0.6)
pl(p=0.5)

#mais próximo de p=0.5 -> maior bimodalidade

if(!require("dplyr")){install.packages(dplyr);require(dplyr)}

#3.12
rmgp = function(n, a, b) {
  rgamma(n, a, b) %>% rpois(n, .)
}

rmgp(1000, 4, 2) %>% hist()

########### Aula 1 - Ex 4 ##########

fross4.2 = function(q)
{
  U = runif(1)
  i = 0
  p = exp(-q)
  FP = p
  while(U >= FP)
  {
    p = (q*p)/(i+1)
    FP = FP + p
    i = i + 1
  }
  return(i)
}

rpoisson = function(n,q)
{
  X = rep(q,n)
  return(sapply(X,fross4.2))
}

########### Aula 1 - Ex 5 ##########

fross4h = function(n,p)
{
  i = 1
  
  x = rep(0,length(p))
  
  while (i <= length(p))
  {
    pi = ifelse(i == 1, p[1], p[i]/(1-sum(p[1:(i-1)])))
    x[i] = (n > 0)*sum(rbinom(n = 1, size = n, prob = pi))
    n = n - x[i]
    i = i + 1
  }
  return(x)
}

rmulti = function(n,size,p)
{
  x = t(replicate(n,fross4h(size,p)))
  return(x)
}

########### Aula 1 - Ex 6 ##########

#minha g será uma t com df baixo, para ter uma calda mais pesada

#densidade da skew normal 
dskew = function(x, ep, w, a) {
  (2/w)*dnorm((x-ep)/w)*pnorm(a*((x-ep)/w))
}

#usando SIR 
rskew = function(n, ep, w, a, m) {
  
  g = function(x, ep) dnorm(x, ep, sd=100)
  y = rnorm(m, mean = ep, sd=100)
  w = (dskew(y, ep=0, w=2, a=6)/g(y, ep=0))/sum((dskew(y, ep=0, w=2, a=6)/g(y, ep=0)))
  sample(y, size=n, prob = w, replace = T) 
  
  
}




