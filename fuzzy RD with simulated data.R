# Two-sided FRD with simulated data
# Chi Huang

rm(list=ls())

set.seed(135790)

# generate bivariate standard Normal with rho=.5
rbvn<-function (n, m1, s1, m2, s2, rho) 
{
  x <- rnorm(n, m1, s1)
  e <- rnorm(n, m2 + (s2/s1) * rho * 
                (x - m1), sqrt((1 - rho^2)*s2^2))
  cbind(x, e)
}
bvn <- rbvn(n=10000, m1=0, s1=1, m2=0, s2=1, rho=0.5)

bvn <- as.data.frame(bvn)
attach(bvn)

# exogenous treatment assignment Z
z <- ifelse(x>0, 1, 0)

# actual treatment take-up d
d <- ifelse(runif(10000)<=.8, z, 1-z)

# define y with potential outcomes y1 and y0
y1 <- 1+x-x^2+e 
y0 <- 0+x-x^2+e
y <- y0+d*(y1-y0)

# define triangular kernel weight
w0=1-abs(x)
w <- ifelse(w0>=0, w0, 0)

# show the relationship between z and d: z!=d
table(z, d)

# load packages
library(AER)
library(stargazer)
library(rddensity)
library(rdrobust)

# Outcome Analysis
# IV version
ols1 <- lm(y~x*d+d)
iv1 <- ivreg(y~x*d+d | x*z+z, weights=w)
stargazer(ols1, iv1, type="text")

# Fuzzy RD: probability of actual treatment receipt d
rdplot(d, x, c=0, nbins=10000, p=1, x.label="Score X", y.label="Prob. of treatment receipt D")

# Fuzzy RD: global polynomial
rdplot(y, x, c=0, nbins=10000, x.label="Score X", y.label="Outcome Y")

# Fuzzy RD: data-driven local polynomial with triangular kernel
frd1 <- rdrobust (y, x, fuzzy=d, kernel="triangular", h=1, bwselect="mserd")
summary(frd1)

# Fuzzy RD: local linear regression plot with h=1 & p=1
rdplot(y, x, x.lim=c(-1, 1), y.lim=c(-5, 2), h=1, p=1, binselect="esmv", kernel="triangular", x.label="Score X", y.label="Outcome Y")
