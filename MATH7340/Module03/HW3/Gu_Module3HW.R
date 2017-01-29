# problem 1
# (b)

# problem 2
f2 <- function(x) {2^x*exp(-2)/gamma(x+1)}
print(f2(1))
print(f2(0)+f2(1)+f2(2)+f2(3))

# problem 3
# n = 3 p = 1/4

# problem 4
rm(list=ls())
# alter way pbinom(2, 3, 0.25)
yrange = c(0:2)
print(sum(dbinom(yrange, size=3, p=0.25)))

Yrange = c(0:3)
EY <- sum(Yrange*dbinom(Yrange, size=3, p=0.25))
print(EY)
VarY <- sum( (Yrange-EY)^2 * dbinom(Yrange, size=3, p=0.25))
print(VarY)

# problem 5
rm(list=ls())
integrate(function(x) dchisq(x, df = 3), lower=1, upper=4)$value
EX <- integrate(function(x) x*dchisq(x, df = 3), lower=-Inf, upper=Inf)$value
print(EX)
VarX <- integrate(function(x) (x-EX)^2*dchisq(x, df = 3), lower=-Inf, upper=Inf)$value
print(VarX)

x <- rchisq(n=100000, df=3)
mean((1<x)&(x<4))

# problem 6
# E(Y) = 10
# Var(Y) = 160
# NO

# problem 7
# (a)
rm(list=ls())
p <- integrate(function(x) dnorm(x, 1.6, 0.4), lower=1, upper=1.6)$value
print(p)
# (b)
x <- rnorm(n=500000, 1.6, 0.4)
print(mean((1<x)&(x<1.6)))
# (c)
print(dbinom(2, 5, p))

# problem 8
rm(list=ls())
m <- 2
n <- 5
print("mean and variance of X~F(m=2, n=5)")
EX<-integrate(function(x) x*df(x, m, n), lower=0, upper=Inf)$value
print(EX)
VarX<-integrate(function(x) (x-EX)^2*df(x, m, n), lower=0, upper=Inf)$value	
print(VarX)
print(n/(n-2))
print( 2*n^2*(m+n-2)/(m*(n-2)^2*(n-4)) )

rm(list=ls())
m <- 10	
n <- 5
print("mean and variance of X~F(m=10, n=5)")
EX<-integrate(function(x) x*df(x, m, n), lower=0, upper=Inf)$value
print(EX)
VarX<-integrate(function(x) (x-EX)^2*df(x, m, n), lower=0, upper=Inf)$value	
print(VarX)
print(n/(n-2))
print( 2*n^2*(m+n-2)/(m*(n-2)^2*(n-4)) )