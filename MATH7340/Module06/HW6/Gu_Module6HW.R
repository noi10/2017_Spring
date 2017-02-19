# problem 1
library(multtest)
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))

# (a)
t.test(golub[2972, gol.fac=="ALL"], mu=-0.9, alternative = "greater")

# (b)
t.test(golub[2972, gol.fac=="ALL"],golub[2972, gol.fac=="AML"])

# (c)
t.test(golub[2972, gol.fac=="ALL"]-golub[2989, gol.fac=="ALL"], alternative = "less")

# (d)
res <- golub[2972, gol.fac=="ALL"] < golub[2989, gol.fac=="ALL"]
binom.test(sum(res), length(res), p=1/2, alternative = "greater")

# (e)
res <- golub[2972,] < -0.6
binom.test(sum(res), length(res), p=0.5, alternative = "less")

# (f)
resALL <- golub[2972, gol.fac=="ALL"] < -0.6
obsALL <- sum(resALL); nALL <- length(resALL)
resAML <- golub[2972, gol.fac=="AML"] < -0.6
obsAML <- sum(resAML); nAML <- length(resAML)
prop.test( x=c(obsALL, obsAML), n=c( nALL, nAML ), alternative="two.sided")

# problem 2
rm(list=ls())
# (a)
2000*0.05

# (b)
pbinom(89, size=2000, prob=0.05)

# problem 3
rm(list=ls())
#(a)
x.sim <- matrix(rnorm(10000*20, mean=3, sd=4), ncol=20)
tstat <- function(x) (mean(x)-3)/sd(x)*sqrt(length(x))
tstat.sim <- apply(x.sim, 1, tstat)
power.sim <- mean(tstat.sim > qt(0.3, df=19) & tstat.sim < qt(0.4, df=19))
power.sim+c(-1,0,1)*qnorm(0.975)*sqrt(power.sim*(1-power.sim)/10000)

x.sim <- matrix(rnorm(10000*20, mean=3, sd=4), ncol=20)
tstat <- function(x) (mean(x)-3)/sd(x)*sqrt(length(x))
tstat.sim <- apply(x.sim, 1, tstat)
power.sim <- mean(tstat.sim > qt(0.9, df=19) )
power.sim+c(-1,0,1)*qnorm(0.975)*sqrt(power.sim*(1-power.sim)/10000)

# problem 4
rm(list=ls())
# (a)
data(golub, package = "multtest")
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
p.values <- apply(golub, 1, function(x) 
  {
    xALL = t(as.matrix(x))[,gol.fac=="ALL"]
    xAML = t(as.matrix(x))[,gol.fac=="AML"]
    t.test(xALL, xAML)$p.value
  }
)
p.bon <-p.adjust(p=p.values, method="bonferroni")
p.fdr <-p.adjust(p=p.values, method="fdr")
sum(p.values<0.05)
sum(p.bon<0.05)
sum(p.fdr<0.05)

# (b)
golub.gnames[,2][order(p.values)][1:3]
golub.gnames[,2][order(p.bon)][1:3]
golub.gnames[,2][order(p.fdr)][1:3]

# problem 5
rm(list=ls())
# (a)
# Wald.CI
Wald.CI <- function(x, n, conf.level) {
  p <- x/n
  z <- qnorm(1-(1-conf.level)/2)
  p + c(-1,1)*z*sqrt(p*(1-p)/n)
}
# Wilson.CI
Wilson.CI <- function(x, n, conf.level) {
  p <- x/n
  z <- qnorm(1-(1-conf.level)/2)
  (x+z^2/2)/(n+z^2) + c(-1, 1)*(z*sqrt(n)/(n+z^2))*sqrt(p*(1-p)+z^2/(4*n))
}
# AC.CI
AC.CI <- function(x, n, conf.level) {
  z <- qnorm(1-(1-conf.level)/2)
  x.s <- x + z^2/2
  n.s <- n + z^2
  p.s <- x.s/n.s
  q.s <- 1-p.s
  p.s + c(-1, 1)*z*sqrt(p.s*q.s/n.s)
}

# (b)
n <- 40
p <- 0.2
x.sim <- rbinom(10000, size=n, prob=p)
Wald.sim <- matrix(Wald.CI(x.sim, n=n, conf.level=0.95), nrow=2)
mean(Wald.sim[1,] < p & Wald.sim[2,] > p)
Wilson.sim <- matrix(Wilson.CI(x.sim, n=n, conf.level=0.95), nrow=2)
mean(Wilson.sim[1,] < p & Wilson.sim[2,] > p)
AC.sim <- matrix(AC.CI(x.sim, n=n, conf.level=0.95), nrow=2)
mean(AC.sim[1,] < p & AC.sim[2,] > p)