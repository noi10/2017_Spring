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
# (a)
2000*0.05

# (b)
pbinom(89, size=2000, prob=0.05)

# problem 3
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