# problem 1
# ln(L) = nln(theta) - n(theta)*(mean of X)
# Derivative
# 1/theta - mean of X = 0
# theta = 1/ mean of X

obs <- c(1.636, 0.374, 0.534, 3.015, 0.932, 0.179)
# Analytic formula
1/mean(obs)

# Numerical Optimization
nloglik <- function(theta) {
  -sum(log(dexp(obs, rate = theta)))
}
optim(par = 1, nloglik)$par



# problem 2
# (a) m = sample mean = 100.8
100.8+qt(0.1, df=52)*(12.4/sqrt(53))
print("98.5891 ~ Inf")

# problem 3
data(golub, package = 'multtest')
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
Zyxin.ALL <- golub[ grep("Zyxin", golub.gnames[,2]), gol.fac == "ALL"]
Zyxin.AML <- golub[ grep("Zyxin", golub.gnames[,2]), gol.fac == "AML"]
length.ALL <- length(Zyxin.ALL)
length.AML <- length(Zyxin.AML)

# (a) and (c)
nboot <- 1000
boot.mean.ALL <- rep(NA, nboot)
boot.mean.AML <- rep(NA, nboot)
boot.var.ALL <- rep(NA, nboot)
boot.var.AML <- rep(NA, nboot)
boot.median.ALL <- rep(NA, nboot)
boot.median.AML <- rep(NA, nboot)
for (i in 1:nboot) {
  data.ALL.star <- Zyxin.ALL[sample(1:length.ALL, replace=TRUE)]
  data.AML.star <- Zyxin.AML[sample(1:length.AML, replace=TRUE)]
  boot.mean.ALL[i] <- mean(data.ALL.star)
  boot.mean.AML[i] <- mean(data.AML.star)
  boot.var.ALL[i] <- var(data.ALL.star)
  boot.var.AML[i] <- var(data.AML.star)
  boot.median.ALL[i] <- median(data.ALL.star)
  boot.median.AML[i] <- median(data.AML.star)
}
quantile(boot.mean.ALL,c(0.025,0.975))
quantile(boot.mean.AML,c(0.025,0.975))

quantile(boot.var.ALL, c(0.025,0.975))
quantile(boot.var.AML, c(0.025,0.975))

quantile(boot.median.ALL,c(0.025,0.975))
quantile(boot.median.AML,c(0.025,0.975))

# (b)
# mean
ci.mean.ALL <- mean(Zyxin.ALL) + qt(c(0.025,0.975), df=length.ALL-1)*sd(Zyxin.ALL)/sqrt(length.ALL)
ci.mean.ALL
ci.mean.AML <- mean(Zyxin.AML) + qt(c(0.025,0.975), df=length.AML-1)*sd(Zyxin.AML)/sqrt(length.AML)
ci.mean.AML

# variance
ci.variance.ALL <- c (var(Zyxin.ALL)*(length.ALL-1)/ qchisq(0.975, df=length.ALL-1),
                      var(Zyxin.ALL)*(length.ALL-1)/ qchisq(0.025, df=length.ALL-1))
ci.variance.ALL
ci.variance.AML <- c (var(Zyxin.AML)*(length.AML-1)/ qchisq(0.975, df=length.AML-1),
                      var(Zyxin.AML)*(length.AML-1)/ qchisq(0.025, df=length.AML-1))
ci.variance.AML

# problem 4
# (a)
MCsim <- function(nsim, lamb){
  dataset <- matrix(rpois(nsim*50, lambda = lamb), nrow = nsim)
  t0.05 <- qt(0.05, 49)
  t0.95 <- -t0.05
  kai0.95 <- qchisq(0.95, 49)
  kai0.05 <- qchisq(0.05, 49)
  means <- apply(dataset, 1, mean)
  vars <- apply(dataset, 1, var)
  in.method1 <- rep(0, nsim)
  in.method2 <- rep(0, nsim)
  for (i in 1:nsim) {
    lower1 <- means[i] + t0.05*sqrt(means[i]/50)
    upper1 <- means[i] + t0.95*sqrt(means[i]/50)
    lower2 <- 49*vars[i]/ kai0.95
    upper2 <- 49*vars[i]/kai0.05
    if (lower1 < lamb && upper1 > lamb) {
      in.method1[i] <- 1
    }
    if (lower2 < lamb && upper2 > lamb) {
      in.method2[i] <- 1
    }
  }
  cat("proportion of the CIs that contains the true lambda using mean:", mean(in.method1), "\n")
  cat("proportion of the CIs that contains the true lambda using variance:", mean(in.method2), "\n")
}

# (b)
MCsim(1000, 0.1)
MCsim(1000, 1)
MCsim(1000, 10)

# (c)
# I would choose sample mean as my CI formula in practice. 
# If we look into the formulas deeply, we could draw the conclusion about the lengths of CIs of these two 
# methods. The lengths of CIs are related to $\sqrt{\bar{X}}$ and $s^2$ for sample mean and sample 
# variance. That is to say, the CI would be too short when $\lambda$ is small for the method of sample 
# variance. Similarly, the CI would be too long when $\lambda$ is large. In other words, the performance 
# of sample variance method is not stable so I prefer to choose sample mean method for estimating the 
# parameter $\lambda$ of poisson distribution.