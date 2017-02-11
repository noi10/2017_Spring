# problem 1
# ln(L) = nln(theta) - n(theta)*(mean of X)
# qiu dao shu
# 1/theta - mean of X = 0
# theta = 1/ mean of X

obs <- c(1.636, 0.374, 0.534, 3.015, 0.932, 0.179)
nloglik <- function(theta) {
  -sum(log(dexp(obs, rate = theta)))
}
optim(par = 1, nloglik)

1/mean(obs)

# problem 2
# (a) m = sample mean = 100.8
100.8+qt(0.1, df=52)*(12.4/sqrt(53))
# 98.5891 ~ Inf

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
ci.mean.AML <- mean(Zyxin.AML) + qt(c(0.025,0.975), df=length.AML-1)*sd(Zyxin.AML)/sqrt(length.AML)

# variance
ci.variance.ALL <- c (var(Zyxin.ALL)*(length.ALL-1)/ qchisq(0.975, df=length.ALL-1),
                      var(Zyxin.ALL)*(length.ALL-1)/ qchisq(0.025, df=length.ALL-1))
ci.variance.AML <- c (var(Zyxin.AML)*(length.AML-1)/ qchisq(0.975, df=length.AML-1),
                      var(Zyxin.AML)*(length.AML-1)/ qchisq(0.025, df=length.AML-1))



# lecture page 39
# problem 4
MCsim <- function(nsim, lamb){
  dataset <- matrix(rpois(nsim*50, lambda = lamb), nrow = nsim)
  t0.05 <- qt(0.05, 49)
  t0.95 <- -t0.05
  kai0.95 <- qchisq(0.95, 49)
  kai0.05 <- qchisq(0.05, 49)
  means <- apply(dataset, 1, mean)
  vars <- apply(dataset, 1, var)
  interval.method1 <- matrix(data=NA, nrow=nsim, ncol=2)
  interval.method2 <- matrix(data=NA, nrow=nsim, ncol=2)
  counter1 <- 0
  counter2 <- 0
  for (i in 1:nsim) {
    interval.method1[i,] <- means[i] + c( t0.05*sqrt(means[i]/50) , t0.95*sqrt(means[i]/50))
    interval.method2[i,] <- 49*vars[i]/ ( c( kai0.95, kai0.05) )
    if (interval.method1[i,1] <= lamb && interval.method1[i,2] >= lamb) {
      counter1 <- counter1 + 1
    }
    if (interval.method2[i,1] <= lamb && interval.method2[i,2] >= lamb) {
      counter2 <- counter2 + 1
    }
  }
  print(counter1/nsim)
  print(counter2/nsim)

}