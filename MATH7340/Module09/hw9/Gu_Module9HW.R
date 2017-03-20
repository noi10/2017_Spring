# problem 1

# (a)
data(golub, package="multtest")
GRO2index <- grep("GRO2 GRO2 oncogene", golub.gnames[,2])
GRO3index <- grep("GRO3 GRO3 oncogene", golub.gnames[,2])
GRO2 <- golub[GRO2index,]
GRO3 <- golub[GRO3index,]
cor(GRO2, GRO3)

# (b)
cor.test(x=GRO2, y=GRO3, conf.level = 0.90)

# (c)
nboot <- 2000 
boot.cor <- rep(NA, nboot) 
data <- cbind(GRO2, GRO3) 
for (i in 1:nboot){
  dat.star <- data[sample(1:nrow(data),replace=TRUE), ] 
  boot.cor[i] <- cor(dat.star[,1], dat.star[,2]) 
}
quantile(boot.cor, c(0.05,0.95)) 

# (d)
quantile(boot.cor, 0.05)


# problem 2
rm(list=ls())
# (a)
data(golub, package="multtest")
Zyxindex <- grep("Zyxin", golub.gnames[,2])
Zyx <- golub[Zyxindex,]
cor.res <- apply(golub, 1, function(x) cor(Zyx, x)) 
length(cor.res[cor.res < -0.5])

# (b)
golub.gnames[,2][order(cor.res)][1:5]

# (c)
p.values <- apply(golub, 1, function(x) cor.test(Zyx, x, alternative = "less")$p.value)
p.fdr <- p.adjust(p=p.values, method="fdr")
sum(p.fdr<0.05)


# problem 3
rm(list=ls())
# (a)
data(golub, package="multtest")
GRO2index <- grep("GRO2 GRO2 oncogene", golub.gnames[,2])
GRO3index <- grep("GRO3 GRO3 oncogene", golub.gnames[,2])
GRO2 <- golub[GRO2index,]
GRO3 <- golub[GRO3index,]
reg.fit <- lm(GRO3 ~ GRO2)
summary(reg.fit)

# (b)
confint(reg.fit, level=0.9)

# (c)
predict(reg.fit, newdata=data.frame(GRO2=0), interval="prediction", level = 0.80)

# (d)
library(lmtest)
plot(reg.fit, which=2)
shapiro.test(resid(reg.fit))

plot(reg.fit, which=1)
bptest(reg.fit, studentize = FALSE)


# problem 4
rm(list=ls())
# (a)
data(stackloss)
lin.reg<-lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc., data=stackloss)
summary(lin.reg)

# (b)
# Air.Flow and Water.Temp have statistical significant effect on stack.loss because of 
# their small p-values which are less than 0.05. Acid.Conc. doesn’t have such effect 
# because p-value = 0.34405.

# (c)
predict(lin.reg, newdata=data.frame(Air.Flow=60, Water.Temp=20, Acid.Conc.=90), 
        interval="confidence", level = 0.9)
predict(lin.reg, newdata=data.frame(Air.Flow=60, Water.Temp=20, Acid.Conc.=90), 
        interval="prediction", level = 0.9)