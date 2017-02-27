library(multtest)

# problem 1
# (a)
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
p.values <- apply(golub, 1, function(x) 
  wilcox.test(x~gol.fac, paired = F, alternative = "greater")$p.value )
p.fdr <-p.adjust(p=p.values, method="fdr")
sum(p.fdr < 0.05)

# (b)
golub.gnames[,2][order(p.fdr)][1:3]


# problem 2
rm(list=ls())
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
p.values <- apply(golub[, gol.fac == "AML"], 1, function(x) shapiro.test(x)$p.value) 
p.fdr <-p.adjust(p=p.values, method="fdr")
sum(p.fdr < 0.05)


# problem 3
rm(list=ls())
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
index.HOXA9 <- grep("HOXA9 Homeo box A9", golub.gnames[,2])
index.CD33 <- grep("CD33", golub.gnames[,2])


# Signed-ranks Test
data(golub, package='multtest') 
wilcox.test (x= golub[index.HOXA9, gol.fac == "ALL"], 
             y= golub[index.CD33, gol.fac == "ALL"], paired=T, alternative="two.sided")


# problem 4
apply(UCBAdmissions, 3, function(x) fisher.test(x)$p.value)


# problem 5
rm(list=ls())
data(golub, package='multtest') 
gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))
index.CD33 <- grep("CD33", golub.gnames[,2])
data<-golub[index.CD33, ]
n<-length(data)
T.obs<- var(data[gol.fac=="ALL"])/var(data[gol.fac=="AML"])

#Observed statistic
n.perm=2000
T.perm = rep(NA, n.perm)
for(i in 1:n.perm) {
  data.perm = sample(data, n, replace=F) #permute data
  T.perm[i] = var(data.perm[gol.fac=="ALL"])/var(data.perm[gol.fac=="AML"]) #Permuted statistic
}
mean(T.perm<=T.obs) #p-value