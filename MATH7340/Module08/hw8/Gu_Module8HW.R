library(ALL)
library(lmtest)

# problem 1

# (a)
data(ALL)
ALLBgroups <- ALL[,ALL$BT %in% c("B", "B1", "B2", "B3", "B4")]
y <- exprs(ALLBgroups)["109_at",]
anova( lm( y ~ ALLBgroups$BT ) )

# (b)
summary( lm( y ~ ALLBgroups$BT ) )

# (c)
pairwise.t.test(y, ALLBgroups$BT)

# (d)
pairwise.t.test(y,ALLBgroups$BT, p.adjust.method='fdr')

# (e)
shapiro.test( residuals( lm( y ~ ALLBgroups$BT )))
bptest( lm( y ~ ALLBgroups$BT ), studentize = FALSE)


# problem 2
rm(list=ls())

# (a)
data(ALL)
ALLBgroups <- ALL[,ALL$BT %in% c("B", "B1", "B2", "B3", "B4")]
y <- exprs(ALLBgroups)
p.values <- apply(y, 1, function(x) kruskal.test(x ~ ALLBgroups$BT)$p.value)
sum(p.values < 0.05)

# (b)
rownames(exprs(ALLBgroups))[order(p.values)][1:3]


# problem 3
rm(list=ls())

# (a)
data(ALL)
ALLBs <- ALL[,which(ALL$BT %in% c("B1","B2","B3","B4"), ALL$sex %in% c("F", "M"))]
y <- exprs(ALLBs)["38555_at",]
Bcell <- ALLBs$BT
sex <- ALLBs$sex
anova( lm( y ~ Bcell*sex ) )

# (b)
shapiro.test( residuals( lm( y ~ Bcell + sex )))
bptest(lm(y ~ Bcell + sex), studentize = FALSE)


# problem 4
rm(list=ls())

# (a)
permutation.test <- function(probe, groups) {
  data(ALL)
  ALLB123 <- ALL[,ALL$BT %in% groups]
  data<- exprs(ALLB123)[probe,]
  group<-ALLB123$BT[,drop=T]
  n <- length(data)
  
  estimatedMean <- mean(by(data, group, mean))
  T.obs <- sum( (by(data, group, mean) - estimatedMean) ^ 2) / 2
  
  n.perm <- 2000
  T.perm <- rep(NA, n.perm)
  for(i in 1:n.perm) {
    data.perm <- sample(data, n, replace=F) #permute data
    estimatedMean <- mean( by(data.perm, group, mean) )
    T.perm[i] <- sum( (by(data.perm, group, mean) - estimatedMean) ^ 2) / 2 #Permuted statistic
  }
  mean(T.perm >= T.obs) #p-value
}  

# (b)
permutation.test("1242_at", c("B1", "B2", "B3"))