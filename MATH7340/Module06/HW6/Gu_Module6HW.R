n <- 20
nsim <- 1000
n.hyp <- 300
x0.sim<-matrix(NA, ncol=n, nrow=n.hyp)
p.fdr<-p.bon<-p.sim<-matrix(NA, nrow=nsim, ncol=n.hyp)
n.true<-3 
n.fdisc<-n.disc<-rep(NA,nsim) 
for (i in 1:nsim) { 
  x0.sim[,]<-rnorm(n*n.hyp) 
  x0.sim[1:n.true,] <- x0.sim[1:n.true,] + 1 
  p.sim[i,]<-apply(x0.sim,1,function(x) t.test(x,mu=0)$p.value) 
  p.bon[i,]<-p.adjust(p.sim[i,],method='bonferroni') 
  p.fdr[i,]<-p.adjust(p.sim[i,],method='fdr') 
} 
n.disc<-apply(p.sim,1,function(x) sum(x<0.05)) 
n.fdisc<-apply(p.sim[,-(1:n.true)],1,function(x) sum(x<0.05))