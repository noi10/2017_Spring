# problem 1
#(a)
data(golub, package="multtest")
clusdata <- data.frame(golub[1042,])
colnames(clusdata)<-c("CCND3 Cyclin D3")
gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))

hc.single <- hclust(dist(clusdata, method="euclidian"), method="single")
plot(hc.single, hang=-1, labels=gol.fac)
rect.hclust(hc.single, k=2)
groups.single <- cutree(hc.single, k=2)
table(gol.fac, groups.single)

hc.ward <- hclust(dist(clusdata,method="euclidian"),method="ward.D2")  
plot(hc.ward, hang=-1, labels=gol.fac)
rect.hclust(hc.ward, k=2)
groups.ward <- cutree(hc.ward, k=2)
table(gol.fac, groups.ward)

# (b)
cl.2mean <- kmeans(clusdata, centers=2, nstart = 10)
table(gol.fac, cl.2mean$cluster)

# (c)
# equally good

# (d)
initial <-cl.2mean$centers
n <- dim(clusdata)[1]
nboot<-1000
boot.cl <- matrix(NA,nrow=nboot,ncol = 2)
for (i in 1:nboot){ 
  dat.star <- clusdata[sample(1:n,replace=TRUE),]
  cl <- kmeans(dat.star, initial, nstart = 10)
  boot.cl[i,] <- c(cl$centers[,1])
}
apply(boot.cl,2,mean)
quantile(boot.cl[,1],c(0.025,0.975))
quantile(boot.cl[,2],c(0.025,0.975))

# (e)
K = (1:30)
sse<-rep(NA,length(K)) 
for (k in 1:30) { 
  sse[k]<-kmeans(clusdata, centers=k,nstart = 10)$tot.withinss 
} 
plot(K, sse, type='o', xaxt='n'); axis(1, at = K, las=2)


# problem 2
rm(list=ls())
# (a)
data(golub, package="multtest")

oncoIndex <- grep("oncogene", golub.gnames[,2])
antiIndex <- grep("antigen", golub.gnames[,2])
tag <- c(rep(0, length(oncoIndex)), rep(1, length(antiIndex)))
clusdata <- rbind(golub[oncoIndex,], golub[antiIndex,])
gol.fac <- factor(tag,levels=0:1, labels= c("oncogene","antigen"))

# (b)
library(cluster)
cl.2mean <- kmeans(clusdata, centers=2, nstart=10)
meanTable <- table(gol.fac, cl.2mean$cluster)
meanTable

cl.2medoid <- pam(dist(clusdata, method='eucl'), k=2)
medoidTable <- table(gol.fac, cl.2medoid$cluster)
medoidTable

# (c)
fisher.test(meanTable)
fisher.test(medoidTable)

# (d)
hc.single <- hclust(dist(clusdata, method="euclidian"), method="single")
plot(hc.single, hang=-1, labels=gol.fac, cex=0.38)
rect.hclust(hc.single, k=2)

hc.complete <- hclust(dist(clusdata, method="euclidian"), method="complete")
plot(hc.complete, hang=-1, labels=gol.fac, cex=0.38)
rect.hclust(hc.complete, k=2)


# problem 3
rm(list=ls())

# (a)
library(ISLR)
ncidata <- NCI60$data
ncilabs <- NCI60$labs

K = (1:30)
sse<-rep(NA,length(K)) 
for (k in K) { 
  sse[k]<-kmeans(ncidata, centers=k, nstart = 10)$tot.withinss 
} 
plot(K, sse, type='o', xaxt='n'); axis(1, at = K, las=2)

# (b)
library(cluster)
cl.pam<-pam(as.dist(1-cor(t(ncidata))), k=7)
table(ncilabs, cl.pam$cluster)
