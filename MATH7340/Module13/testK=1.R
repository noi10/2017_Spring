rm(list=ls())
library(VGAM)
pca.iris<-prcomp(iris[,1:4], scale=TRUE)
Species <- iris$Species
n <- length(Species)
data.pca <- pca.iris$x[, 1:1]
iris2 <- data.frame(Species, data.pca)


mcr.cv.raw<-rep(NA, n) 
for (i in 1:n) {
  print(i)
  #lgr.fit <- vglm(Species~., family=multinomial, data=iris2[-i,])
  #pred.prob <- predict(lgr.fit, data = iris2[i,], type="response")
  #print(pred.prob)
  #pred <- apply(pred.prob, 1, which.max) 
  fit.lgr <- vglm(Species~., family=multinomial, data=iris2[-i,]) 
  #fit logistic regression without i-th observation 
  newdata = iris2[i, ]
  pred.prob <- predict(fit.lgr, newdata, type="response") 
  print(pred.prob)
  #get prediction probability 
  pred <- apply(pred.prob, 1, which.max) #Assign class 
  pred <- factor(pred, levels=c("1","2","3"), labels=levels(iris2$Species)) 
  #print(pred)
  mcr.cv.raw[i]<- mean(pred!=Species[i]) 
}
print(mcr.cv.raw)
mcr.cv<-mean(mcr.cv.raw)
cat("cross-validation mcr for logistic regression:", mcr.cv, "\n\n")