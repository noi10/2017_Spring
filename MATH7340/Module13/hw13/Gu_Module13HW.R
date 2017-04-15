# problem 1
library(ALL)
library(ROCR)
library(e1071)
require(rpart)
require(caret)

# (a)
data(ALL)
IsB <- factor(ALL$BT %in% c("B", "B1", "B2", "B3", "B4"))

# (b)
probedat <- as.matrix(exprs(ALL[c("39317_at", "38018_g_at"),]))
c.tr <- rpart(IsB ~ ., data = data.frame(t(probedat)))
#plot(c.tr, branch=0,margin=0.1)
#text(c.tr, digits=3)
rpartpred <- predict(c.tr, type="class")
table(rpartpred, IsB)

pred.prob <- predict(c.tr, type="prob")[,2]
pred <- prediction(pred.prob, IsB == "TRUE")
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# (c)
# mcr
mcr <- sum(rpartpred != IsB)/length(IsB)
mcr

# fnr
fnr <- sum(rpartpred == "FALSE" & IsB == "TRUE")/sum(IsB == "TRUE")
fnr

# specificity
fpr <- sum(rpartpred == "TRUE" & IsB == "FALSE")/sum(IsB == "FALSE")
spec <- 1-fpr
spec

performance(pred, "auc")@y.values[[1]]

# (d)
data <- data.frame(IsB, t(probedat))
n <- dim(probedat)[2]
index <- 1:n
K <- 10
flds <- createFolds(index, k=K)
fnr.cv.raw <- rep(NA, K)
for (i in 1:K){
  testID <- flds[[i]]
  data.tr <- data[-testID,]
  data.test <- data[testID,]
  tree.cv <- rpart(IsB ~ ., data = data.tr)
  tree.cv.pred <- predict(tree.cv, newdata = data.test, type = "c")
  fnr.cv.raw[i] <- sum(tree.cv.pred == "FALSE" & data.test$IsB == "TRUE")/sum(data.test$IsB == "TRUE")
}
fnr.cv <- mean(fnr.cv.raw)
fnr.cv

# (e)
fit.lgr <- glm(IsB~. , family=binomial(link='logit'), data = data)
summary(fit.lgr)

confint(fit.lgr, level=0.8)

# (f)
n<-dim(data)[1]
lgr.mcr.cv.raw <- rep(NA, n)
for (i in 1:n) {
  data.tr <- data[-i,]
  data.test <- data[i,]
  fit.lgr <- glm(IsB~., family = binomial(link='logit'), data = data.tr)
  pred.prob <- predict(fit.lgr, newdata = data.test, type ="response")
  pred.B <- (pred.prob > 0.5)
  lgr.mcr.cv.raw[i] <- pred.B!=data.test$IsB
}

lgr.mcr.cv <- mean(lgr.mcr.cv.raw)
lgr.mcr.cv

# (g)
PCA<-prcomp(t(exprs(ALL)), scale=TRUE)
#summary(PCA)
importance <- summary(PCA)$importance[2,]
K = (1:128)
plot(K, importance, type='o', xaxt='n')
axis(1, at = K, las=2, cex.axis=0.65)
abline(v=8, col="red")
abline(v=12, col="red")

# (h)
data.pca <- PCA$x[,1:5]
svm <- svm(data.pca, IsB, type = "C-classification", kernel = "linear")
svm.pred <- predict(svm , data.pca)

# tpr
sum(svm.pred == "TRUE" & IsB == "TRUE")/sum(IsB == "TRUE")

# (i)
n <- dim(data.pca)[1]
svm.mcr.cv.raw <- rep(NA, n)

for (i in 1:n){
  svm.cv <- svm(data.pca[-i,], IsB[-i], type = "C-classification", kernel="linear")
  svm.cv.pred <- predict(svm.cv, t(data.pca[i,]))
  svm.mcr.cv.raw[i]<-svm.cv.pred!=IsB[i]
}
svm.mcr.cv <- mean(svm.mcr.cv.raw)
svm.mcr.cv

# (j)
# SVM, lower cross-validation misclassification rate.


# problem 2
rm(list=ls())
library(VGAM)
data(iris)

lgr.classification <- function(iris2) {
  iris2.lgr <- vglm(Species~. , family = multinomial, data = iris2)
  pred.prob <- predict(iris2.lgr, as.data.frame(iris2[,-1]), type = "response")
  pred.lgr <- apply(pred.prob, 1, which.max)
  pred.lgr <- factor(pred.lgr, levels=c("1", "2", "3"), labels = levels(iris2$Species))
  
  mcr.lgr <- mean(pred.lgr!=iris2$Species)
  cat("empirical mcr for logistic regression:", mcr.lgr, "\n")
  
  mcr.cv.raw<-rep(NA, n) 
  for (i in 1:n) {
    lgr.fit <- vglm(Species~., family=multinomial, data=iris2[-i,])
    pred.prob <- predict(lgr.fit, iris2[i,], type="response")
    pred <- apply(pred.prob, 1, which.max) 
    pred <- factor(pred, levels=c("1","2","3"), labels=levels(iris2$Species)) 
    mcr.cv.raw[i]<- mean(pred!=Species[i]) 
  }
  mcr.cv<-mean(mcr.cv.raw)
  cat("cross-validation mcr for logistic regression:", mcr.cv, "\n\n")
}

svm.classification <- function(iris2) {
  iris2.svm <- svm(Species~., type = "C-classification", kernel = "linear", data = iris2) 
  svmpred <- predict(iris2.svm , data.pca)  
  mcr.svm<- mean(svmpred!=Species) 
  cat("empirical mcr for SVM:", mcr.svm, "\n")
  mcr.cv.raw<-rep(NA, n) 
  for (i in 1:n) { 
    svmest <- svm(Species~., type = "C-classification", kernel = "linear", data = iris2[-i,])
    svmpred <- predict(svmest, iris2[i,]) 
    mcr.cv.raw[i]<- mean(svmpred!=iris2$Species[i]) 
  }
  mcr.cv<-mean(mcr.cv.raw)
  cat("cross-validation mcr for SVM:", mcr.cv, "\n\n")
}

tree.classification <- function(iris2) {
  fit <- rpart(Species ~ ., data = iris2, method = "class")
  pred.tr<-predict(fit, iris2, type = "class")
  mcr.tr <- mean(pred.tr!=Species)
  cat("empirical mcr for classification tree :", mcr.tr, "\n")
  
  mcr.cv.raw<-rep(NA, n)
  for (i in 1:n) { 
    fit.tr <- rpart(Species ~ ., data = iris2[-i,], method = "class")
    pred <- predict(fit.tr, iris2[i,], type = "class")
    mcr.cv.raw[i]<- mean(pred!=Species[i])
  }
  mcr.cv<-mean(mcr.cv.raw)
  cat("cross-validation mcr for classification tree:", mcr.cv, "\n\n\n")
}

pca.iris<-prcomp(iris[,1:4], scale=TRUE)
Species <- iris$Species
n <- length(Species)
for (k in 1:4){
  cat("K = ", k, "\n")
  data.pca <- as.matrix(pca.iris$x[, 1:k])
  iris2 <- data.frame(Species, data.pca)
  lgr.classification(iris2)
  svm.classification(iris2)
  tree.classification(iris2)
}