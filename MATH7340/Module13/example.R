library("ALL"); data(ALL);
allB123 <- ALL[,which(ALL$BT %in% c("B1","B2","B3"))]
prob.name <- "1389_at"
expr.data <-exprs(allB123)[prob.name,]
IsB1 <- (allB123$BT=="B1")
data.lgr <- data.frame(IsB1, expr.data)
fit.lgr <- glm(IsB1~., family=binomial(link='logit'), data=data.lgr)
pred.prob <- predict(fit.lgr, data=data.lgr$expr.data, type="response")
pred.B1<- factor(pred.prob> 0.5, levels=c(TRUE,FALSE), labels=c("B1","not"))
IsB1<-factor(IsB1, levels=c(TRUE,FALSE), labels=c("B1","not"))
table(pred.B1, IsB1)

library(ROCR) 
pred <- prediction(pred.prob, IsB1=="B1")
perf <- performance(pred, "tpr", "fpr" )
plot(perf)

performance(pred, "auc")

library("ALL"); data(ALL); 
allB123 <- ALL[ , which(ALL$BT %in% c("B1","B2","B3"))] 
expr.data <-exprs(allB123)[ "1389_at", ] 
IsB1 <- (allB123$BT=="B1") 
data.lgr <- data.frame( IsB1,expr.data) 
set.seed(131)  
testID <- sample(1:78, 31, replace = FALSE)
data.tr<-data.lgr[-testID, ]
data.test<-data.lgr[testID, ]
fit.lgr <- glm(IsB1~., family=binomial(link='logit'), data=data.tr)