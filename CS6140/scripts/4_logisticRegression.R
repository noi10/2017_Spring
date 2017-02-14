#######################################################################
#######################################################################
#                 L O G I S T I C   R E G R E S S I O N
#######################################################################
#######################################################################


#######################################################################
#                       Example of Bernouilli distribution 
#                         (data recordered per subect)
#######################################################################
setwd("/Users/ovitek/Dropbox/Olga/Teaching/CS6140/Spring17/LectureNotes/4_logisticRegression")
X <- read.table("smokingAndObesity.txt", sep=" ", as.is=TRUE, header=TRUE)
X <- X[order(X$age),]

# factor for 'smoking status'
X$smokeF <- factor(X$smoke)
head(X)


# create a proper binary response for 'overweight'
table(X$over_wt)
X$over_wtF <- factor(abs(X$over_wt - 2), levels=c(0,1))
table(X$over_wtF)
head(X)

# smoking as predictor
fit<- glm(over_wtF ~ smokeF, family=binomial, data=X) 
summary(fit)


# add age
fit<- glm(over_wtF ~ age + smokeF + age*smokeF, family=binomial, data=X) 
summary(fit)




#######################################################################
#                  Example of Binomial distribution 
#         (data recorded as the # of successes per covariate pattern)
#######################################################################
library(faraway)
data(orings)
?orings
orings

# ---------------------Explore graphically-----------------------------
plot(damage/6 ~ temp, orings, xlim=c(25,85),ylim=c(0,1),
    xlab="Temperature",ylab="Proportion of damage", pch=16)

# ----------Fit (specify 2 responses: # of 1s and # of 0s)-------------
fit <- glm(cbind(damage, 6-damage) ~ temp, family=binomial, data=orings)
summary(fit)

# ----------------------------Prediction--------------------------------
newOrings <- data.frame(temp=seq(from=10, to=100, length=10))
newOrings.predict <- predict(fit, newdata=newOrings, se.fit=T, type="response")
lines(newOrings$temp, newOrings.predict$fit)

# -------------------------Alternative link function---------------------------
plot(damage/6 ~ temp, orings, xlim=c(25,85),ylim=c(0,1),
    xlab="Temperature",ylab="Proportion of damage", pch=16)

# Same as before: redo precistion with logit link
newOrings <- data.frame(temp=seq(from=10, to=100, length=10))
newOrings.predict.logistic <- predict(fit, newdata=newOrings, se.fit=T, type="response")
lines(newOrings$temp, newOrings.predict.logistic$fit, col="blue")

# Compare with probit link
fit2 <- glm(cbind(damage,6-damage) ~ temp, family=binomial(link="probit"), data=orings)
newOrings.predict.probit <- predict(fit2, newdata=newOrings, se.fit=T, type="response")
lines(newOrings$temp, newOrings.predict.probit$fit, col="red")

legend("topright", lty=1, col=c("blue", "red"), c("logit", "probit"))




#######################################################################
#                   Example of variable selection
#######################################################################
library(faraway)
data(pima)
?pima
head(pima)

# ---------------------Fit the full model-----------------------------
fit <- glm(test ~., family=binomial, data=pima)
summary(fit)


# -------------------------Compare nested models-------------------------
summary(fit)

# stepwise variable selection based on AIC 
# ('k' distinguishes AIC and BIC)
step.aic <- step(fit, k=2, trace=F) 
step.aic$anova

# stepwise variable selection based on BIC
step.bic <- step(fit, k=log(nrow(pima)), trace=F) 
step.bic$anova


#######################################################################
#                  ROC curves
#######################################################################
# --------------Predictive ability on the training set-------------------------
library(ROCR)
# select training set (as example, here only use 1/4 of the data to build the model)
train <- sample(x=1:nrow(pima), size=nrow(pima)/4)

# fit the full model on the training dataset
fit.train <- glm(test ~., family=binomial, data=pima[train,])
summary(fit.train)
summary(fit)

# calculate predicted probabilities on the same training set
scores <- predict(fit.train, newdata=pima[train,], type="response")

# compare predicted probabilities to labels, for varying probability cutoffs
pred <- prediction(scores, labels=pima[train,]$test )
perf <- performance(pred, "tpr", "fpr")

# plot the ROC curve
plot(perf, colorize=F, main="In-sample ROC curve")

# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


# --------------Evalate the predictive ability on the validation set------------
# make prediction on the validation dataset
scores <- predict(fit.train, newdata=pima[-train,], type="response")
pred <- prediction( scores, labels=pima[-train,]$test )
perf <- performance(pred, "tpr", "fpr")

# overlay the line for the ROC curve
plot(perf, colorize=T, add=TRUE)

# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)

