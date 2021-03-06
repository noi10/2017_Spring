#######################################################################
#######################################################################
#                   L O G I S T I C   R E G R E S S I O N
#                               GLMpath
#######################################################################
#######################################################################

library(faraway)
data(pima)
head(pima)
set.seed(123)

#----------------Separate predictors from the response------------------
predictors <- pima[,1:8]
response <- pima[,9]

#---Partition the dataset into the training and the validation dataset---
train <- sample(x=1:nrow(pima), size=nrow(pima)/4)

################################################################
# Stepwise variable selection
################################################################

# ---------------------Fit the full model-----------------------------
fit.train <- glm(test ~., family=binomial, data=pima[train,])
summary(fit.train)

# --------------------Stepwise variable selection---------------------

# stepwise variable selection based on AIC
step.train <- step(fit.train, k=2, trace=F) 
step.train$anova

step.fit.train <- glm(test ~ pregnant+glucose+insulin+bmi, family=binomial, data=pima[train,])
summary(step.fit.train)



################################################################
# Variable selection with glmpath 
################################################################
library(glmpath)

# selection of the parameter lambda
fit.cv.glmpath <- cv.glmpath(x=as.matrix(predictors[train,]),
                  y=response[train], 
                  family=binomial, nfold=10, plot.it=T)

# cross-validated prediction on the training set
fit.cv.glmpath1 <- cv.glmpath(x=as.matrix(predictors[train,]),
                  y=response[train], 
                  family=binomial, nfold=10, plot.it=T, type="response")


# find parameter value that minimizes cross-validated error
cv.s <- fit.cv.glmpath$fraction[which.min(fit.cv.glmpath$cv.error)]

# refit the model without cross-validation
fit.glmpath <- glmpath(x=as.matrix(predictors[train,]),
                  y=response[train], family=binomial)

# plot the path
par(mfrow=c(1,1), mar=c(4,4,4,8))
plot(fit.glmpath, xvar="lambda")

# in-sample predictive accuracy
pred.coef <- predict(fit.glmpath, s=cv.s, mode="norm.fraction", type="coefficients")
pred.glmpath.train <- predict(fit.glmpath, newx=as.matrix(predictors[train,]), s=cv.s, 
			mode="norm.fraction", type="response")
table(true=response[train], predicted=pred.glmpath.train > 0.5)

# predictive accuracy on a validation set
pred.coef <- predict(fit.glmpath, s=cv.s, mode="norm.fraction", type="coefficients")
pred.glmpath.valid <- predict(fit.glmpath, newx=as.matrix(predictors[-train,]), s=cv.s, 
			mode="norm.fraction", type="response")
table(true=response[-train], predicted=pred.glmpath.valid > 0.5)



################################################################
# Produce in-sample, cross-validation and validation set ROC 
#                   curve for logistic regression
################################################################

# ----------ROC curve using glmpath----------------

# ROC on the training set
library(ROCR)
pred <- prediction( predictions=pred.glmpath.train, labels=response[train] )
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, main="glmpath")
# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


# ROC on the validation set
pred <- prediction( predictions=pred.glmpath.valid, labels=response[-train] )
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, add=TRUE)
# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


# ----------ROC curve using stepwise variable selection----------------

# ROC on the training set
scores <- predict(step.fit.train, newdata=predictors[train,], type="response")
pred <- prediction( scores, labels=response[train] )
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, main=" ", add=TRUE)
# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)

# ROC on the validation set
scores <- predict(step.fit.train, newdata=predictors[-train,], type="response")
pred <- prediction( scores, labels=response[-train] )
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, add=TRUE)
# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)

