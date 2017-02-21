library(glmulti)
library(glmpath)
library(pastecs)
library(knitr)
library(ROCR)

setwd("C:/Users/lenovo/Desktop/2017_Spring/CS6140/homework/HW3")
#setwd("C:/Users/Bobo/Desktop/2017_Spring/CS6140/homework/HW3")
Mydata <- read.table("SouthAfricanHeartDisease.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)
Mydata <- Mydata[,-1]
Mydata[,5][Mydata[,5]=="Present"] <- 2
Mydata[,5][Mydata[,5]=="Absent"] <- 1

#Mydata[,5] <- factor(Mydata[,5])
Mydata[,5] <- as.integer(Mydata[,5])
predictors <- Mydata[,1:9]
response <- Mydata[,10]

sum(is.na(Mydata))

train <- sample(x=1:nrow(Mydata), size=nrow(Mydata)/2)
trainSet <- Mydata[train,]

one.variable.summary <- summary(trainSet, digits=2)
one.variable.summary

format(round(stat.desc(trainSet, basic=F), 2), nsmall = 2)
format(round(stat.desc(trainSet, desc=F), 2), nsmall=2)

round(cor(trainSet), digits=2)
pairs(trainSet)

#glmulti.logistic.out <-
#  glmulti(chd ~ ., data = trainSet,
#          level = 1,               # Interaction considered
#          method = "h",            # Exhaustive approach
#          crit = "aic",            # AIC as criteria
#          confsetsize = 10,         # Keep 3 best models
#          plotty = T, report = T,  # No plot or interim reports
#          fitfunction = "glm",     # glm function
#          family = binomial)       # binomial family for logistic regression

# all subset
glmulti.logistic.out <- glmulti(chd~. , data=trainSet, level=1, fitfunction="glm", crit = "aic", confsetsize=512)
plot(glmulti.logistic.out)
print(glmulti.logistic.out)
summary(glmulti.logistic.out@objects[[1]])
#glmulti.summary <- summary(glmulti.logistic.out)
#glmulti.logistic.out@formulas
#summary(glmulti.logistic.out@objects[[1]])


# regularization
fit.cv.glmpath <- cv.glmpath(x=as.matrix(predictors[train,]),
                             y=response[train], 
                             family=binomial, nfold=10, plot.it=T)

# cross-validated prediction on the training set
fit.cv.glmpath1 <- cv.glmpath(x=as.matrix(predictors[train,]),
                              y=response[train], 
                              family=binomial, nfold=10, plot.it=T , type="response")

cv.s <- fit.cv.glmpath$fraction[which.min(fit.cv.glmpath$cv.error)]

fit.glmpath <- glmpath(x=as.matrix(predictors[train,]),
                       y=response[train], family=binomial)

predict(fit.glmpath, s=cv.s, mode="norm.fraction", type="coefficients")