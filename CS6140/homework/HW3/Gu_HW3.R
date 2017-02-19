library(glmulti)
library(glmpath)
library(pastecs)
library(knitr)

Mydata <- read.table("SouthAfricanHeartDisease.txt", sep=",", stringsAsFactors = FALSE, header = TRUE)
Mydata <- Mydata[,-1]
Mydata[,5][Mydata[,5]=="Present"] <- 1
Mydata[,5][Mydata[,5]=="Absent"] <- 0

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
#          confsetsize = 3,         # Keep 3 best models
#          plotty = F, report = F,  # No plot or interim reports
#          fitfunction = "glm",     # glm function
#          family = binomial)       # binomial family for logistic regression

#glmulti.logistic.out@formulas
#summary(glmulti.logistic.out@objects[[1]])

fit.cv.glmpath <- cv.glmpath(x=as.matrix(predictors[train,]),
                             y=response[train], 
                             family=binomial, nfold=10, plot.it=T)

fit.glmpath <- glmpath(x=as.matrix(predictors[train,]),
                       y=response[train], family=binomial)