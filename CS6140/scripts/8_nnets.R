setwd("/Users/ovitek/Dropbox/Olga/Teaching/CS6140/Spring17/LectureNotes/5_generative")
library(Biobase)


########################################################################
#                   Example of generative classifiers
#######################################################################
library(faraway)
data(pima)
?pima
head(pima)


################################################################
# Separation of the training and the validation datasets 
################################################################
# select training set (as example, here only use 1/4 of the data to build the model)
# set seed for reproducible result of random sampling
set.seed(123)
train <- sample(x=1:nrow(pima), size=nrow(pima)/4)
pimaTrain <- pima[train,]
pimaValid <- pima[-train,]

################################################################
#                 				Neural networks
################################################################
# Fit neural networks
library(neuralnet)
fit.nn <- neuralnet(test ~ bmi+diabetes+age, data=pimaTrain, err.fct='ce', linear.output=FALSE)


# Visualize neural networks
plot(fit.nn)
# (Error: sum of squared errors; steps: number of iterations)

# Add hidden nodes
fit.nn1 <- neuralnet(test ~ bmi+diabetes+age, hidden = 2, data=pimaTrain, err.fct='ce', linear.output=FALSE)
plot(fit.nn1)

# Evaluate predictive performance
p <- compute(fit.nn, pimaValid[,c('bmi', 'diabetes','age')])



