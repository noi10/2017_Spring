##########################################################
# Generalized additive models (GAMs)
##########################################################
# Inspired from https://m-clark.github.io/docs/GAM.html 

# Read data: SA heart desease
setwd('/Users/ovitek/Dropbox/Olga/Teaching/CS6140/Spring17/LectureNotes/6_kernels')
X <- read.table('SAheart.txt', sep=",", header=TRUE)[,-c(1,6)]


# Exploratory data analysis
#----------------------------
pairs(X[,-9])

# One-predictor model
library(mgcv)
mod_glm1 <- gam(chd ~ age, family="binomial", data=X)
summary(mod_glm1)

mod_gam1 <- gam(chd ~ s(age, bs="cr"), family="binomial", data=X)
summary(mod_gam1)
plot(mod_gam1)

c(AIC(mod_glm1), AIC(mod_gam1))


# Two-predictor model
#----------------------------
mod_glm2 <- gam(chd ~ age+obesity, family="binomial", data=X)
summary(mod_glm2)

mod_gam2 <- gam(chd ~ s(age, bs="cr")+s(obesity, bs="cr"), family="binomial", data=X)
summary(mod_gam2)
plot(mod_gam2, pages=1)

c(AIC(mod_glm2), AIC(mod_gam2))

vis.gam(mod_glm2, type='response', plot.type='contour')
vis.gam(mod_gam2, type='response', plot.type='contour')
vis.gam(mod_gam2, type='response', plot.type='perspective')

library(visreg)
visreg2d(mod_gam2, xvar='age', yvar='obesity', scale='response')

concurvity(mod_gam2)


# Smooth of the one-variable smoothes
#-------------------------------------------
mod_gam3 <- gam(chd ~ te(age, obesity), family="binomial", data=X)
summary(mod_gam3)
c(AIC(mod_glm2), AIC(mod_gam2), AIC(mod_gam3)) # does not fit as well

visreg2d(mod_gam3, xvar='age', yvar='obesity', scale='response')
vis.gam(mod_gam3, type='response', plot.type='perspective')

concurvity(mod_gam3)

