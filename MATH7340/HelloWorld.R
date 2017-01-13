# TODO: Add comment
# 
# Author: lenovo
###############################################################################

integrand <- function(x) sqrt(x)
X <- c()
for (i in 1:20){
	X[i] <- 2*i
}
print(X)
Y <- rep(0, 20)
print(Y)

for (k in 1:20){
	if (k < 12){
		Y[k] = cos(3*k)	
	} 
	else {
		Y[k] = integrate(integrand, lower = 0, upper = k)$value
	}
}
print(Y)

