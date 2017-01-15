# problem 1(a)
rm(list=ls())
vec <- c(5,TRUE)
print(class(vec))


# problem 1(b)
rm(list=ls())
x <- 1:4
y <- 1:2
print(x+y)


# problem 1(c)
rm(list=ls())
fsin <- function(x) sin(pi*x)
print(fsin(1))


# problem 1(d)
rm(list=ls())
print(c(1,2)%*%t(c(1,2)))


# problem 1(e)
rm(list=ls())
f <- function(x){
	g <- function(y){
		y+z
	}
	z <- 4
	x + g(x)
}
z <- 15
print(f(3))


# problem 2
rm(list=ls())
sum <- 0
for (i in 1:1000) {
	sum <- sum + i^2
}
print(sum)


# problem 3(a)
rm(list=ls())
X <- c()
for (i in 1:20){
	X[i] <- 2*i
}
print(X)

# problem 3(b)
rm(list=ls())
Y <- rep(0, 20)
print(Y)

# problem 3(c)
integrand <- function(x) sqrt(x)
for (k in 1:20){
	if (k < 12){
		Y[k] = cos(3*k)	
	} 
	else {
		Y[k] = integrate(integrand, lower = 0, upper = k)$value
	}
}
print(Y)

