#Convolution
#https://stackoverflow.com/questions/23569133/adding-two-random-variables-via-convolution-in-r

f.X <- function(x) dnorm(x,1,0.5)        # normal (mu=1.5, sigma=0.5)
f.Y <- function(y) dlnorm(y,1.5, 0.75)   # log-normal (mu=1.5, sigma=0.75)
# convolution integral
f.Z <- function(z) integrate(function(x,z) f.Y(z-x)*f.X(x),-Inf,Inf,z)$value
f.Z <- Vectorize(f.Z)                    # need to vectorize the resulting fn.

set.seed(1)                              # for reproducible example
X <- rnorm(1000,1,0.5)
Y <- rlnorm(1000,1.5,0.75)
Z <- X + Y
# compare the methods
hist(Z,freq=F,breaks=50, xlim=c(0,30))
z <- seq(0,50,0.01)
lines(z,f.Z(z),lty=2,col="red")


#----
library(distr)
N <- Norm(mean=1, sd=0.5)          # N is signature for normal dist
L <- Lnorm(meanlog=1.5,sdlog=0.75) # same for log-normal
conv <- convpow(L+N,1)             # object of class AbscontDistribution
f.Z  <- d(conv)                    # distribution function

hist(Z,freq=F,breaks=50, xlim=c(0,30))
z <- seq(0,50,0.01)
lines(z,f.Z(z),lty=2,col="red")


#----------
x <- c(0,0,0,100,0,0,0)
y <- c(0,0,1, 2 ,1,0,0)/4
zapsmall(convolve(x, y))         #  *NOT* what you first thought.
zapsmall(convolve(x, y[3:5], type = "f")) # rather
x <- rnorm(50)
y <- rnorm(50)
# Circular convolution *has* this symmetry:
all.equal(convolve(x, y, conj = FALSE), rev(convolve(rev(y),x)))

n <- length(x <- -20:24)
y <- (x-10)^2/1000 + rnorm(x)/8

Han <- function(y) # Hanning
  convolve(y, c(1,2,1)/4, type = "filter")

plot(x, y, main = "Using  convolve(.) for Hanning filters")
lines(x[-c(1  , n)      ], Han(y), col = "red")
lines(x[-c(1:2, (n-1):n)], Han(Han(y)), lwd = 2, col = "dark blue")
# }