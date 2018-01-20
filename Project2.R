#Name: Project 2
#Authoer: Juan P. Garces
#Date: 09/20/2016

#1. Functions
GenUnifSamples <- function(S, N=10000){
  set.seed(100)
  means <- replicate(N, mean(runif(S, 1, 10)))
  return(means)
}
GenNormSamples <- function(S, N=10000){
  set.seed(100)
  means <- replicate(N, mean(rnorm(S)))
  return(means)
  
}
GenExpSamples <- function(S, N=10000){
  set.seed(100)
  means <- replicate(N, mean(rexp(S, rate = 1)))
  return(means)
}

#2a. THeoreticals
set.seed(100)
x <- seq(-3,3, length=500)
xx <- seq(.9, 10.1, length = 500)
plot(xx, dunif(xx, min = 1, max= 10) , type= "l", ylab = "Density", xlab = "Numbers", main = "Theoretical Uniform Distribution")
plot(x, dnorm(x) ,type= "l",ylab = "Density", xlab = "Numbers", main = "Theoretical Standard Normal Distribution")
plot(x, dexp(x), type= "l",ylab = "Density", xlab = "Numbers", main = "Exponential Distribution")

#2b. One Sample with 10,000 values
#Uniform Distribution ~~ Sample 1
vu <-GenUnifSamples(S = 1)
hist(vu, xlab = "Numbers in Sample", main = "Uniform Distribution Sample of 10,000")
mean(vu)
sd(vu)
fivenum(vu)
#Standard Normal Distribution ~~ Sample 1
vn <-GenNormSamples(S = 1)
hist(vn, xlab = "Numbers in Sample", main = "Standard Normal Sample of 10,000")
mean(vn)
sd(vn)
fivenum(vn)
#Exponential Distribution ~~ Sample 1
ve <-GenExpSamples(S = 1)
hist(ve, xlab = "Numbers in Sample", main = "Exponential Distribution Sample of 10,000")
mean(ve)
sd(ve)
fivenum(ve)

#2c.
#Uniform Distribution ~~ Sample 2 Size 5
vu <-GenUnifSamples(S = 5)
hist(vu, xlab = "Numbers in Sample", main = "Uniform Distribution Sample of 10,000 Size 5")
mean(vu)
sd(vu)
fivenum(vu)
#Standard Normal Distribution ~~ Sample 2 Size 5
vn <-GenNormSamples(S = 5)
hist(vn, xlab = "Numbers in Sample", main = "Standard Normal Sample of 10,000 Size 5")
mean(vn)
sd(vn)
fivenum(vn)
#Exponential Distribution ~~ Sample 2 Size 5
ve <-GenExpSamples(S = 5)
hist(ve, xlab = "Numbers in Sample", main = "Exponential Distribution Sample of 10,000 Size 5")
mean(ve)
sd(ve)
fivenum(ve)

#Uniform Distribution ~~ Sample 3 Size 25
vu <-GenUnifSamples(S = 25)
hist(vu, xlab = "Numbers in Sample", main = "Uniform Distribution Sample of 10,000 Size 25")
mean(vu)
sd(vu)
fivenum(vu)
#Standard Normal Distribution ~~ Sample 3 Size 25
vn <-GenNormSamples(S = 25)
hist(vn, xlab = "Numbers in Sample", main = "Standard Normal Sample of 10,000 Size 25")
mean(vn)
sd(vn)
fivenum(vn)
#Exponential Distribution ~~ Sample 3 Size 25
ve <-GenExpSamples(S = 25)
hist(ve, xlab = "Numbers in Sample", main = "Exponential Distribution Sample of 10,000 Size 25")
mean(ve)
sd(ve)
fivenum(ve)

#Uniform Distribution ~~ Sample 4 Size 500
vu <-GenUnifSamples(S = 500)
hist(vu, xlab = "Numbers in Sample", main = "Uniform Distribution Sample of 10,000 Size 500")
mean(vu)
sd(vu)
fivenum(vu)
#Standard Normal Distribution ~~ Sample 4 Size 500
vn <-GenNormSamples(S = 500)
hist(vn, xlab = "Numbers in Sample", main = "Standard Normal Sample of 10,000 Size 500")
mean(vn)
sd(vn)
fivenum(vn)
#Exponential Distribution ~~ Sample 4 Size 500
ve <-GenExpSamples(S = 500)
hist(ve, xlab = "Numbers in Sample", main = "Exponential Distribution Sample of 10,000 Size 500")
mean(ve)
sd(ve)
fivenum(ve)
