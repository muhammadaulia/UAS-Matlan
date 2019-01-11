
library(matlib)

#1
x <- c(50,51,52,53,54)
y <- c(40,46,44,55,49)

linear <- lm(y~x)

summary(linear)

#2
x <- c(50,51,52,53,54)
y <- c(40,46,44,55,49)
linear <- lm(y~x)

predict(linear,data.frame(x=55))


library(polynom)

#3
x <- c(0,1,2,3,4)
y <- c(1,2.25,3.75,4.25,5.65)

poly.calc(x,y)



#4
x <- c(0,1,2,3,4)
y <- c(1,2.25,3.75,4.25,5.65)

P<-poly.calc(x,y)

predict(P,data.frame(x=2.75))


#5
x <- c(0,1,2,3,4)
y <- c(1,2.25,3.75,4.25,5.65)

P<-poly.calc(x,y)

f1 <- function(x) {
    return(1 - 0.07916667*x + 2.19375*x^2 - 0.9958333*x^3 + 0.13125*x^4 )
}
f1(2.75)
plot(x,y)
curve(f1,add=TRUE)

#11
library(pracma)
library(Ryacas)
x <- Sym("x")
f <- function(x) {
  return((x^2) - 6)
}


trapzfun(f, 0, 1)

#12
library(pracma)
library(Ryacas)
x <- Sym("x")
f <- function(x) {
  return((x^3)+(4*(x^2))-10)
}

trapzfun(f,1,2)

#13,14
h <- 0.1
x <- seq(0,1, by=h)
f <- function(x){
    return(x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:10],f)
fn <- f(x[length(x)])

trap <- function(f0,fi,fn,h) {
    L <- h * (f0 + 2 * sum(fi) + fn) /2 
    return(L)
}
trap(f0,fi,fn,h)

#15
h <- 0.2
x <- seq(0,1, by=h)
f <- function(x){
    return(x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:5],f)
fn <- f(x[length(x)])

trap <- function(f0,fi,fn,h) {
    L <- h * (f0 + 2 * sum(fi) + fn) /2 
    return(L)
}
trap(f0,fi,fn,h)
