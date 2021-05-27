#Regneark
x <- c(1,2,3)
y <- c(1,5)
n <- 0.01
W1 <- matrix(c(0,0,0,.8,.6,.4,.8,.6,.4,.8,.6,.4), 4,3,T)
W2 <- matrix(c(0,0,0,0,3.98,1.88,1.86,1.84),2,4,T)


z1 <- W1 %*% x
a1 <- z1 + c(1,0,0,0)

z2 <- W2 %*% a1
a2 <- z2 + c(1,0) 

d2 <- matrix(c(0,0,0,1),2,2,T) %*% (a2-y)
NabW2 <- d2 %*% t(a1)

d1 <- (diag(4)-matrix(c(1,rep(0,15)),4,4,T)) %*% t(W2) %*%d2
NabW1 <- d1 %*% x

W1 <- W1 - n*NabW1;W1
W2 <- W2 - n*NabW2;W2

dist(rbind(t(a2),y));a2

