newcomp3ordered<-function(param) 
{
n <- dim(param$x)
      a <- param$a
    b <- param$b
    cc <- param$cc
pii<-param$pii
pj<-param$pj
pk<-param$pk
    g <- param$g
    pqr <- dim(g)
    p <- pqr[1]
    q <- pqr[2]
    r <- pqr[3]
qr<-q*r
    x <- param$x
    f <- Kron(b[,1:q], cc[,1:r]) #dimension JK x qr
    gf <- flatten(g) #dimension p x qr
    fg <- f %*% t(gf) #dimension JK x p
    xf <- flatten(x) #dimension Ix JK
    y <- xf %*% fg #dimension Ixp
########
    p <- min(p, n[1])
    mj <- c(1:n[1])
#mj<-c(param$a[,2])
   
Apoly <- emerson.poly(mj, pii)
    Apoly <- Apoly[,-c(1) ]
  #  cat("Apoly \n")
  # print(Apoly)
    a <- diag(sqrt(pii)) %*% Apoly
#    a <-  Apoly[, 1:p]
 
  # cat("Checking the orthonormality of polynomials a:\n")
   # print(t(a) %*% (a))


####
 #  browser()  
    res <- svd(y)
        a <- res$u %*% t(res$v)
  #  a <- a %*% t(res$v) #dim I,p
#a<-a
#browser()
    list(a = as.matrix(a), b = as.matrix(b), cc = as.matrix(cc), 
        g = g, xs = x,pii=pii,pj=pj,pk=pk)
}
