newcomp3ordered2<-function(param) 
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
        res <- svd(y)
        a <- res$u %*% t(res$v)
  #  a <- a %*% t(res$v) #dim I,p
#a<-a
#browser()
    list(a = as.matrix(a), b = as.matrix(b), cc = as.matrix(cc), 
        g = g, xs = x,pii=pii,pj=pj,pk=pk)
}
