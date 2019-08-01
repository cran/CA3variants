newcomp3<-function(param)
{
#----------------------
# estimation of the third component matrix given the two others b and c
# and the non-diagonal core g
# --------------------------------provisoire
#--------------------------------------------ind (externe)
#                  a = x f (f'x'x f)**-.5
#-------------------------------------------------------
a <- param$a
b <- param$b
cc <- param$cc
g <- param$g
pqr <- dim(g)
p <- pqr[1]
q <- pqr[2]
r <- pqr[3]
x <- param$x
f <- Kron(b, cc)#f de dimensions (JxK) x (qxr)
gf <- flatten(g)#g de dimensions p x (qxr)
fg <- f %*% t(gf)#fg de dimensions (JxK) x p
xf <- flatten(x)#xf de dimensions I x (JxK)
y <- xf %*% fg# y de dimensions I x p
#if(ind) {
#s <- t(y) %*% y
#w <- svd(s)
#d <- 1/sqrt(w$d)
#u <- as.matrix(w$u)
#a <- y %*% u %*% diag(d, p, p) %*% t(u) #rotation not needed!!
#a <- y %*% u %*% diag(d, p, p) 
#}
#else {
#browser()
res <- svd(y)
#a <- res$u %*% t(res$v) #it implies a rotation not needed!!
a <- res$u  
#}
#browser()
list(a = as.matrix(a), b = as.matrix(b), cc = as.matrix(cc), g = g, x = x)
}
