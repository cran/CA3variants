 step.g3ordered<-function(param)
{
#-----------------------------------------------------------
# Comput the core g given the 3-way matrix x, and the matrices A,B and C
#      x   of dimensions I x J x K
#      A   of dimensions I x p
#      B   of dimensions J x q
#      C   of dimensions K x r
#      g   of dimensions p x q x r
#-----------------------------------------------------------------------
Z <- param$Z
a <- param$a
b <- param$b
cc <- param$cc
pii<-param$pii
pj<-param$pj
pk<-param$pk
p <- ncol(a)
q <- ncol(b)
r <- ncol(cc)
x <- param$x
x <- flatten(x)# dim(x) is I x (JxK)
#cat("sono dentro spep.g3\n")#print(dim(a))
#print(dim(x))
ax <- t(a) %*% x# dim(ax) is p x (JxK)
#bc <- b %k% cc#bc de dimensions (J x  K) x (q x r)
bc<-Kron(b,cc)
#print(bc)
g <- ax %*% bc# g de dimensions c(p,(q x r))
 Z <- t(a) %*%diag(sqrt(pii))%*% x%*%Kron(diag(sqrt(pj)),diag(sqrt(pk))) %*% bc
dim(g) <- c(p, q, r)
dim(Z) <- c(p, q, r)
param$g <- g
param$Z <- Z #includes the metrics
#browser()
param
}
