init3ordered1<-function(x, p, q, r, x0)
{
#-------------------------------------------------
# Initialisation of TUCKER3 by 
# Triple PCA for a three-way table on each
# way :
#    dim(x) is IxJxK
#    a      is   Ixp
#    b      is   Jxq
#    c      is   Kxr
# polinomi ortogonali calcolati solo sul secondo modo
#-------------------------------------------------
	nom <- dimnames(x)
	n <- dim(x)
	dimnames(x) <- NULL
dimnames(x0) <- NULL
	y <- x0
#pk <- apply(y/sum(y), 3, sum)
dim(y) <- c(n[1], n[2] * n[3])	
pii <- apply(y/sum(y), 1, sum)
p <- min(p, n[1])
a <- svd(y)$u[, 1:p]
#browser()
#-------------------------------------------------------------
	y <- aperm(x0, c(2, 3, 1))
	dim(y) <- c(n[2], n[3] * n[1])
pj <- apply(y/sum(y), 1, sum)
  mj <- c(1:(n[2]))
Bpoly <- emerson.poly(mj, pj)	# Emerson orthogonal polynomials
	Bpoly <- Bpoly[, - c(1,2)  ]	#Bpoly <- Bpoly[1:p,  ]
	b <- diag(sqrt(pj)) %*% Bpoly[, 1:q]	#polinomio con pesi Di
	#cat("Checking the orthonormality of polynomials b:\n")
	#print(t(b) %*% (b))	
#-------------------------------------------------------------
# only the second variable is ordinal
	dimnames(y) <- NULL
    y <- aperm(x0, c(3, 1, 2))
    dim(y) <- c(n[3], n[1] * n[2])
    pk <- apply(y/sum(y), 1, sum)
    r <- min(r, n[3])
  	cc <- svd(y)$u[, 1:r]
#####################################################################################
	dimnames(x) <- nom
	list(a = as.matrix(a), b = as.matrix(b), cc = as.matrix(cc), g = NULL, x = x,pii=pii,pj=pj,pk=pk)
}
