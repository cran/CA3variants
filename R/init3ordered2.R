init3ordered2<-function(x, p, q, r, x0)
{
#-------------------------------------------------
# Initialisation of TUCKER3 by 
# Triple PCA for a three-way table on each
# way :
#    dim(x) is IxJxK
#    a      is   Ixp
#    b      is   Jxq
#    c      is   Kxr
# polinomi ortogonali calcolati solo sul terzo modo
#-------------------------------------------------
	nom <- dimnames(x)
	n <- dim(x)
	dimnames(x) <- NULL
	y <- x0
pii <- apply(y/sum(y), 1, sum)
pj <- apply(y/sum(y), 2, sum)
pk <- apply(y/sum(y), 3, sum)
y<-aperm(y,c(2,1,3))
dim(y) <- c(n[3], n[1] * n[2])	
r <- min(r, n[3], n[1] * n[2])
cc <- svd(y)$u[, 1:r]
#---------------------------------------------------------------
#  the first and second variables are ordinal
	dimnames(y) <- NULL
	r <- min(q, n[2])
	mj <- c(1:n[2])
	Bpoly <- emerson.poly(mj, pj)	# Emerson orthogonal polynomials
	Bpoly <- Bpoly[, - c(1)  ]	#
	b <- diag(sqrt(pj)) %*% Bpoly[, 1:q]	#polinomio con pesi Di
	cat("Checking the orthonormality of polynomials of the second mode b:\n")
	print(t(b) %*% (b))	
#-----------------------------------------------------	
mi <- c(1:n[1])
Bpoly <- emerson.poly(mi, pii)	# Emerson orthogonal polynomials
	Bpoly <- Bpoly[, - c(1)  ]	#Bpoly <- Bpoly[1:p,  ]
	a <- diag(sqrt(pii)) %*% Bpoly[, 1:p]	#polinomio con pesi Di
	cat("Checking the orthonormality of polynomials a:\n")
	print(t(a) %*% (a))	

	#####################################################################################
	list(a = as.matrix(a), b = as.matrix(b), cc = as.matrix(cc), g = NULL, x = x,pii=pii,pj=pj,pk=pk)
}
