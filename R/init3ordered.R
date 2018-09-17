init3ordered<-function (x, p, q, r, x0) 
{
    nom <- dimnames(x)
    n <- dim(x)
    dimnames(x) <- NULL
    dimnames(x0) <- NULL
    y <- x0
    dimnames(y) <- NULL
    dim(y) <- c(n[1], n[2] * n[3])
    pii <- apply(y/sum(y), 1, sum)
    p <- min(p, n[1])
    mj <- c(1:n[1])
    Bpoly <- emerson.poly(mj, pii)
    Bpoly <- Bpoly[,-c(1) ]
  # Bpoly <- t(Bpoly)
    cat("Bpoly \n")
    print(Bpoly)
    a <- diag(sqrt(pii)) %*% Bpoly[,1:p]
  #  a <- Bpoly
  
   cat("Checking the orthonormality of polynomials a:\n")
    print(t(a) %*% (a))
    y <- aperm(x0, c(2, 3, 1))
    dim(y) <- c(n[2], n[3] * n[1])
    pj <- apply(y/sum(y), 1, sum)
    q <- min(q, n[2])
    mj <- c(1:n[2])
    Bpoly <- emerson.poly(mj, pj)
    Bpoly <- Bpoly[,-c(1) ]
  #  Bpoly <- t(Bpoly)
    cat("Bpoly flattened\n")
    print(Bpoly)
   b <- diag(sqrt(pj)) %*% Bpoly[, 1:q]
 #   b <-  Bpoly

    cat("Checking the orthonormality of polynomials b:\n")
    print(t(b) %*% (b))
    y <- aperm(x0, c(3, 1, 2))
    dim(y) <- c(n[3], n[1] * n[2])
    pk <- apply(y/sum(y), 1, sum)
    r <- min(r, n[3])
    mj <- c(1:(n[3]))
    Bpoly <- emerson.poly(mj, pk)
    Bpoly <- Bpoly[,-c(1) ]
#    Bpoly <- t(Bpoly)
    cat("Bpoly flattened\n")
    print(Bpoly)
    cc <- diag(sqrt(pk)) %*% Bpoly[, 1:r]
 #  cc <-  Bpoly

    cat("Checking the orthonormality of polynomials cc\n")
    print(t(cc) %*% (cc))
    dimnames(x) <- nom
   # cat("fine di init3\n")
   # print(a)
   # print(b)
   # print(cc)
   # cat("inerzia ricostruita con i polinomi ortogonali\n")
    xsf <- flatten(x)
    #print(dim(xsf))
    bc <- Kron(b, cc)
    #print((bc))
    Z <- t(a) %*% xsf %*% bc
   # Z <- t(a) %*%diag(sqrt(pii))%*% xsf%*%Kron(diag(sqrt(pj)),diag(sqrt(pk))) %*% bc
 
   cat("index ==inerzia of Z'Z and ZZ' and Z2\n")
    print(sum(diag(t(Z) %*% Z)))
    print(sum(diag((Z) %*% t(Z))))
 # cat("Z table after Trivariate Moment Decomposition\n")
  #print(Z)
#zij=t(a)%*%apply(xsf,2,sum)%*%b
#browser()
    list(a = as.matrix(a), b = as.matrix(b), cc = as.matrix(cc), 
        g = NULL, x = x,pii=pii,pj=pj,pk=pk)
}
