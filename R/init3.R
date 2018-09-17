init3 <-
function(x, p, q, r){
    
    nom <- dimnames(x)
    n <- dim(x)
    dimnames(x) <- NULL
    y <- x
    dim(y) <- c(n[1], n[2] * n[3])
    p <- min(p, n[1], n[2] * n[3])
    a <- svd(y)$u[, 1:p]
    y <- aperm(x, c(2, 3, 1))
    dim(y) <- c(n[2], n[3] * n[1])
    q <- min(q, n[2], n[1] * n[3])
    b <- svd(y)$u[, 1:q]
    y <- aperm(x, c(3, 1, 2))
    dim(y) <- c(n[3], n[1] * n[2])
    r <- min(r, n[3], n[1] * n[2])
    cc <- svd(y)$u[, 1:r]
    dimnames(x) <- nom
    list(a = as.matrix(a), b = as.matrix(b), cc = as.matrix(cc), g = NULL, 
         x = x)
}
