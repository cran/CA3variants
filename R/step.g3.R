step.g3 <-
function(param) {
    a <- param$a
    b <- param$b
    cc <- param$cc
    p <- ncol(a)
    q <- ncol(b)
    r <- ncol(cc)
    x <- param$x
    x <- flatten(x)
    ax <- t(a) %*% x
    bc <-Kron( b, cc)
    g <- ax %*% bc
    dim(g) <- c(p, q, r)
    param$g <- g
    param
}
