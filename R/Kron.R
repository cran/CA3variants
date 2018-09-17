Kron <-
function(a, b){
    a <- as.matrix(a)
    b <- as.matrix(b)
    ni <- nrow(a)
    nj <- nrow(b)
    p <- ncol(a)
    q <- ncol(b)
    ab <- a %o% b    #ab di dimensione IxpxJxq
    ab <- aperm(ab, c(1, 3, 2, 4))	# bc di dimensione IxJ xpxq
    dim(ab) <- c(ni * nj, p * q)
    ab
}
