margI <-
function(m){
    
    n <- dim(m)
    dimnames(m) <- NULL
    if(length(n) == 3) {
        n2 <- n[2] * n[3]
        dim(m) <- c(n[1], n2)
        mm <- apply(m, 1, sum)
        dim(m) <- n
        mm
    } else {
        cat(" Wrong dimension m")
        return(0)
    }
}
