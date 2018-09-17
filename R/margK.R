margK <-
function(m){
    
    n <- dim(m)
    dimnames(m) <- NULL
    if(length(n) == 3) {
        n1 <- n[1] * n[2]
        dim(m) <- c(n1, n[3])
        mm <- apply(m, 2, sum)
    } else {
        cat(" Wrong dimension m")
        return(0)
    }
}
