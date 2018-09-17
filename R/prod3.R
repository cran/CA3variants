prod3 <-
function(m, a1, a2, a3){
    
    n <- dim(m)
    nn <- dimnames(m)
    dimnames(m) <- NULL
    if(!missing(a1)){
        if(length(a1) != n[1]){
            stop("prod3: length(arg1) wrong")
        }
        dim(m) <- c(n[1], n[2] * n[3])
        m <- diag(a1) %*% m
        dim(m) <- n
    }
    if(!missing(a2)){
        if(length(a2) != n[2]){
            stop("prod3: length(arg2) wrong")
        }
        m <- aperm(m, c(2, 1, 3))
        dim(m) <- c(n[2], n[1] * n[3])
        m <- diag(a2) %*% m
        dim(m) <- c(n[2], n[1], n[3])
        m <- aperm(m, c(2, 1, 3))
    }
    if(!missing(a3)){
        if(length(a3) != n[3]){
            stop("prod3: length(arg3) wrong")
        }
        dim(m) <- c(n[1] * n[2], n[3])
        m <- m %*% diag(a3)
        dim(m) <- n
    }
    dimnames(m) <- nn
    m
}
