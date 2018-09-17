rstand3 <-
function(x, std = T, ctr = T){
    
    ntot <- sum(x)
    
    if(length(dim(x)) != 3){
        stop("standtab: this is not a 3-way array!\n")
    }
    if(ntot == 0){
        stop("This matrix as for sum zero!")
    }
    
    x <- x/ntot
    pii <- margI(x)
    pj <- margJ(x)
    pk <- margK(x)
    ui <- rep(1, length(pii))
    uj <- rep(1, length(pj))
    y <- rep(1, length(x))
    dim(y) <- dim(x)
    if(ctr){ 
        y <- prod3(y, pii, pj, pk)
    } else {
        y <- 0
    }
    x <- x - y
    if(std){
        prod3(x, ui, 1/sqrt(pj), 1/sqrt(pk))
    } else {
        prod3(x, ui, 1/pj, 1/pk)
    }
}
