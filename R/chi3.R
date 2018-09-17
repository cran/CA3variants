chi3 <-
function(f3,digits=3){
    nn <- dim(f3)
    ni <- nn[1]
    nj <- nn[2]
    nk <- nn[3]
    n <- sum(f3)
    p3 <- f3/n
    if(length(dim(f3)) != 3){
        stop("f3 is'not a 3 way table\n")
    }
    pi <- apply(p3, 1, sum)
    pj <- apply(p3, 2, sum)
    pk <- apply(p3, 3, sum)
    pijk <- pi %o% pj %o% pk
    khi3 <- n * (sum(p3^2/pijk) - 1)
    pij <- apply(p3, c(1, 2), sum)
    pik <- apply(p3, c(1, 3), sum)
    pjk <- apply(p3, c(2, 3), sum)
    khij <- n * (sum(pij^2/(pi %o% pj)) - 1)
    khik <- n * (sum(pik^2/(pi %o% pk)) - 1)
    khjk <- n * (sum(pjk^2/(pj %o% pk)) - 1)
    khin3 <- khi3 - khij - khik - khjk
    
    nom <- c("X2IJ", "X2IK", "X2JK", "X2INTER", "X2IJK")
    x <- c(khij, khik, khjk, khin3, khi3)
    y <- (100 * x)/khi3
    dijk <- (ni - 1) * (nj - 1) * (nk - 1)
    dij <- (ni - 1) * (nj - 1)
    dik <- (ni - 1) * (nk - 1)
    djk <- (nj - 1) * (nk - 1)
    dtot<-dij+dik+djk+dijk
    df <- c(dij, dik, djk, dijk, dtot)
   pvalue= 1 - pchisq(x, df)
    z <- rbind(x, y, df,pvalue)
    nomr <- c("Index", "% of Inertia", "df","p-value")
    dimnames(z) <- list(nomr, nom)
    z <- round(z, digits = digits)
    list(z = z)
}
