chi3 <-
function(f3,digits=3){
    nn <- dim(f3)
    ni <- nn[1]
    nj <- nn[2]
    nk <- nn[3]
    n <- sum(f3)
    p3 <- f3/n
    if(length(dim(f3)) != 3){
        stop("f3 is not a 3 way table\n")
    }
    pi <- apply(p3, 1, sum)
    pj <- apply(p3, 2, sum)
    pk <- apply(p3, 3, sum)
    pijk <- pi %o% pj %o% pk
    khi3 <- n * (sum(p3^2/pijk) - 1)
    pij <- apply(p3, c(1, 2), sum)
    pik <- apply(p3, c(1, 3), sum)
    pjk <- apply(p3, c(2, 3), sum)
#alpha<- (pij%o%pk)+aperm(pik%o%pj,c(1,3,2))+aperm(pjk%o%pi,c(3,1,2))-2*pijk
#pijktab<-((p3-alpha))
#pijkint<-n*(sum((p3-alpha)^2/pijk)) #good

    khij <- n * (sum(pij^2/(pi %o% pj)) - 1)
    khik <- n * (sum(pik^2/(pi %o% pk)) - 1)
    khjk <- n * (sum(pjk^2/(pj %o% pk)) - 1)
    khin3 <- khi3 - khij - khik - khjk
    nom <- c("Term-IJ", "Term-IK", "Term-JK", "Term-IJK", 
        "Term-total")
xphi <- c(khij/n, khik/n, khjk/n, khin3/n, khi3/n)
    x <- c(khij, khik, khjk, khin3, khi3)
    y <- (100 * x)/khi3
    dijk <- (ni - 1) * (nj - 1) * (nk - 1)
    dij <- (ni - 1) * (nj - 1)
    dik <- (ni - 1) * (nk - 1)
    djk <- (nj - 1) * (nk - 1)
    dtot<-dij+dik+djk+dijk
    df <- c(dij, dik, djk, dijk, dtot)
   pvalue= 1 - pchisq(x, df)
x2<-x/df
    z <- rbind(x,xphi, y, df,pvalue,x2)
    nomr <- c("Chi-squared","Phi-squared", "% of Inertia", "df","p-value", "X2/df")
    dimnames(z) <- list(nomr, nom)
    z <- round(z, digits = digits)
    list(z = z,pij=pij,pik=pik,pjk=pjk)
}
