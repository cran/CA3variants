tau3 <-
function(f3, digits = 3){
    nn <- dim(f3)
    ni <- nn[1]
    ui <- rep(1, ni)
    nj <- nn[2]
    nk <- nn[3]
    n <- sum(f3)
    p3 <- f3/n
    
    if(length(dim(f3)) != 3){
        stop("f3 is not a 3 way table \n")
    }
    
    pi <- apply(p3, 1, sum)
    pj <- apply(p3, 2, sum)
    pk <- apply(p3, 3, sum)
    pijk <- pi %o% pj %o% pk
    p1jk <- ui %o% pj %o% pk
    devt <- 1 - sum(pi^2)
    tau3 <- sum(((p3 - pijk)^2/p1jk))
    itau3 <- tau3/devt
    pij <- apply(p3, c(1, 2), sum)
    pik <- apply(p3, c(1, 3), sum)
    pjk <- apply(p3, c(2, 3), sum)
    p1j <- ui %o% pj
    p1k <- ui %o% pk
    p2ij <- pi %o% pj
    p2ik <- pi %o% pk
    tauij <- sum(((pij - p2ij)^2/p1j))
    itauij <- tauij/devt
    tauik <- sum(((pik - p2ik)^2/p1k))
    itauik <- tauik/devt
    khjk <- 1/ni * (sum((pjk - (pj %o% pk))^2/(pj %o% pk)))
    ikhjk<-khjk/devt        
    khin3 <- tau3 - tauij - tauik - khjk
    ikhin3 <-khin3/devt
   # cat("Numerator Values of partial and total indices\n")
    nom <- c("term-IJ", "term-IK", "term-JK", "term-IJK", "term-total")
   # nomI <- c("TauIJ", "TauIK", "TauJK", "TauIJK", "TauM")
    x <- c(tauij, tauik, khjk, khin3, tau3)
    y <- (100 * x)/tau3
    dres<-(ni-1)*(nj-1)*(nk-1)
    dij<-(ni-1)*(nj-1)
    dik<-(ni-1)*(nk-1)
    djk<-(nj-1)*(nk-1)
    dtot<-dij+dik+djk+dres
    CM <- (n - 1) * (ni - 1) * itau3        
    Cij<-(n-1)*(ni-1)*itauij
    Cik<-(n-1)*(ni-1)*itauik
    Cjk<-(n-1)*(ni-1)*ikhjk
    Cijk<-(n-1)*(ni-1)*ikhin3
    zz <- c(itauij, itauik, ikhjk, ikhin3,itau3)   
    zz2<-c(Cij,Cik,Cjk,Cijk,CM)
    zz3<-c(dij,dik,djk,dres,dtot)
x2<-zz2/zz3
 pvalue= 1 - pchisq(zz2, zz3)
      z <- rbind(x, zz,y,zz2,zz3,pvalue,x2)
    nomr <- c("Tau Numerator", "Tau", "% of Inertia", 
              "CM-Statistic","df","p-value","CM-Statistic/df")
    dimnames(z) <- list(nomr, nom)
    z <- round(z, digits = digits)
    list(z = z, CM = CM,pij=pij,pik=pik,pjk=pjk)
}

