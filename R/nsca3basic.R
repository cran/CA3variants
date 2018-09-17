nsca3basic <-
function(x, p, q, r, test = 10^-6, ctr = T, std = T){
    
    nnom <- dimnames(x)
    nomi <- nnom[[1]]
    nomj <- nnom[[2]]
    nomk <- nnom[[3]]
    np <- paste("p", 1:p, sep = "")
    nq <- paste("q", 1:q, sep = "")
    nr <- paste("r", 1:r, sep = "")
    tot <- sum(x)
    n <- dim(x)
    pi <- apply(x/tot, 1, sum)
    devt <- 1 - sum(pi^2)
    xs <- rstand3(x, ctr = ctr, std = std) * sqrt((tot - 1) * (n[1] - 1) * 
                                                      (1/devt))
    res <- tucker(xs, p, q, r, test)
    dimnames(res$g) <- list(np, nq, nr)
    res$xs <- xs
    xhat <- reconst3(res)
    nx2 <- sum(xs^2)
    res$tot <- nx2    
    nxhat2 <- sum(xhat^2)
    nxhat2 <- sum(res$g^2)
    prp <- nxhat2/nx2
    
    res$ctr <- list(cti = res$a^2, ctj = res$b^2, ctk = res$cc^2)
   # res <- coordrnsc3(res, x)
    res$xinit <- x
    
    dimnames(res$a) <- list(nomi, np)
    dimnames(res$b) <- list(nomj, nq)
    dimnames(res$cc) <- list(nomk, nr)
    dimnames(xhat) <- list(nomi, nomj, nomk)
nsca3results<-list( x = x, xs = xs, xhat = xhat, nxhat2 = 
                          nxhat2, prp = prp, a = res$a, b = res$b, cc = res$cc, g = res$g, 
                      iteration = res$cont)
  #class(nsca3results)<-"ca3basicresults"
return(nsca3results)
 
#    nsca3basic <- new("ca3basicresults", x = x, xs = xs, xhat = xhat, nxhat2 = 
#                          nxhat2, prp = prp, a = res$a, b = res$b, cc = res$cc, g = res$g, 
#                      iteration = res$cont)
}
