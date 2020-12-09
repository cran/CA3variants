ca3basic <-
function(x, p, q, r, test = 10^-6, ctr = T, std = T){
        nnom <- dimnames(x)
    nomi <- nnom[[1]]
    nomj <- nnom[[2]]
    nomk <- nnom[[3]]
   # np <- paste("p", 1:p, sep = "")
   # nq <- paste("q", 1:q, sep = "")
   # nr <- paste("r", 1:r, sep = "")
    xs <- standtab(x, ctr = ctr, std = std) * sqrt(sum(x)) #to get chi3 
    res <- tucker(xs, p, q, r, test)
ncore<-dim(res$g)
np <- paste("p", 1:ncore[1], sep = "")
nq <- paste("q", 1:ncore[2], sep = "")
nr <- paste("r", 1:ncore[3], sep = "")
    dimnames(res$g) <- list(np, nq, nr)
    res$xs <- xs
    xhat <- reconst3(res)
    nx2 <- sum(xs^2)
    res$tot <- nx2    
    nxhat2 <- sum(xhat^2)
    ng2 <- sum(res$g^2)
    prp <- ng2/nx2
    res$ctr <- list(cti = res$a^2, ctj = res$b^2, ctk = res$cc^2)
comp <- coord(res, x)    
res$xinit <- x
#browser()
    dimnames(comp$a) <- list(nomi, np)
    dimnames(comp$b) <- list(nomj, nq)
    dimnames(comp$cc) <- list(nomk, nr)
    dimnames(xhat) <- list(nomi, nomj, nomk)
ca3results<-list( x = x, xs = xs, xhat = xhat, nxhat2 = 
                         nxhat2, prp = prp, a = comp$a, b = comp$b, cc = comp$cc, g = res$g, 
                     iteration = res$cont)
#class(ca3results)<-"ca3basicresults"
return(ca3results)


}

