CA3variants <-
function(Xtable, p = dim(Xtable)[[1]], q = dim(Xtable)[[2]], 
                        r = dim(Xtable)[[3]], ca3type = "CA3", test = 10^-6, norder=3){ 
        X <- as.array(Xtable)
    ni <- dim(X)[1]
    nj <- dim(X)[2]
    nk <-dim(X)[3]
labelgjk <- NULL 
    nomi <- dimnames(X)[[1]]
    nomj <- dimnames(X)[[2]]
    nomk <- dimnames(X)[[3]]
    n <- sum(X)
    maxaxes <- min(ni - 1, nj - 1, nk - 1)
    pj <- apply(X/sum(X), 2, sum)
    pk <- apply(X/sum(X), 3, sum)
    S <- switch(ca3type, "CA3" = ca3basic(X, p, q, r),  "NSCA3" = 
                    nsca3basic(X, p, q, r),"OCA3" = oca3basic(X, p, q, r,norder=norder),
                "ONSCA3" = onsca3basic(X, p, q, r,norder=norder))
     ######################################################################################
    #                                                                                                                                                                                                          #
    # Defines the analysis to be performed  for nominal variables- CA3 or NSCA3 -for ordinal variables OCA3 -ONSCA3    #
    #                                                                                                                                                                                                         #
    #####################################################################################
        if(ca3type == "CA3"){
    #    S <- ca3basic(X, p, q, r)
        pi <- apply(X/sum(X), 1, sum)
        index3 <- chi3(X)$z
firstdim<-1
lastdim<-2
    } 
        if(ca3type == "OCA3"){
      #  S <- oca3basic(X, p, q, r)
        pi <- apply(X/sum(X), 1, sum)
chi2res<-chi3ordered(X)
#print(chi2res)
        index3 <- chi3ordered(X)$zijk
firstdim<-2
lastdim<-3
#browser()   
 } 
if(ca3type == "NSCA3") {
     #   S <- nsca3basic(X, p, q, r)
        index3 <- tau3(X)$z
        pi <- rep(1, ni)
firstdim<-1
lastdim<-2
    }
    if(ca3type == "ONSCA3"){
      #  S <- oca3basic(X, p, q, r)
      pi <- rep(1, ni)
      #tauMres<-tau3ordered(X)
      #print(chi2res)
      index3 <- tau3ordered(X)$zijk
firstdim<-2
lastdim<-3
      #browser()   
    } 
####################################################################
    #                                                                  #
    # Calculation of coordinates  for CA3 and NSCA3                    #
    #                                                                  #
    ####################################################################
# if((ca3type == "CA3")|(ca3type == "NSCA3")) {
   fiStandard <-  S$a
    #fi <- diag(sqrt(pi)) %*% S$a     
    # Standard row coordinates 
#    fi<- diag(1/sqrt(pi))  %*% S$a %*% flatten(S$g) 
fi <- S$a %*% flatten(S$g)
    # Principal row coordinates
#    gjkStandard <- Kron(diag(sqrt(pj)) %*%S$b,diag(sqrt(pk)) %*% S$cc ) 
gjkStandard <- Kron(S$b,S$cc ) 
    gjk <- Kron(S$b,S$cc) %*% t(flatten(S$g)) 
        # Calculating the column-tube principal coordinates
labelfi<-nomi
 for (i in 1:nk){
        labelgjk <- c(labelgjk, paste(nomj, nomk[i], sep = ""))
    }
 fidim<-p
fiCdim<-q*r      
#}#end if ca3type
    nr <- dim(gjk)[[1]]
    nc <- dim(gjk)[[2]]
  nc2 <- dim(gjkStandard)[[2]]
productfigjk <- fiStandard %*% t(gjk)
    productfigjk <- round(productfigjk, digits = 2) # Inner product 
    dimnames(gjk) <- list(labelgjk, paste("ax", 1:nc, sep = ""))
  dimnames(gjkStandard) <- list(labelgjk, paste("ax", 1:nc2, sep = ""))
    dimnames(productfigjk) <- list(labelfi, labelgjk)
    dimnames(fi) <- list(labelfi, paste("ax", 1:fiCdim, sep = ""))
    dimnames(fiStandard) <- list(labelfi, paste("ax", 1:fidim, sep = ""))
    ####################################################################
    #                                                                  #
    # Calculation inertia values                                       #
    #                                                                  #
    ####################################################################
    inertia <- sum(S$xs^2)
    inertiapc0 <- (S$g^2/inertia*100)
    inertiaRSS <- (inertia-sum(S$g^2))
 inertiacoltub <- apply(inertiapc0,1,sum)  
inertiarow <- c(apply(inertiapc0,c(2,3),sum))  
 #   ca3corporateresults<-new("ca3corporateresults", DataMatrix = X, xs = 
 #                                S$xs, xhat = S$xhat, nxhat2 = S$nxhat2, prp = S$prp, a=S$a,b=S$b,cc=S$cc,
#fi = fi, fiStandard= fiStandard, gjk = gjk,gjkStandard=gjkStandard, rows = ni, cols = nj, tubes = nk, flabels = 
 #                                labelfi, glabels = labelgjk, maxaxes = maxaxes, 
 #                            inertia = inertia,inertiaRSS = inertiaRSS, inertiapc0 = inertiapc0, inertiacoltub = inertiapc1coltub,inertiarow=inertiapc2row, 
 #                            iproduct = productfigjk, g = S$g, index3 = index3, ca3type = 
 #                                ca3type, iteration = S$iteration)
   #   print(ca3corporateresults)
   # plot(ca3corporateresults, cex = cex, firstaxis = firstaxis, lastaxis = 
   #         lastaxis, plottype = plottype, prop = prop,arrow=arrow)
ca3corporateresults<-list(DataMatrix = X, xs = 
                                 S$xs, xhat = S$xhat, nxhat2 = S$nxhat2, prp = S$prp, a=S$a,b=S$b,cc=S$cc,
fi = fi, fiStandard= fiStandard, gjk = gjk,gjkStandard=gjkStandard, rows = ni, cols = nj, tubes = nk, flabels = 
                                 labelfi, glabels = labelgjk, maxaxes = maxaxes, 
                             inertia = inertia, inertiaRSS=inertiaRSS, inertiapc = inertiapc0, inertiacoltub = inertiacoltub,inertiarow=inertiarow, 
                             iproduct = productfigjk, g = S$g, index3 = index3, ca3type = 
                                 ca3type, iteration = S$iteration,firstaxis=firstdim,lastaxis=lastdim)
 
class(ca3corporateresults)<-"CA3variants"
invisible(return(ca3corporateresults))
}
