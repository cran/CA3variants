CA3variants<-
function(Xdata, dims=c(p,q,r), ca3type = "CA3", test = 10^-6, resp="row", norder=3, sign=TRUE){ 
#------------------------------------------
#Xdata can be an array or raw data
#--------------------------------------
if ( missing(dims)) {stop("'dims' must be given without default\n number of dimension for rows, columns and tubes must be given\n 
for example: dims=c(p=2,q=2,r=2). For guidance, use the 'tunelocal' function\n")
}
if (is.array(Xdata)==FALSE){
  #Xdata=dget("Xdata.txt")
Xdata1<-Xdata  
#attach(Xdata1)
  nam<-names(Xdata1)
ndims<-dim(Xdata1)[[2]]
if (ndims<3) {stop("number of vars for output must be at least 3\n\n")}
Xdata2=table(Xdata1[[1]],Xdata1[[2]],Xdata1[[3]])
  if(is.numeric(Xdata1)==TRUE){  
dimnames(Xdata2)=list(paste(nam[[1]],1:max(Xdata[[1]]),sep=""),paste(nam[[2]],1:max(Xdata[[2]]),sep=""),paste(nam[[3]],1:max(Xdata[[3]]),sep=""))
}  
#paste("This is your given array\n\n")
  Xdata<-Xdata2
#print(Xdata)
  }
if (is.array(Xdata)==FALSE) {stop("number of variables for three-way analysis must be at least 3\n\n")}

# READ DATA FILE
#Xdata <- read.table(file = datafile, header=header)
#if (header==FALSE) { 
#for (i in 1:dim(Xdata)[1]) rownames(Xdata)[i] <- paste("r",i,sep="")
#for (i in 1:dim(Xdata)[2]) colnames(Xdata)[i] <- paste("c",i,sep="")
#}
X <- as.array(Xdata)
if (resp=="row"){
p<-dims[[1]]
q<-dims[[2]]
r<-dims[[3]]
}
#-------------------------------------------------------------------------------
if ((resp=="col")||(resp=="column")){
X<-aperm(X,c(2,1,3))
p<-dims[[2]]
q<-dims[[1]]
r<-dims[[3]]}
if (resp=="tube"){
X<-aperm(X,c(3,2,1))
p<-dims[[3]]
q<-dims[[2]]
r<-dims[[1]]}
#------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------
    ni <- dim(X)[1]
    nj <- dim(X)[2]
    nk <-dim(X)[3]
labelgjk <- NULL 
labelgik <- NULL 
labelgij <- NULL 
    nomi <- dimnames(X)[[1]]
    nomj <- dimnames(X)[[2]]
    nomk <- dimnames(X)[[3]]
    n <- sum(X)
pii <- apply(X/sum(X), 1, sum)
    pj <- apply(X/sum(X), 2, sum)
    pk <- apply(X/sum(X), 3, sum)
  #---------------------------
    S <- switch(ca3type, "CA3" = ca3basic(X, p=p, q=q, r=r, sign=sign),  "NSCA3" = 
                    nsca3basic(X, p=p, q=q, r=r, sign=sign),"OCA3" = oca3basic(X, p=p, q=q, r=r,norder=norder, sign=sign),
                "ONSCA3" = onsca3basic(X, p=p, q=q, r=r,norder=norder, sign=sign))
     ######################################################################################
    #                                                                                                                                                                                                          #
    # Defines the analysis to be performed  for nominal variables- CA3 or NSCA3 -for ordinal variables OCA3 -ONSCA3    #
    #                                                                                                                                                                                                         #
    #####################################################################################
        if(ca3type == "CA3"){
           pi <- apply(X/sum(X), 1, sum)
index3res<-chi3(X)        
index3 <- index3res$z
#indexij<-NULL
#indexik<-NULL
#indexjk<-NULL
    } 
        if(ca3type == "OCA3"){
           pi <- apply(X/sum(X), 1, sum)
index3res<-chi3ordered(X)
        index3 <- index3res$zijk
#indexij<-chi3ordered(X)$zij
#indexik<-chi3ordered(X)$zik
#indexjk<-chi3ordered(X)$zjk

 } 
if(ca3type == "NSCA3") {
index3res<-tau3(X)
        index3 <- tau3(X)$z
        pi <- rep(1, ni)
#indexij<-NULL
#indexik<-NULL
#indexjk<-NULL
    }
    if(ca3type == "ONSCA3"){
      #  S <- oca3basic(X, p, q, r)
      pi <- rep(1, ni)
      #tauMres<-tau3ordered(X)
      #print(chi2res)
index3res<-tau3ordered(X)
      index3 <- index3res$zijk
#indexij<-tau3ordered(X)$zij
#indexik<-tau3ordered(X)$zik
#indexjk<-tau3ordered(X)$zjk
    } 
#---------------------two-way margin tables
pij<-index3res$pij
pik<-index3res$pik
pjk<-index3res$pjk

####################################################################
    #                                                                  #
    # Calculation of coordinates  for CA3  NSCA3   and all oordered variants                 #
    #                                                                  #
    ####################################################################
#----sign core issue---
#g<-sqrt(S$g^2) #the sign of core values will be always positive...

g<-S$g
#----------------------------------------------------------------------for biptype row and column-tube
   fiStandard <-  S$a
        # Standard row coordinates 
ncore<-dim(S$g)
 fidim<-ncore[1]
fiCdim<-ncore[2]*ncore[3]   
fi <- S$a %*% flatten(g)
#------------------------------------core sign issue!!!
#for (i in 1:fidim){
#for (j in 1:fiCdim){
#if (flatten(g)[i,j]<0) fiStandard[i,j]<-(-1)*fiStandard[i,j]
#}}
#------------------------------------------------------------
    # Principal row coordinates
gjkStandard <- Kron(S$b,S$cc ) 
    gjk <- Kron(S$b,S$cc) %*% t(flatten(g)) 
        # Calculating the column-tube principal coordinates
#------------------------------------core sign issue!!!
#for (i in 1:fidim){
#for (j in 1:fiCdim){
#if (flatten(g)[i,j]<0) 
#gjkStandard[i,j]<-(-1)*gjkStandard[i,j]  #change the sign
# fi[i,j]<-(-1)*fi[i,j]  #change the sign
#}}
#------------------------------------------------------------
labelfi<-nomi
 for (i in 1:nk){
        labelgjk <- c(labelgjk, paste(nomj, nomk[i], sep = ""))
    }
     nr <- dim(gjk)[[1]]
    nc <- dim(gjk)[[2]]
  nc2 <- dim(gjkStandard)[[2]]
productfigjk <- fiStandard %*% t(gjk)
    productfigjk <- round(productfigjk, digits = 2) # Inner product 
    dimnames(gjk) <- list(labelgjk, paste("ax", 1:nc, sep = ""))
  dimnames(gjkStandard) <- list(labelgjk, paste("ax", 1:nc2, sep = ""))
    dimnames(productfigjk) <- list(labelfi, labelgjk)
    dimnames(fi) <- list(labelfi, paste("ax", 1:fiCdim, sep = ""))
 dimnames(fiStandard) <- list(labelfi, paste("ax", 1:dim(S$a)[[2]], sep = ""))
#---------------------------------------------------------------------------------------biptype =col and row-tube
fjStandard <-  S$b # Standard col coordinates 
fj <- S$b %*% flatten(aperm(g,c(2,1,3)))    # Principal col coordinates JxPR
gikStandard <- Kron(S$a,S$cc ) # interactive coordinates IKxPR
    gik <- Kron(S$a,S$cc) %*% t(flatten(aperm(g,c(2,1,3)))) # interactive principal coordinates
        # Calculating the labels for the column-tube principal coordinates
labelfj<-nomj
 for (i in 1:nk){
        labelgik <- c(labelgik, paste(nomi, nomk[i], sep = ""))
    }
fjdim<-ncore[2]
#fjCdim<-p*r      
 fjCdim<-ncore[1]*ncore[3]
   nr <- dim(gik)[[1]]
    nc <- dim(gik)[[2]]
  nc2 <- dim(gikStandard)[[2]]
productfjgik <- fjStandard %*% t(gik)
    productfjgik <- round(productfjgik, digits = 2) # Inner product 
    dimnames(gik) <- list(labelgik, paste("ax", 1:nc, sep = ""))
  dimnames(gikStandard) <- list(labelgik, paste("ax", 1:nc2, sep = ""))
    dimnames(productfjgik) <- list(labelfj, labelgik)
    dimnames(fj) <- list(labelfj, paste("ax", 1:fjCdim, sep = ""))
 dimnames(fjStandard) <- list(labelfj, paste("ax", 1:dim(fjStandard)[[2]], sep = ""))
#--------------------------------------
#---------------------------------------------------------------------------------------biptype tube and row-col
fkStandard <-  S$cc # Standard col coordinates 
fk <- S$cc %*% flatten(aperm(g,c(3,1,2)))    # Principal col coordinates JxPR
gijStandard <- Kron(S$a,S$b ) # interactive coordinates IKxPR
    gij <- Kron(S$a,S$b) %*% t(flatten(aperm(g,c(3,1,2)))) # interactive principal coordinates

        # Calculating the labels for the column-tube principal coordinates
labelfk<-nomk
 for (i in 1:nj){
        labelgij <- c(labelgij, paste(nomi, nomj[i], sep = ""))
    }

fkdim<-ncore[3]
#fkCdim<-p*q      
fkCdim<-ncore[1]*ncore[2]
    nr <- dim(gij)[[1]]
    nc <- dim(gij)[[2]]
  nc2 <- dim(gijStandard)[[2]]
productfkgij <- fkStandard %*% t(gij)
    productfkgij <- round(productfkgij, digits = 2) # Inner product 
    dimnames(gij) <- list(labelgij, paste("ax", 1:nc, sep = ""))
  dimnames(gijStandard) <- list(labelgij, paste("ax", 1:nc2, sep = ""))
    dimnames(productfkgij) <- list(labelfk, labelgij)
    dimnames(fk) <- list(labelfk, paste("ax", 1:fkCdim, sep = ""))
 dimnames(fkStandard) <- list(labelfk, paste("ax", 1:dim(fkStandard)[[2]], sep = ""))
#--------------------------------------
    ####################################################################
    #                                                                  #
    # Calculation inertia values                                       #
    #                                                                  #
    ####################################################################
    inertiaorig <- sum(S$xs^2) #total inertia of the table (I,J,K) equal to the related index 
    inertiatot<-sum(S$g^2) #inertia reconstructed when p,q and r are different from I, J and K dimensions    
prp <- sum(S$g^2)/inertiaorig  
    inertiapc <- (S$g^2/inertiaorig*100)
 #   inertiaRSS <- (inertiaorig-inertiatot)
#----------------------------------------------------------for row and col-tube biplot
    inertiacoltub <- apply(inertiapc,1,sum)  
    inertiarow <- c(apply(inertiapc,c(2,3),sum)) 
    names(inertiarow)<-paste("ax", 1:fiCdim, sep = "")
#----------------------------------------------------------for col and row-tube biplot
    inertiarowtub <- apply(inertiapc,2,sum)  
    inertiacol <- c(apply(inertiapc,c(1,3),sum)) 
    names(inertiacol)<-paste("ax", 1:fjCdim, sep = "")
#----------------------------------------------------------for tube and row-col biplot
    inertiarowcol <- apply(inertiapc,3,sum)  
    inertiatube <- c(apply(inertiapc,c(1,2),sum)) 
    names(inertiatube)<-paste("ax", 1:fkCdim, sep = "")
#-----------------------------------------------------------------------------------------------------------------------

#ca3corporateresults<-list(Data = X, ca3type = ca3type, rows = ni, cols = nj, tubes = nk, labelfi =labelfi, labelgjk = labelgjk,labelfj =labelfj, labelgik = labelgik, labelfk =labelfk, labelgij = #labelgij, iteration = S$iteration, pi=pii, pj=pj, pk=pk, pij=pij, pik=pik, pjk=pjk,
#prp = S$prp, a=S$a,b=S$b,cc=S$cc,
#fi = fi, fiStandard= fiStandard, gjk = gjk,gjkStandard=gjkStandard, fj = fj, fjStandard= fjStandard, gik = gik,gikStandard=gikStandard, 
#fk = fk, fkStandard= fkStandard, gij = gij,gijStandard=gijStandard, 
#inertiaorig = inertiaorig,inertiatot=inertiatot, inertiaRSS=inertiaRSS, inertiapc=inertiapc,
#inertiapcsum = inertiapcsum, inertiacoltub = inertiacoltub,inertiarow=inertiarow, inertiarowtub = inertiarowtub,inertiacol=inertiacol,
#inertiarowcol = inertiarowcol,inertiatube=inertiatube, iproductijk = productfigjk,iproductjik = productfjgik,iproductkij = productfkgij, 
#g = S$g, index3res = index3res, index3=index3, 
#gammai=gammai,gammajk=gammajk,
#gammaj=gammaj,gammaik=gammaik,gammak=gammak,gammaij=gammaij)
 
ca3corporateresults<-list(Data = X, ca3type = ca3type, resp=resp,  pi=pii, pj=pj, pk=pk, pij=pij, pik=pik, pjk=pjk,
prp = prp, a=S$a,b=S$b,cc=S$cc,
fi = fi, fiStandard= fiStandard, gjk = gjk,gjkStandard=gjkStandard, fj = fj, fjStandard= fjStandard, gik = gik,gikStandard=gikStandard, 
fk = fk, fkStandard= fkStandard, gij = gij,gijStandard=gijStandard, 
inertiaorig = inertiaorig,inertiatot=inertiatot,  inertiapc=inertiapc, inertiacoltub = inertiacoltub,inertiarow=inertiarow, inertiarowtub = inertiarowtub,inertiacol=inertiacol,
inertiarowcol = inertiarowcol,inertiatube=inertiatube, iproductijk = productfigjk,iproductjik = productfjgik,iproductkij = productfkgij, 
g = S$g, index3res = index3res, index3=index3)

class(ca3corporateresults)<-"CA3variants"
invisible(return(ca3corporateresults))
}

