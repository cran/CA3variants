onsca3basic<-function(x, p,q,r, test = 10^-6, ctr = T, std = T,norder=3,sign = TRUE)
{
#-------------------------------------------------------------------------
##  3-way ordered non-symmetrical correspondence analysis
#  
#  x 3-way contingency table
# ni nj nk original table dimensions
#  p, q, r order of the decomposition
#  test    treshold used in the algorithm
#  ctr     (T or F) if F the analysis is not centered
#---------------------------------------------------------------------------
nnom <- dimnames(x)
I<-dim(x)[1]
 J<-dim(x)[2]    
 K<-dim(x)[3]
if (p==I){p<-p-1}
if (q==J){q<-q-1}
if (r==K){r<-r-1}
 nomi <- nnom[[1]]
nomj <- nnom[[2]]
nomk <- nnom[[3]]
tot<-sum(x)
n<-dim(x)
pi <- apply(x/tot, 1, sum)
devt <- 1 - sum(pi^2)
#cost<-(tot - 1) * (n[1] - 1) *  (1/devt)
xs <- rstand3(x, ctr = ctr, std = std)* sqrt((tot - 1) * (n[1] - 1) *(1/devt))#la fun orig standtab
#xs <- standtabnew(x, ctr = ctr, std = std)*sqrt(tot)#la fun orig standtab
#browser()
#xsg<-xs$xg
#browser()
#res <- tuckerORDEREDnotrivial(xsg, p=p, q=q, r=r,x, test=test,order=order)
if (sign==TRUE){
res <- tuckerORDERED(xs, p=p, q=q, r=r, x, test=test,norder=norder) #good
res<-signscore(res$a,res$b,res$cc,I,J,K,p=p,q=q,r=r,core=res$g,IFIXA=0,IFIXB=0,IFIXC=0) #given negative core, change signs in components
}
if (sign==FALSE){
res <- tuckerORDERED(xs, p=p, q=q, r=r, x, test=test,norder=norder) #good
}
#browser()
ncore<-dim(res$g)
np <- paste("p", 1:ncore[1], sep = "")
nq <- paste("q", 1:ncore[2], sep = "")
nr <- paste("r", 1:ncore[3], sep = "")
dimnames(res$g) <- list(np, nq, nr)
res$xs <- xs  #con standtab
#res$xs<-xsg  #con standtabnew
#cat("Total Variance, Estimated Variance, Proportions\n")
      xhat <- reconst3(res)
nx2 <- sum(xs^2)
res$tot <- nx2
nxhat2 <- sum(res$g^2)
res$prp <- nxhat2/nx2
#print(nx2, digit = 5)
#print(nxhat2, digit = 5)
#print(res$prp, digit = 3)
#cat("Squared Core Values")
#print(res$g^2, digit = 3)
#------------------------------------------------------
#  Chargement des resultats bruts pour les contributions
#-------------------------------------------------------
#res$ctr <- list(cti = res$a^2, ctj = res$b^2, ctk = res$cc^2)
#res <- coordrnsc3(res, x)# cat("Facteurs a\n")
res$xinit <- x
dimnames(res$a) <- list(nomi, np)
dimnames(res$b) <- list(nomj, nq)
dimnames(res$cc) <- list(nomk, nr)
onsca3results<-list( x = x, xs = xs, xhat = xhat, nxhat2 = 
                         nxhat2, prp = res$prp, a = res$a, b = res$b, cc = res$cc, g = res$g, 
                     iteration = res$cont)
#class(oca3results)<-"ca3basicresults"
return(onsca3results)
#oca3basic <- new("ca3basicresults", x = x, xs = xsg, xhat = xhat, nxhat2 = 
#                         nxhat2, prp = res$prp, a = res$a, b = res$b, cc = res$cc, g = res$g, 
#                     iteration = res$cont)
}


