chi3ordered <-
function (f3, digits = 3) 
{
####################################
#f3 three-way absolute frequency table
# ordered response variable
#ordered predictor variables X1 and X2 
#it returns the three-way Pearson index, called chiIJK, and allow to partition this index
#without (result in table format) and with the orthogonal polynomials of Emerson
###################################
nn <- dim(f3)
    ni <- nn[1]
    ui <- rep(1, ni)
    nj <- nn[2]
    nk <- nn[3]
    n <- sum(f3)
    p3 <- f3/n
    if (length(dim(f3)) != 3) 
        stop("f3 is not a 3 way table \n")
    pi <- apply(p3, 1, sum)
    pj <- apply(p3, 2, sum)
    pk <- apply(p3, 3, sum)
    pijk <- pi %o% pj %o% pk #to get the product 
    p1jk <- ui %o% pj %o% pk #to get the product 
        khi3 <-n* sum(((p3 - pijk)^2/pijk)) #khi3 index
    pij <- apply(p3, c(1, 2), sum)
    pik <- apply(p3, c(1, 3), sum)
    pjk <- apply(p3, c(2, 3), sum)
        p2ij <- pi %o% pj
    p2ik <- pi %o% pk
p2jk<-pj %o% pk
    khiij <- sum(((pij - p2ij)^2/p2ij))*n
    khiik <- sum(((pik - p2ik)^2/p2ik))*n
    khijk <- n* (sum((pjk - p2jk)^2/p2jk))
    khin3 <- khi3 - khiij - khiik - khijk
   # cat("Values of partial and total indices\n")
    #nom <- c("chi2IJ", "chi2IK", "chi2JK", "chi2IJK",  "chi2")
    nom <- c("Term-IJ", "Term-IK", "Term-JK", "Term-IJK", "Term-total")
    dres <- (ni - 1) * (nj - 1) * (nk - 1)
    dij <- (ni - 1) * (nj - 1)
    dik <- (ni - 1) * (nk - 1)
    djk <- (nj - 1) * (nk - 1)
    dtot <- dij + dik + djk + dres
  #  zz <- c(khiij, khiik, khijk, khin3, khi3)
   # zz3 <- c(dij, dik, djk, dres, dtot)
    #z <- rbind(zz,  zz3)
    #nomr <- c("values of partial terms",   "degree of freedom")
    #dimnames(z) <- list(nomr, nom)
  #  print(round(z, digits = digits))
############################################ computing polynomials
mi<-c(1:ni)
Apoly <- emerson.poly(mi, pi)	# Emerson orthogonal polynomials
poli <- diag(sqrt(pi)) %*% Apoly[,-1]	#poly with weight Di
mj <- c(1:nj)
 Bpoly <- emerson.poly(mj, pj)
      polj <- diag(sqrt(pj)) %*%(Bpoly[,-1]) #poly with weight Dj
    mk <- c(1:nk)
    Cpoly <- emerson.poly(mk, pk)
    polk <- diag(sqrt(pk)) %*%(Cpoly[,-1]) #poly with weight Dk
##################################################partial term IJ
fij <- (pij - p2ij)/sqrt(p2ij)
z3par<-   t(poli[,-1])%*%fij%*%polj[,-1]
z3par2<-   t(poli)%*%fij%*%polj
chi2ij<-sum(z3par^2)*n 
pvalijtot<-1 - pchisq(chi2ij, (ni-1)*(nj-1))
#cat("index chi2_ij reconstructed by 2 polynomials without the 0th order poly \n")
chi2ijcol<-apply(z3par^2*n,2,sum)
chi2ijrow<-apply(z3par^2*n,1,sum)
pval<-c()
for (i in 1:(ni-1)){
pval[i]<-1 - pchisq(chi2ijrow[i], nj-1)
}
pvalchi2ijrow<-pval
pval<-c()
for (i in 1:(nj-1)){
pval[i]<-1 - pchisq(chi2ijcol[i], ni-1)
}
pvalchi2ijcol<-pval
#row and column poly component
dfi<-rep((nj-1),(ni-1))
perci<-chi2ijrow/chi2ij*100
zi<-cbind(chi2ijrow,perci,dfi,pvalchi2ijrow)
zitot<-c(apply(zi[,1:3],2,sum),pvalijtot)
zi<-rbind(zi,zitot)
dfj<-rep((ni-1),(nj-1))
percj<-chi2ijcol/chi2ij*100
zj<-cbind(chi2ijcol,percj,dfj,pvalchi2ijcol)
zjtot<-c(apply(zj[,1:3],2,sum),pvalijtot)
zj<-rbind(zj,zjtot)
zij<-rbind(zi,zj)
nomi<-paste("poly-row",1:(ni-1),sep="")
nomj<-paste("poly-col",1:(nj-1),sep="")
dimnames(zij)<-list(c(nomi,"Chi2-IJ",nomj,"Chi2-IJ"),c("Term-IJ-poly","%inertia","df","p-value"))
#################################################partial term IK
fik <- (pik - p2ik)/sqrt(p2ik)
z3par<-   t(poli[,-1])%*%fik%*%polk[,-1]
chi2ik<-sum(z3par^2)*n 
pvaliktot<-1 - pchisq(chi2ik, (ni-1)*(nk-1))
z3par2<-   t(poli)%*%fik%*%polk
chi2ikcol<-apply(z3par^2*n,2,sum)
chi2ikrow<-apply(z3par^2*n,1,sum)
pval<-c()
for (i in 1:(ni-1)){
pval[i]<-1 - pchisq(chi2ikrow[i], nk-1)
}
pvalchi2ikrow<-pval
pval<-c()
for (i in 1:(nk-1)){
pval[i]<-1 - pchisq(chi2ikcol[i], ni-1)
}
pvalchi2ikcol<-pval
#row and column poly component
dfi<-rep((nk-1),(ni-1))
perci<-chi2ikrow/chi2ik*100
zi<-cbind(chi2ikrow,perci,dfi,pvalchi2ikrow)
zitot<-c(apply(zi[,1:3],2,sum),pvaliktot)
zi<-rbind(zi,zitot)
dfk<-rep((ni-1),(nk-1))
perck<-chi2ikcol/chi2ik*100
zk<-cbind(chi2ikcol,perck,dfk,pvalchi2ikcol)
zktot<-c(apply(zk[,1:3],2,sum),pvaliktot)
zk<-rbind(zk,zktot)
zik<-rbind(zi,zk)
nomi<-paste("poly-row",1:(ni-1),sep="")
nomk<-paste("poly-tube",1:(nk-1),sep="")
dimnames(zik)<-list(c(nomi,"Chi2-IK",nomk,"Chi2-IK"),c("Term-IK-poly","%inertia","df","p-value"))
################################################partial term 
   fjk <- (pjk - p2jk)/sqrt(p2jk)
z3par<-   t(polj[,-1])%*%fjk%*%polk[,-1]
z3par2<-   t(polj)%*%fjk%*%polk
chi2jk<-sum(z3par^2)*n 
pvaljktot<-1 - pchisq(chi2jk, (nj-1)*(nk-1))
chi2jkcol<-apply(z3par^2*n,2,sum)
chi2jkrow<-apply(z3par^2*n,1,sum)
pval<-c()
for (i in 1:(nk-1)){
pval[i]<-1 - pchisq(chi2jkcol[i], nj-1)
}
pvalchi2jkcol<-pval
pval<-c()
for (i in 1:(nj-1)){
pval[i]<-1 - pchisq(chi2jkrow[i], nk-1)
}
pvalchi2jkrow<-pval
#row and column poly component
dfj<-rep((nk-1),(nj-1))
percj<-chi2jkrow/chi2jk*100
zj<-cbind(chi2jkrow,percj,dfj,pvalchi2jkrow)
zjtot<-c(apply(zj[,1:3],2,sum),pvaljktot)
zj<-rbind(zj,zjtot)
dfk<-rep((nj-1),(nk-1))
perck<-chi2jkcol/chi2jk*100
zk<-cbind(chi2jkcol,perck,dfk,pvalchi2jkcol)
zktot<-c(apply(zk[,1:3],2,sum),pvaljktot)
zk<-rbind(zk,zktot)
zjk<-rbind(zj,zk)
nomj<-paste("poly-col",1:(nj-1),sep="")
nomk<-paste("poly-tube",1:(nk-1),sep="")
dimnames(zjk)<-list(c(nomj,"Chi2-JK",nomk,"Chi2-JK"),c("Term-JK-poly","%inertia","df","p-value"))
#################################three ordered variables and interaction term
fijk=(p3 - pijk)/sqrt(pijk)
z3n<-   t(poli[,-1])%*%flatten(fijk)%*%Kron(polj[,-1],polk[,-1])
chi2int<-sum(z3n^2)*n
pvalijktot<-1 - pchisq(chi2int, (ni-1)*(nj-1)*(nk-1))
z3n2<-   t(poli)%*%flatten(fijk)%*%Kron(polj,polk)
chi2tot<-sum(z3n2^2)*n
dim(z3n)<-c(ni-1,nj-1,nk-1)
chi2introw<-apply(z3n^2*n,1,sum)
pval<-c()
for (i in 1:(ni-1)){
pval[i]<-1 - pchisq(chi2introw[i], ni-1)
}
pvalchi2introw<-pval
dfi<-rep((nj-1)*(nk-1),(ni-1))
#----------------------------------
chi2intcol<-apply(z3n^2*n,2,sum)
pval<-c()
for (i in 1:(nj-1)){
pval[i]<-1 - pchisq(chi2intcol[i], nj-1)
}
pvalchi2intcol<-pval
#-----------------------------------
chi2inttub<-apply(z3n^2*n,3,sum)
pval<-c()
for (i in 1:(nk-1)){
pval[i]<-1 - pchisq(chi2inttub[i], nk-1)
}
pvalchi2inttub<-pval
#-----------------------------------------
#row column and tube poly component on three-way intereraction term
perci<-chi2introw/chi2int*100
zi<-cbind(chi2introw,perci,dfi,pvalchi2introw)
zitot<-c(apply(zi[,1:3],2,sum),pvalijktot)
zi<-rbind(zi,zitot)
#----------
dfj<-rep((ni-1)*(nk-1),(nj-1))
percj<-chi2intcol/chi2int*100
zj<-cbind(chi2intcol,percj,dfj,pvalchi2intcol)
zjtot<-c(apply(zj[,1:3],2,sum),pvalijktot)
zj<-rbind(zj,zjtot)
#--------
dfk<-rep((ni-1)*(nj-1),(nk-1))
perck<-chi2inttub/chi2int*100
zk<-cbind(chi2inttub,perck,dfk,pvalchi2inttub)
zktot<-c(apply(zk[,1:3],2,sum),pvalijktot)
zk<-rbind(zk,zktot)
zijk<-rbind(zi,zj,zk)
nomi<-paste("poly-row",1:(ni-1),sep="")
nomj<-paste("poly-col",1:(nj-1),sep="")
nomk<-paste("poly-tube",1:(nk-1),sep="")
#browser()
dimnames(zijk)<-list(c(nomi, "Chi2-IJK",nomj,"Chi2-IJK",nomk,"Chi2-IJK"),c("Term-IJK-poly","%inertia","df","p-value"))
#============================================================
  zznew <- c(chi2ij, chi2ik, chi2jk, chi2int, chi2tot)
zphi <- c(chi2ij/n, chi2ik/n, chi2jk/n, chi2int/n, chi2tot/n)
   df<- c(dij, dik, djk, dres, dtot)
pvalue= 1 - pchisq(zznew, df)
perc<-zznew/chi2tot*100
x2<-zznew/df
    znew <- rbind(zznew, zphi, perc, df, pvalue,x2)
    nomr <- c("Chi-squared", "Phi-squared","%inertia",
  "df","p-value","X2/df")
    dimnames(znew) <- list(nomr, nom)
znew<-round(znew,digits=digits)
    return(list(z=znew,zij=zij,zik=zik,zjk=zjk,zijk=zijk,pij=pij,pik=pik,pjk=pjk))
}

