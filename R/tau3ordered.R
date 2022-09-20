tau3ordered<-
  function (f3, digits = 3) 
  {
    ####################################
    #f3 three-way absolute frequency table
    # ordered response variable: orderedY=T,F
    #number of ordered predictor variables: orderedX=0 or 1 or 2 if orderedX=1 then consider X1 ordered and X2 not ordered
    #########################
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
    devt <- 1 - sum(pi^2) #tau3 denominator
    tau3 <- sum(((p3 - pijk)^2/p1jk)) #tau3 numerator
    itau3 <- tau3/devt #tau3 index
    pij <- apply(p3, c(1, 2), sum)
    pik <- apply(p3, c(1, 3), sum)
    pjk <- apply(p3, c(2, 3), sum)
    p1j <- ui %o% pj
    p1k <- ui %o% pk
    p2ij <- pi %o% pj
    p2ik <- pi %o% pk
    p2jk<-pj %o% pk
    tauij <- sum(((pij - p2ij)^2/p1j))
    itauij <- tauij/devt
    tauik <- sum(((pik - p2ik)^2/p1k))
    itauik <- tauik/devt
    khjk <- 1/ni * (sum((pjk - (pj %o% pk))^2/(pj %o% pk)))
    ikhjk <- khjk/devt
    khin3 <- tau3 - tauij - tauik - khjk
    ikhin3 <- khin3/devt
    cost<-(n - 1) * (ni - 1) *  (1/devt)
    #cat("Numerator Values of partial and total indices\n")
  nom <- c("Term-IJ", "Term-IK", "Term-JK", "Term-IJK", "Term-total")
     # nomI <- c("TauIJ", "TauIK", "TauJK", "TauIJK", "TauM")
    x <- c(tauij, tauik, khjk, khin3, tau3)
    y <- (100 * x)/tau3
    dres <- (ni - 1) * (nj - 1) * (nk - 1)
    dij <- (ni - 1) * (nj - 1)
    dik <- (ni - 1) * (nk - 1)
    djk <- (nj - 1) * (nk - 1)
    dtot <- dij + dik + djk + dres
    CM <- (n - 1) * (ni - 1) * itau3
    Cij <- (n - 1) * (ni - 1) * itauij
    Cik <- (n - 1) * (ni - 1) * itauik
    Cjk <- (n - 1) * (ni - 1) * ikhjk
    Cijk <- (n - 1) * (ni - 1) * ikhin3
    zz <- c(itauij, itauik, ikhjk, ikhin3, itau3)
    zz2 <- c(Cij, Cik, Cjk, Cijk, CM)
    zz3 <- c(dij, dik, djk, dres, dtot)
pvalue<-1-pchisq(zz2,zz3)
x2<-zz2/zz3
    z <- rbind(x, zz, y, zz2, zz3,pvalue,x2)
    nomr <- c("Tau-Numerator", "Tau-index", 
              "% of Inertia", "CM-Statistic", "df","p-value","CM-statistic/df")
    dimnames(z) <- list(nomr, nom)
   z<-round(z, digits = digits)
   # cat("C-statistic of Marcotorchino\n")
   # print(CM)
   # cat("Tau3 denominator\n")
   # print(devt)
    ############################################ computing polynomials
    mi<-c(1:ni)
    Apoly <- emerson.poly(mi, pi)	# Emerson orthogonal polynomials
    Apoly <- Apoly[, - c(1)  ] #delete zero-vec but with trivial solution to reconstruct tau
     poli <- diag(sqrt(pi)) %*% Apoly	#poly with weight Di
       mj <- c(1:nj)
    Bpoly <- emerson.poly(mj, pj)
    Bpoly <- Bpoly[,-c(1) ]
    polj <- diag(sqrt(pj)) %*%(Bpoly) #poly with weight Dj
        mk <- c(1:nk)
    Cpoly <- emerson.poly(mk, pk)
    Cpoly <- Cpoly[,-c(1) ]
    polk <- diag(sqrt(pk)) %*%(Cpoly) #poly with weight Dk
    #polk <- t(Cpoly)
    #xs<-rstand3(f3) #standardized residuals 
    #xs<-p3
    ##########################################################partial term IJ
    fij <- (pij - p2ij)/sqrt(p1j)
    z3par<-   t(poli)%*%fij%*%polj
    tauij<-(sum(z3par^2)/(devt))*(ni-1)*(n-1)
    pvalijtot<-1 - pchisq(tauij, (ni-1)*(nj-1))
  #  cat("index numerator of tau_ij reconstructed by 2 polynomials \n")
  #  print(tauij)
    z3ij<-(z3par^2/(devt))*(ni-1)*(n-1)
    #cat("partition of index numerator of tau_ij reconstructed by 2 polynomials \n")
    tauijcol<-apply(z3ij,2,sum)
    tauijrow<-apply(z3ij,1,sum)
    #browser()
    pval<-c()
    for (i in 1:(ni)){
      pval[i]<-1 - pchisq(tauijrow[i], nj-1)
    }
    pvaltauijrow<-pval
    pval<-c()
    for (i in 1:(nj)){
      pval[i]<-1 - pchisq(tauijcol[i], ni-1)
    }
    pvaltauijcol<-pval
    #row and column poly component
    dfi<-rep((nj-1),(ni))
    perci<-tauijrow/tauij*100
    zi<-cbind(tauijrow,perci,dfi,pvaltauijrow)
    zitot<-c(apply(zi[,1:3],2,sum),pvalijtot)
    zi<-rbind(zi,zitot)
    dfj<-rep((ni-1),(nj))
    percj<-tauijcol/tauij*100
    zj<-cbind(tauijcol,percj,dfj,pvaltauijcol)
    zjtot<-c(apply(zj[,1:3],2,sum),pvalijtot)
    zj<-rbind(zj,zjtot)
    zij<-rbind(zi,zj)
    nomi<-paste("poly-row",0:(ni-1),sep="")
    nomj<-paste("poly-col",0:(nj-1),sep="")
    dimnames(zij)<-list(c(nomi,"Tau-IJ",nomj,"Tau-IJ"),c("Term-IJ-poly","%inertia","df","p-value"))
    zij<-round(zij,digits=digits)
    #################################################partial term IK
    fik <- (pik - p2ik)/sqrt(p1k)
   # z3par<-   t(poli[,-1])%*%fik%*%polk[,-1]
    z3par<-   t(poli)%*%fik%*%polk
          tauik<-(sum(z3par^2)/devt)*(ni-1)*(n-1)
pvaliktot<-1 - pchisq(tauik, (ni-1)*(nk-1))
    z3ik<-  (z3par^2/devt)*(ni-1)*(n-1)
    tauikcol<-apply(z3ik,2,sum)
    tauikrow<-apply(z3ik,1,sum)
   #---
pval<-c()
    for (i in 1:(ni)){
      pval[i]<-1 - pchisq(tauikrow[i], nk-1)
    }
    pvaltauikrow<-pval
    pval<-c()
    for (i in 1:(nk)){
      pval[i]<-1 - pchisq(tauikcol[i], ni-1)
    }
    pvaltauikcol<-pval
     #row and tube poly component
    dfi<-rep((nk-1),(ni))
    perci<-tauikrow/tauik*100
    zi<-cbind(tauikrow,perci,dfi,pvaltauikrow)
    zitot<-c(apply(zi[,1:3],2,sum),pvaliktot)
    zi<-rbind(zi,zitot)
    dfk<-rep((ni-1),(nk))
    perck<-tauikcol/tauik*100
    zk<-cbind(tauikcol,perck,dfk,pvaltauikcol)
    zktot<-c(apply(zk[,1:3],2,sum),pvaliktot)
    zk<-rbind(zk,zktot)
    zik<-rbind(zi,zk)
    nomi<-paste("poly-row",0:(ni-1),sep="")
    nomk<-paste("poly-tube",0:(nk-1),sep="")
    dimnames(zik)<-list(c(nomi,"Tau-IK",nomk,"Tau-IK"),c("Term-IK-poly","%inertia","df","p-value"))
    zik<-round(zik,digits=digits)
    ################################################partial term JK
    fjk <- (pjk - p2jk)/sqrt(p2jk)
    z3par<-   t(polj)%*%fjk%*%polk
       z3jk<- 1/ni * (z3par^2/devt)*(ni-1)*(n-1)
    taujk=(sum(z3par^2)/devt)*(ni-1)*(n-1)
pvaljktot<-1 - pchisq(taujk, (nj-1)*(nk-1))
    taujkcol<-apply(z3jk,2,sum)
    taujkrow<-apply(z3jk,1,sum)
#---
pval<-c()
    for (i in 1:(nj)){
      pval[i]<-1 - pchisq(taujkrow[i], nk-1)
    }
    pvaltaujkrow<-pval
    pval<-c()
    for (i in 1:(nk)){
      pval[i]<-1 - pchisq(taujkcol[i], nj-1)
    }
    pvaltaujkcol<-pval
      #row and column poly component
    dfj<-rep((nk-1),(nj))
    percj<-taujkrow/taujk*100
    zj<-cbind(taujkrow,percj,dfj,pvaltaujkrow)
    zjtot<-c(apply(zj[,1:3],2,sum),pvaljktot)
    zj<-rbind(zj,zjtot)
    dfk<-rep((nj-1),(nk))
    perck<-taujkcol/taujk*100
    zk<-cbind(taujkcol,perck,dfk,pvaltaujkcol)
    zktot<-c(apply(zk[,1:3],2,sum),pvaljktot)
    zk<-rbind(zk,zktot)
    zjk<-rbind(zj,zk)
    nomj<-paste("poly-col",0:(nj-1),sep="")
    nomk<-paste("poly-tube",0:(nk-1),sep="")
    dimnames(zjk)<-list(c(nomj,"Tau-JK",nomk,"Tau-JK"),c("Term-JK-poly","%inertia","df","p-value"))
    zjk<-round(zjk,digits=digits)
    #################################three ordered variables and interaction term
    fijk=(p3 - pijk)/sqrt(p1jk)
#browser()
p2ijk<-p2ij%o%pk
p2ikj<-p1k%o%pj
p2ikj<-aperm(p2ikj,c(1,3,2))
p2jki<-p2jk%o%ui
p2jki<-aperm(p2jki,c(3,1,2))
#z3n<- sqrt((ni-1)*(n-1))* t(poli[,-1])%*%flatten(fijk)%*%Kron(polj[,-1],polk[,-1])/sqrt(devt)
    z3n<-sqrt((ni-1)*(n-1))*  ( t(poli)%*%flatten(fijk)%*%Kron(polj,polk))/sqrt(devt)
    tauint<-(z3n^2)
dim(tauint)<-c(ni,nj,nk)      
#dim(tauint)<-c(ni-1,nj-1,nk-1)      
tautot<-(sum(z3n^2)/devt)*(ni-1)*(n-1)
pvalinttot<-1 - pchisq(tautot, (ni-1)*(nj-1)*(nk-1))
#browser()
       tauintrow<-apply(tauint,1,sum)
          dfi<-rep((nj-1)*(nk-1),(ni))
#          dfi<-rep((nj-1)*(nk-1),(ni-1))
    pvaltauintrow<-1 - pchisq(tauintrow, ni-1)
    #----------------------------------
    tauintcol<-apply(tauint,2,sum)
    pvaltauintcol<-1 - pchisq(tauintcol, nj-1)
    #-----------------------------------
    tauinttub<-apply(tauint,3,sum)
    pvaltauinttub<-1 - pchisq(tauinttub, nk-1)
    #-----------------------------------------
    #row column and tube poly component on three-way intereraction term
    perci<-tauintrow/tautot*100
    zi<-cbind(tauintrow,perci,dfi,pvaltauintrow)
    zitot<-c(apply(zi[,1:3],2,sum),pvalinttot)
    zi<-rbind(zi,zitot)
    #----------
    dfj<-rep((ni-1)*(nk-1),(nj))
#    dfj<-rep((ni-1)*(nk-1),(nj-1))
    percj<-tauintcol/tautot*100
    zj<-cbind(tauintcol,percj,dfj,pvaltauintcol)
    zjtot<-c(apply(zj[,1:3],2,sum),pvalinttot)
    zj<-rbind(zj,zjtot)
    #--------
#    dfk<-rep((ni-1)*(nj-1),(nk-1))
    dfk<-rep((ni-1)*(nj-1),(nk))
    perck<-tauinttub/tautot*100
    zk<-cbind(tauinttub,perck,dfk,pvaltauinttub)
    zktot<-c(apply(zk[,1:3],2,sum),pvalinttot)
    zk<-rbind(zk,zktot)
    zijk<-rbind(zi,zj,zk)
#    nomi<-paste("poly",1:(ni-1),sep="")
#    nomj<-paste("poly",1:(nj-1),sep="")
#    nomk<-paste("poly",1:(nk-1),sep="")
    nomi<-paste("poly-row",0:(ni-1),sep="")
    nomj<-paste("poly-col",0:(nj-1),sep="")
    nomk<-paste("poly-tube",0:(nk-1),sep="")
    #browser()
    dimnames(zijk)<-list(c(nomi, "Tau-IJK",nomj,"Tau-IJK",nomk,"Tau-IJK"),c("Term-IJK-poly","%inertia","df","p-value"))
    zijk<-round(zijk,digits=digits)
    #============================================================
    return(list(z=z,zij=zij,zik=zik,zjk=zjk,zijk=zijk,pij=pij,pik=pik,pjk=pjk,cost=cost))
  }

