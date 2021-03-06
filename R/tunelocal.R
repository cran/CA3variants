tunelocal<-function (Xdata, ca3type = "CA3", resp="row", norder = 3, digits = 3, boots = FALSE, 
          nboots = 0, boottype= "bootpsimple", resamptype = 1) 
{
#when boot = T it does bootstrap sampling. There are three kinds of possible bootstrap 
# sampling. When boottype = "bootnp" it is non parametric bootstrap sampling
#When boottype = "bootpsimple" it is  parametric (multinomial or poisson) simple bootstrap
#  sampling 
#When boottype = "bootpstrat" it is parametric stratified bootstrap sampling  
#  library(multichull)
#  library(CA3variants)
#source("simula-bootstrap-simpleSamp.R")
  #source("simulaboot.R")  
 #   ca3type="CA3"
 # norder=2
 # boots=T
 # boottype="bootpsimple" #bootnp bootpsimple bootpstrat
 # nboots<-2
 # Xdata<-happy
#----------------------------------
if (is.array(Xdata)==FALSE){
  #Xtable=dget("Xdata.txt")
Xdata1<-Xdata  
  nam<-names(Xdata1)
ndims<-dim(Xdata1)[[2]]
if (ndims<3) {stop("number of variables for output must be at least 3\n\n")}
Xdata2=table(Xdata1[[1]],Xdata1[[2]],Xdata1[[3]])
  if(is.numeric(Xdata1)==TRUE){  
dimnames(Xdata2)=list(paste(nam[[1]],1:max(Xdata[[1]]),sep=""),paste(nam[[2]],1:max(Xdata[[2]]),sep=""),paste(nam[[3]],1:max(Xdata[[3]]),sep=""))
}  
#paste("This is your given array\n\n")
  Xdata<-Xdata2
#print(Xdata)
  }

#-----
    rows <- dim(Xdata)[[1]]
  cols <- dim(Xdata)[[2]]
  tubs <- dim(Xdata)[[3]]
  pi <- apply(Xdata/sum(Xdata), 1, sum)
  pj <- apply(Xdata/sum(Xdata), 2, sum)
  pk <- apply(Xdata/sum(Xdata), 3, sum)
  I <- rows
  J <- cols
  K <- tubs
  XG <- Xrand<-list()
  output1 <- output2 <- output3 <- output4 <- output5 <- output6 <- NULL
    nran <- sum(Xdata)
  risCArss <- RSScrit <- list()
  risCAgof <- CAgof <- list()
  risCAchi2 <- list()
  risCAfp <- risCAdf <- dfmodel <- CAinertia <- list()
  fpmodel <- dimmodel <- list()
      if (boots == T) {
      Xrand <- switch(boottype, "bootnp" = threewayboot(Xdata,nboots=nboots),  "bootpsimple" = 
                  simulabootsimple(Xdata, nboots=nboots, resamptype = resamptype),"bootpstrat" =simulabootstrat(Xdata, nboots=nboots, resamptype = resamptype) )
    XG[[1]]<-Xdata
           for (b in 1:nboots) {
#XG[[b ]]<-array(unlist(Xrand[[b]]),c(rows,cols,tubs))
XG[[b+1]]<-Xrand[[b]]           
 dimnames(XG[[b ]]) <- dimnames(Xdata)

    }
  }

else{
XG<-list(Xdata)
}

#print(XG)
#browser()
#stop()
  nsets <- nboots + 1
  for (b in 1:nsets) {
    ng <- 0
    for (i in 1:rows) {
      for (j in 1:cols) {
        for (k in 1:tubs) {
          ng <- ng + 1
          res <- CA3variants(XG[[b]], dims=c(p= i, q= j,  r =k), 
                             ca3type = ca3type, resp=resp, norder = norder)

         # risCArss[[ng]] <- res$inertiaRSS
          risCAgof[[ng]] <- sum(res$inertiapc) #percentage of explained inertia
          risCAchi2[[ng]] <- res$inertiatot
          risCAdf[[ng]] <- ((i - 1) * (j - 1) * (k - 
                                                   1) + (i - 1) * (j - 1) + (i - 1) * (k - 1) + 
                              (j - 1) * (k - 1))
          risCAfp[[ng]] <- i + j + k - 2
          dimmodel[[ng]] <- c(i, j, k)
        }
      }
    }
  #  RSScrit[[b]] <- matrix(unlist(risCArss), ng, 1)
    CAinertia[[b]] <- matrix(unlist(risCAchi2), ng, 1)
    CAgof[[b]] <- matrix(unlist(risCAgof), ng, 1)
    dfmodel[[b]] <- matrix(unlist(risCAdf), ng, 1)
    fpmodel[[b]] <- matrix(unlist(risCAfp), ng, 1)
  }
  dflabel <- matrix(unlist(as.character(dimmodel)), ng, 1)
 # RSScritot <- matrix(unlist(RSScrit), ng, nsets)
  CHIcritot <- matrix(unlist(CAinertia), ng, nsets)
  GOFcritot <- matrix(unlist(CAgof), ng, nsets)
  dftot <- matrix(unlist(dfmodel), ng, nsets)
  fptot <- matrix(unlist(fpmodel), ng, nsets)
  if (boots == TRUE) {
  #  RSS_allrandom <- RSScritot[, 2:nsets]
    CHI_allrandom <- CHIcritot[, 2:nsets]
    GOF_allrandom <- GOFcritot[, 2:nsets]
   # RSS_random <- as.matrix(rowMeans(RSScritot[, 2:nsets]))
    CHI_random <- as.matrix(rowMeans(CHIcritot[, 2:nsets]))
    GOF_random <- as.matrix(rowMeans(GOFcritot[, 2:nsets]))
    #RSSplot <- cbind(dftot[, 1], RSScritot[, 1], RSS_random)
    CHIplot <- cbind(dftot[, 1], CHIcritot[, 1], CHI_random)
    GOFplot <- cbind(dftot[, 1], GOFcritot[, 1], GOF_random)
    #RSSord <- RSSplot[order(RSSplot[, 1]), ]
    CHIord <- CHIplot[order(CHIplot[, 1]), ]
    GOFord <- GOFplot[order(GOFplot[, 1]), ]
    #dimnames(RSSord) <- list(paste(dflabel[order(RSSplot[, 
    #                                                     1])]), c("df", "RSS_data", "RSS_Random"))
    dimnames(CHIord) <- list(paste(dflabel[order(CHIplot[, 
                                                         1])]), c("df", "CHI_data", "CHI_Random"))
    dimnames(GOFord) <- list(paste(dflabel[order(GOFplot[, 
                                                         1])]), c("df", "GOF_data", "GOF_Random"))
    #RSSord2 <- cbind(RSSord[, 1], RSSord[, 3])
    CHIord2 <- cbind(CHIord[, 1], CHIord[, 3])
    GOFord2 <- cbind(GOFord[, 1], GOFord[, 3])
  }
  #RSSplot <- cbind(dftot[, 1], RSScritot[, 1])
  CHIplot <- cbind(dftot[, 1], CHIcritot[, 1])
  GOFplot <- cbind(dftot[, 1], GOFcritot[, 1])
  #RSSord <- RSSplot[order(RSSplot[, 1]), ]
  CHIord <- CHIplot[order(CHIplot[, 1]), ]
  GOFord <- GOFplot[order(GOFplot[, 1]), ]
  #dimnames(RSSord) <- list(paste(dflabel[order(RSSplot[, 1])]), 
  #                         c("df", "RSS_data"))
  dimnames(CHIord) <- list(paste(dflabel[order(CHIplot[, 1])]), 
                           c("df", "CHI_data"))
  dimnames(GOFord) <- list(paste(dflabel[order(GOFplot[, 1])]), 
                           c("df", "GOF_data"))
  output1 <- CHull(CHIord, bound = "upper", PercentageFit = 0.001)
  plot(output1, type = "b")
  title(sub="Original Data-Chi2 criterion")
  #output3 <- CHull(RSSord, bound = "lower", PercentageFit = 0.001)
  #plot(output3, type = "b")
  #title(sub="Original Data-RSS criterion")
  #output5 <- CHull(GOFord, bound = "upper", PercentageFit = 0.001)
  #plot(output5, type = "b", ylim = c(0, 100))
  #title(sub="Original Data-GoF criterion")
  if (boots == TRUE) {
    output2 <- CHull(CHIord2, bound = "upper", PercentageFit = 0.001)
    plot(output2, type = "b")
    title(sub="Bootstrap Data-Chi2 criterion")
   # output4 <- CHull(RSSord2, bound = "lower", PercentageFit = 0.001)
   # plot(output4, type = "b")
   # title(sub="Bootstrap Data-RSS criterion")
   # output6 <- CHull(GOFord2, bound = "upper", PercentageFit = 0.001)
   # plot(output6, type = "b")
   # title(sub="Bootstrap Data-GoF criterion")
  }
  #RSSord <- round(RSSord, digits = 3)
  CHIord <- round(CHIord, digits = 3)
  GOFord <- round(CHIord, digits = 3)
  return(list(XG = XG, output1 = output1, output2 = output2))
}
