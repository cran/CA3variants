tunelocal<- function(Xdata, ca3type = "CA3", resp="row", norder = 3, digits = 3, boots = FALSE, 
          nboots = 0, boottype= "bootpsimple", resamptype = 1, PercentageFit = 0.01) 
{
#-------------------------------------------------------------------------------------------------------------------------------------
# when boot = T it does bootstrap sampling. There are three kinds of possible bootstrap 
# sampling. When boottype = "bootnp" it is non parametric bootstrap sampling
# When boottype = "bootpsimple" it is  parametric (multinomial or poisson) simple bootstrap
#  sampling 
# When boottype = "bootpstrat" it is parametric stratified bootstrap sampling  
#  library(multichull)
#  library(CA3variants)
#  source("simula-bootstrap-simpleSamp.R")
#  source("simulaboot.R")  
#   ca3type="CA3"
#   norder=2
#   boots=T
#   boottype="bootpsimple" #bootnp bootpsimple bootpstrat
#   nboots<-2
#   Xdata<-happy
#   PercentageFit = 0.01, used in CHull function
#-----------------------------------------------------------------------------------------------------------------------------------------
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
  output1 <- NULL
    nran <- sum(Xdata)
  risCArss <- RSScrit <- list()
  risCAgof <- CAgof <- list()
  risCAchi2 <- list()
  risCAfp <- risCAdf <- dfmodel <- CAinertia <- list()
  fpmodel <- dimmodel <- list()
      if (boots == T) {
if (nboots==0) {
nboots<-100
cat("Note that 'nboots' by default is 100\n You can change the parameter in input of 'tunelocal' function \n")
cat("Also, note that the number of different models that is assessed is based on the size of all models obtained from the combination of dimensions of the bootstrapped data\n 
For example, for a 4 x 5 x 4 array, there are 800 different models that are assessed.\n  \n")}
      Xrand <- switch(boottype, "bootnp" = threewayboot(Xdata,nboots=nboots),  "bootpsimple" = 
                  simulabootsimple(Xdata, nboots=nboots, resamptype = resamptype),"bootpstrat" =simulabootstrat(Xdata, nboots=nboots, resamptype = resamptype) )
    XG[[1]]<-Xdata
 dimnames(XG[[1]]) <- dimnames(Xdata)
           for (b in 1:nboots) {
XG[[b+1]]<-Xrand[[b]]           
 dimnames(XG[[b ]]) <- dimnames(Xdata)
    }
  }

else{
nboots<-0
cat("Note that the number of different models that is assessed is based on the size of the original data being analysed.\n 
For example, for a 4 x 5 x 4, there are 80 different models that are assessed.\n")
XG<-list(Xdata)
}
  nsets <- nboots + 1
  for (b in 1:nsets) {
    ng <- 0
    for (i in 1:rows) {
      for (j in 1:cols) {
        for (k in 1:tubs) {
          ng <- ng + 1
            res <- CA3variants(XG[[b]], dims=c(p= i, q= j,  r =k), 
                            ca3type = ca3type, resp=resp, norder = norder, sign = FALSE)
                   risCAchi2[[ng]] <- res$inertiatot
          risCAdf[[ng]] <- ((i - 1) * (j - 1) * (k - 
                                                   1) + (i - 1) * (j - 1) + (i - 1) * (k - 1) + 
                              (j - 1) * (k - 1))
                   dimmodel[[ng]] <- c(i, j, k)
        }
      }
    }
    CAinertia[[b]] <- matrix(unlist(risCAchi2), ng, 1)
    dfmodel[[b]] <- matrix(unlist(risCAdf), ng, 1)
    }
  dflabel <- matrix(unlist(as.character(dimmodel)), ng, 1)
 dimmod <- dimmodel  #newnew
  CHIcritot <- matrix(unlist(CAinertia), ng, nsets)
  dftot <- matrix(unlist(dfmodel), ng, nsets)
  if (boots == TRUE) {
    CHI_allrandom <- CHIcritot[, 2:nsets]
    CHI_random <- as.matrix(rowMeans(CHIcritot[, 2:nsets]))
    CHIplot <- cbind(dftot[, 1], CHIcritot[, 1], CHI_random)
    CHIord <- CHIplot[order(CHIplot[, 1]), ]
    dimnames(CHIord) <- list(paste(dflabel[order(CHIplot[, 
                                                         1])]), c("df", "CHI_data", "CHI_Random"))
    CHIord2 <- cbind(CHIord[, 1], CHIord[, 3])
  }
  CHIplot <- cbind(dftot[, 1], CHIcritot[, 1])
  CHIord <- CHIplot[order(CHIplot[, 1]), ]
  dimnames(CHIord) <- list(paste(dflabel[order(CHIplot[, 1])]), 
                           c("df", "CHI_data"))
  output1 <- CHull(CHIord, bound = "upper", PercentageFit = PercentageFit)
 #plot(output1, type = "b")
  #title(sub="Original Data -Chi2 criterion-")
  if (boots == TRUE) {
    output1 <- CHull(CHIord2, bound = "upper", PercentageFit = PercentageFit)
 #plot(output1, type = "b")
  #title(sub="Bootstrap Data -Chi2 criterion-")
  }
  #CHIord <- round(CHIord, digits = 3)
#---------------------export
# restunelocal<-list(XG = XG, risCAchi2 = risCAchi2, risCAdf = risCAdf, output1 = output1, ca3type = ca3type)
restunelocal<-list(XG = XG, output1 = output1, ca3type = ca3type, boots = boots)
class(restunelocal)<-"tunelocal"
invisible(return(restunelocal))

}
