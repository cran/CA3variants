simulabootstrat<-function(Xtable,nboots=100,resamptype=1){
# BOOTSTRAPPING for three-way contingency table
#Xtable <- read.table(file = "Suicidedata.txt", header=TRUE)
#Xtable<-happy
set.seed(1234)
X <- as.array(Xtable)
rows <- dim(X)[1]
cols <- dim(X)[2]
tubs <- dim(X)[3]
n<-c()
for (k in 1:tubs){
n[k] <- sum(X[,,k])
}
# Number of bootstrap replicates

nboots <- nboots

# set value of resamptype according to method
# resamptype 1 = multinomial
# resamptype 2 = Poisson

XB<-Xslice<-margI<-margJ<-margK<-list()

for (b in 1:nboots) { 
# vary if mntype not 0 or poisszero,poissone not 0
for(k in 1:tubs){
  
if (resamptype==1) Xr <- rmultinom(1,n[k],X[,,k]) else Xr <- rpois(rows*cols,X[,,k])
Xslice[[k]] <- matrix(data=Xr,nrow=rows,ncol=cols)
}#end array
XB[[b]]<-array(unlist(Xslice),c(rows,cols,tubs))  

}#end bootstrap

XmargI<-apply(X/sum(X),1,sum)
XmargJ<-apply(X/sum(X),2,sum)
XmargK<-apply(X/sum(X),3,sum)
#list(XB,XmargI,XmargJ,XmargK)
return(XB)
}