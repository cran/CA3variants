simulabootsimple<-function(Xtable,nboots=100,resamptype=1){
# BOOTSTRAPPING for three-way contingency table
#Xtable <- read.table(file = "Suicidedata.txt", header=TRUE)
#Xtable<-happy
#nboots=3
#resamptype=1
X <- as.array(Xtable)
Xf<-flatten(X)
rows <- dim(X)[1]
cols <- dim(X)[2]
tubs <- dim(X)[3]
n <- sum(X)

# Number of bootstrap replicates
# set value of resamptype according to method
# resamptype 1 = multinomial
# resamptype 2 = Poisson

XB<-Xslice<-margI<-margJ<-margK<-list()

for (b in 1:nboots) { 
# vary if mntype not 0 or poisszero,poissone not 0

if (resamptype==1) Xr <- rmultinom(1,n,Xf) else Xr <- rpois(rows*cols,Xf)
XB[[b]]<-array(unlist(Xr),c(rows,cols,tubs))  

}#end bootstrap

XmargI<-apply(X/sum(X),1,sum)
XmargJ<-apply(X/sum(X),2,sum)
XmargK<-apply(X/sum(X),3,sum)
list(XB,XmargI,XmargJ,XmargK)
return(XB)
}