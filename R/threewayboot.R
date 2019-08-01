threewayboot<-function(Xdata,nboots=100){

# Do nboots bootstrap on the observations x rows+cols+tubs categories indicator matrix X.
# If X is a three way table first make the indicator using makeindicator.
# The output is a list of threeway tables
 rows <- dim(Xdata)[[1]]
  cols <- dim(Xdata)[[2]]
  tubs <- dim(Xdata)[[3]]
Z<-makeindicator(Xdata)
N<-nrow(Z)

margtubs<-colSums(Z[,(rows+cols+1):(rows+cols+tubs)]) 
ZB<-Xslices<-XB<-list()

for (b in 1:nboots) {
  ZB<-Z[sample(N,replace=TRUE),]
  ind<-0
  for (k in 1:tubs){
    ZB1<-ZB[(ind+1):(ind+margtubs[k]),1:rows]
    ZB2<-ZB[(ind+1):(ind+margtubs[k]),(rows+1):(rows+cols)]
    Xslices[[k]] <-t(ZB1) %*% ZB2
    ind<-ind+margtubs[k]
  }
  
  XB[[b]]<-array(unlist(Xslices),c(rows,cols,tubs))
  
}

return(XB)}