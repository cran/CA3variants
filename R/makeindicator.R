makeindicator<-function(X){

# Input a three-way table as can be used in CA3variants
# Output the N x total number of categories (rows+cols+tubs) indicator matrix

N<-sum(X)
rows <- dim(X)[1]
cols <- dim(X)[2]
tubs <- dim(X)[3]
margtubs<-apply(X,3,sum) 

Z<-NULL  # Create empty and start making the tubes:
for (k in 1:tubs){
  Tk<-X[,,k]
  n<-sum(Tk)    # Number of observation. Can be replaced by margtubs[k]
  
  for (i in 1:rows) {
    
    for (j in 1:cols) {
      
      rep<-Tk[i,j]    # Number of observations in cell ij of tube k
      if (rep>0) {
        for (r in 1:rep) {
          
          z<-matrix(0,1,rows+cols+tubs)    # Make a 1 x (total # categories) vector of zeros 
          z[i]<-1                          # column i corresponds to row i of tube k: the row category
          z[rows+j]<-1                     # column rows+j corresponds to column j of tube k: the column category
          z[rows+cols+k]<-1                # column rows+cols+k corresponds to tube k: the tube category
          Z<-rbind(Z,z)}
      }
    }
  }
}
return(Z)
}