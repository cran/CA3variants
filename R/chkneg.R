chkneg<-
function(comp,nr,nc)
#    Subroutine to check the negativity of the column of an #array
#!     AND the positivity of the columns of an array
#!     If NegPtr = 1 then there is an entirely negative #component
#!     If PosPtr = 1 then there is an entirely positive #component
#!     If BigPtr = 1 then maximum neg. abs > max pos
#!---------------------------
{
#posptr=NULL
#negptr=NULL
#bigptr=NULL
posptr=1
negptr=1
bigptr=1
ic=0
for (j in 1:nc){
posbig=0
negbig=0
for (i in 1:nr){
ic=ic+1
posind=which(comp[i,j]>0)
negind=which(comp[i,j]<0)
if ((comp[i,j]>=0)){negptr[j]=0}
if ((comp[i,j]>posbig)) {posbig=comp[i,j]}
#if ((comp[ic]>=0)){negptr[j]=0}
#if ((comp[ic]>posbig)) {posbig=comp[ic]}
else {posptr[j]=0}
#if ((comp[ic]<negbig)) {negbig=comp[ic]}
if ((comp[i,j]<negbig)) {negbig=comp[i,j]}
#}
else {negptr[j]=0}
}#end for
if (length(posind)>length(negind)) {bigptr[j]=0}
bigptr[j]=1
}
#print(posptr)
#print(negptr)
#print(bigptr)
list(posptr=posptr,negptr=negptr,bigptr=bigptr)
}

