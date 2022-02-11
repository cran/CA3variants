selmod <-
function(aptr,bptr,cptr,posptrA,negptrA,bigptrA,posptrB,negptrB,bigptrB,posptrC,negptrC,bigptrC,
IA,IB,IC,I,J,K,p,q,r,longest)
{
#-------------------------------------------------------------------
#ai, bi cci column of a component matrix
#
#IA, IB and IC pointers to the components associated with the greatest (in absolute value) core elements  
#!------------------------------------------------------------------------------------
#!   Select the mode in which the column has to be sign reversed.
#!   Below is an heuristic algorithm but a fully rational choice is hard to come by.
#!
#!   1,1,1 is always the largest element.
#!
#!   maximal number of sign reversals = p+q+r-2, but this number can be much smaller
#!
#!         *sign reverse a component
#!          determine which if any of s, t and u is available for reversal
#!          if one of them is wholly positieve way
#!             never choose it
#!          else if one is wholly negative
#!             choose that one from  A, B, C respectively
#!          else if there is a component with a largest absolute value which is negative
#!             choose that one, or the one from A,B,C in that order
#!          else
#!             choose the column of the longest mode
#!          end if
#!
#!   FreeA,FreeB,FreeC = 0 component is not available; = 1 component is available
#!------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
freeA=0
freeB=0
freeC=0
chgmode=0
chgcomp=0
success=0
#cat("longest is\n")
#print(longest)
#1. check if components are available for sign inversion
if (aptr==1) freeA=1
if (bptr==1) freeB=1
if (cptr==1) freeC=1
if ((freeA+freeB+freeC)==0) {success=0} #no components available to be reversed
if ((freeA+freeB+freeC)==1) {###success=1
if (freeA==1)
{chgmode=1
chgcomp=IA
#success=1
}
if (freeB==1)
{chgmode=2
chgcomp=IB
#success=1
}
if (freeC==1)
{chgmode=3
chgcomp=IC
#success=1
}
success=1
}#endif
#2. check whether there are only positive components
if (posptrA==1) freeA=0
if (posptrB==1) freeB=0
if (posptrC==1) freeC=0
if ((freeA+freeB+freeC)==0) {success=0} #no components available to be reversed
if ((freeA+freeB+freeC)==1) {#success=1-in pieter's goto999
if (freeA==1)
{chgmode=1
chgcomp=IA
#success=1
}
if (freeB==1)
{chgmode=2
chgcomp=IB
#success=1
}
if (freeC==1)
{chgmode=3
chgcomp=IC
#success=1
}
success=1
}#endif 
#3. check whether there are only negative components
free2A=freeA*negptrA
free2B=freeB*negptrB
free2C=freeC*negptrC
if ((free2A+free2B+free2C)==0) {
freeA=0
freeB=0
freeC=0
success=0}
if ((free2A+free2B+free2C)==1) 
{freeA=free2A
freeB=free2B
freeC=free2C
####success=1
if (freeA==1)
{chgmode=1
chgcomp=IA
#success=1
}
if (freeB==1)
{chgmode=2
chgcomp=IB
#success=1
}
if (freeC==1)
{chgmode=3
chgcomp=IC
#success=1
}
success=1
}#endif
if ((free2A+free2B+free2C)>1) {
if (free2A==1){
freeA=1
freeB=0
freeC=0}
 if (free2B==1){
freeA=0
freeB=1
freeC=0}
if (free2C==1){
freeA=0
freeB=0
freeC=1}
###success=1
if (freeA==1)
{chgmode=1
chgcomp=IA
#success=1
}
if (freeB==1)
{chgmode=2
chgcomp=IB
#success=1
}
if (freeC==1)
{chgmode=3
chgcomp=IC
#success=1
}
success=1
}#endif
#4. no negative components, check whether there is one with a larger negative absolute value than a positive one
free2A=freeA*bigptrA
free2B=freeB*bigptrB
free2C=freeC*bigptrC
if ((free2A+free2B+free2C)==0) {
freeA=0
freeB=0
freeC=0}
if ((free2A+free2B+free2C)==1){
freeA=free2A
freeB=free2B
freeC=free2C
######success=1
if (freeA==1)
{chgmode=1
chgcomp=IA
#success=1
}
if (freeB==1)
{chgmode=2
chgcomp=IB
#success=1
}
if (freeC==1)
{chgmode=3
chgcomp=IC
#success=1
}
success=1
}#endif
if ((free2A+free2B+free2C)>1) {
if (free2C==1) 
{freeA=0
freeB=0
freeC=1}
if (free2B==1){
freeA=0
freeB=1
freeC=0}
if (free2A==1){
freeA=1
freeB=0
freeC=0
}
#####999 success=1
if (freeA==1)
{chgmode=1
chgcomp=IA
#success=1
}
if (freeB==1)
{chgmode=2
chgcomp=IB
#success=1
}
if (freeC==1)
{chgmode=3
chgcomp=IC
#success=1
}
success=1
}#endif
#
#5. no special reason for selection could be found, choose components of longest mode to be inverted
if (freeA==1){
if (freeB==1){
if (freeC==1){
if (longest==1){
freeA=1
freeB=0
freeC=0
}
if (longest==2){
freeA=0
freeB=1
freeC=0}
else{
freeA=0
freeB=0
freeC=1}
if (I>=J)
{freeA=1
freeB=0
freeC=0
}
else{
freeA=0
freeB=1
freeC=0
}
if (I>=K)
{
freeA=1
freeB=0
freeC=0
}
else {
freeA=0
freeB=0
freeC=1
}
if (J>=K)
{freeA=0
freeB=1
freeC=0
}
else{
freeA=0
freeB=0
freeC=1
}
}#end if freeA
}#end if freeB
}#end if freeC
#-----------------------
list(success=success,chgmode=chgmode,chgcomp=chgcomp)
}

