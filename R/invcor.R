invcor <-
function(core,p,q,r,chgmode,chgcomp){
#to change the sign of the elements of core slice
#chgcomp of mode slice of the core matrix
#case 1 horizontal slice
if (chgmode==1){
k=chgcomp
for (i in 1:q*r)
core[k]=(-1)*core[k]
k=k+p}
#case 2 lateral slice
if (chgmode==2){
k=(chgcomp-1)*p+1
for (i in 1:p*r){
#for (i in 1:r){
#for (j in 1:p){
core[k]=(-1)*core[k]
k=k+1
#}
#k=k-p+p*q
}}
#case 3 frontal slice
if (chgmode==3){
k=(chgcomp-1)*p*q+1
for (i in 1:p*q){
core[k]=(-1)*core[k]
k=k+1}}
#list(core=core,chgmode=chgmode,chgcomp=chgcomp)
list(core=core)
}

