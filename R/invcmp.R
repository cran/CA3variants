invcmp <-
function(comp,nr,nc,chgcomp){
k=nr*(chgcomp-1)+1
for (i in 1:nr){
comp[k]= (-1)*comp[k]
k=k+1}
list(comp=comp,chgcomp=chgcomp)
}
