crptrs <-
function(icore,p,q,r){
#given the pointer to an element of icore, it calculates IA, IB,IC pointing to components
IC=as.integer(1+(icore-1)/(p*q))
IB= as.integer(1+((icore-1)-as.integer((icore-1)/(p*q))*(p*q))/p)
IA=1+((icore-1)-as.integer((icore-1)/p)*p)
list(IA=IA,IB=IB,IC=IC)
}
