srtcor <-
function(core,p,q,r)
{#give core pointers 
#coreptr=array(sort(abs(core),index.return=T,decreasing=T)$ix,c(p,q,r)) #pointers to the greatest absolute value in core
coreptr=sort(abs(core),index.return=T,decreasing=T)$ix #pointers to the greatest absolute value in core
#coreord=array(sort(abs(core),index.return=T,decreasing=T)$x,c(p,q,r))
list(coreptr=coreptr)
}
