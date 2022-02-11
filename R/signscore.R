signscore <-
function(a,b,cc,I,J,K,p,q,r,core,IFIXA,IFIXB,IFIXC){
aptr=1
bptr=1
cptr=1
if (IFIXA==1) aptr=0
if (IFIXB==1) bptr=0
if (IFIXC==1) cptr=0
if (I==max(I,J,K)) longest=1
if (J==max(I,J,K)) longest=2
if (K==max(I,J,K)) longest=3
ressrtcor=srtcor(core,p,q,r)
coreord=core[ressrtcor$coreptr]
dim(coreord)=dim(core)
####-----------------------sort core check neg and pos and flag 
achk=chkneg(a,nr=I,nc=p)
#print(achk)
bchk=chkneg(b,nr=J,nc=q)
cchk=chkneg(cc,nr=K,nc=r)
for (i in 1:p*q*r){
rescrptrs=crptrs(ressrtcor$coreptr[i],p,q)
aord=unique(a[,crptrs(ressrtcor$coreptr,p,q)$IA],MARGIN=2) #ordered component a
bord=unique(b[,crptrs(ressrtcor$coreptr,p,q)$IB],MARGIN=2) #ordered component b
ccord=unique(cc[,crptrs(ressrtcor$coreptr,p,q)$IC],MARGIN=2) #ordered component cc
if (core[ressrtcor$coreptr[i]]<0){
rselmod=selmod(aptr=1, bptr=1, cptr=1, posptrA=achk$posptr[rescrptrs$IA],negptrA=achk$negptr[rescrptrs$IA],bigptrA=achk$bigptr[rescrptrs$IA],posptrB=bchk$posptr[rescrptrs$IB],negptrB=bchk$negptr[rescrptrs$IB],bigptrB=bchk$bigptr[rescrptrs$IB],posptrC=cchk$posptr[rescrptrs$IC],negptrC=cchk$negptr[rescrptrs$IC],bigptrC=cchk$bigptr[rescrptrs$IC],
IA=rescrptrs$IA,IB=rescrptrs$IB,IC=rescrptrs$IC,I=I,J=J,K=K,p=p,q=q,r=r,longest=longest)
if (rselmod$success==1)
{
rinvcore=invcor(core,p,q,r,chgmode=rselmod$chgmode,chgcomp=rselmod$chgcomp)
if (rselmod$chgmode==1) {comp=a
nr=I
nc=p}
if (rselmod$chgmode==2) {comp=b
nr=J
nc=q}
if (rselmod$chgmode==3) {comp=cc
nr=K
nc=r}
rinvcmp=invcmp(comp,nr,nc,chgcomp=rselmod$chgcomp)
}#end if success
if (rselmod$chgmode==1) a=rinvcmp$comp
if (rselmod$chgmode==2) b=rinvcmp$comp
if (rselmod$chgmode==3) cc=rinvcmp$comp
}#end if core
aptr[rescrptrs$IA]=0
bptr[rescrptrs$IB]=0
cptr[rescrptrs$IC]=0
#if (sum(aptr)+sum(bptr)+sum(cptr)==0) 
}#end for
list(g=core,gord=coreord,a=a,aord=aord,b=b,bord=bord,cc=cc,ccord=ccord)
}