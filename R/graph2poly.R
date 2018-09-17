graph2poly <-
function(f,g,ni,nj,nk,a1=1,a2=2, prop=.8,cex = 1,
 length=0.01,  cex.lab = 0.8, cols = c(1, 4),
 mar = c(5, 4, 4, 2) + 0.1,arrow=F,type="l",pos=2)
{
#------------------------------------------------------------------------------
# f and g are the row and column coordinates
#------------------------------------------------------------------------------
Inames<-dimnames(f)[[1]]

Jnames<-dimnames(g)[[1]]
picsize<-c(range(f[,1], f[,2], g[,1], g[,2])/prop)
nrows<-dim(f)[[1]]
ncols<-dim(g)[[1]]
    par(pty = "s", mar = mar)
plot(0, 0, xlim=picsize, ylim=picsize, 
     xlab=paste("Polynomial Axis ", a1), 
     ylab=paste("Polynomial Axis ", a2),  
asp=1, pch="", col=4,cex=cex,type=type)
nv <- rep(0, nrow(f))
#vec <- f[, c(a1, a2)]
ng <- rep(0, nrow(g))

if(arrow) {

#arrows(nv, nv, f[, a1], f[, a2], length = length)
arrows(ng, ng, g[, a1], g[, a2], length = length)

}
text(f[, a1], f[, a2], labels = Inames, adj = 0, col = cols[1], 
cex = cex,cex.lab=cex.lab,pos=pos)
points(f[, a1], f[, a2], pch="*",col=cols[1])
k=1
for (i in 1:nk){
lines(g[k:(nj*i), a1], g[k:(nj*i), a2], pch="+",col=i+1,type=type)
#col=i+1,type=type)
#break
text(g[k:(nj*i), a1], g[k:(nj*i), a2], labels = Jnames[k:(nj*i)], adj = 1, col = 1+i, 
cex = cex,cex.lab=cex.lab,pos=pos)
points(g[k:(nj*i), a1], g[k:(nj*i), a2], pch="+",col=1+i)
k=k+nj
cat("trajectory\n",i,"\n")

}
#points(g[, a1], g[, a2], pch="+",col=cols[2])
#text(g[, a1], g[, a2], labels = Jnames, adj = 1, col = cols[2], 
#cex = cex,cex.lab=cex.lab,pos=pos)

     abline(h = 0, v = 0)

}
