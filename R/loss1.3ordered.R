loss1.3ordered<-function(param, comp.old)
{
#calcul de la fonction de perte generale (difference entre x et xhat)
#cat("dentro loss1.3ordered\n")
	xhat <- reconst3(param)
#	criterordered(param$x, xhat)
criter(param$x, xhat)
}
