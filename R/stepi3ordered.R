stepi3ordered<-function (param) 
{
    xp <- aperm(param$x, c(2, 3, 1))
    gp <- aperm(param$g, c(2, 3, 1))
    paramp <- list(a = param$b, b = param$cc, cc = param$a, g = gp, 
        x = xp,pii=param$pj,pj=param$pk,pk=param$pii)
#browser()
    newcomp3ordered(paramp)
}
