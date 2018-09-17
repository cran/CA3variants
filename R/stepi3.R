stepi3 <-
function(param){
    xp <- aperm(param$x, c(2, 3, 1))
    gp <- aperm(param$g, c(2, 3, 1))
    paramp <- list(a = param$b, b = param$cc, cc = param$a, g = gp, x = xp)
    newcomp3(paramp)
}
