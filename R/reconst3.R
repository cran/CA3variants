reconst3 <-
function(param){
    
    a <- param$a
    b <- param$b
    cc <- param$cc
    g <- param$g
    pqr <- dim(g)
    y <- Kron(b, cc)    #              dim(y) is (JxK)x(qxr)
    gf <- flatten(g)	#              dim(gf) is p x (qxr)      
    y <- y %*% t(gf)	#              dim(y)  is (JxK) x p 
    z <- p.ext(a, y)
    xhat <- apply(z, 1, sum)
    dim(xhat) <- c(dim(a)[1], dim(b)[1], dim(cc)[1])
    xhat
}
