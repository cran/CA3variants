coord <- function(res, x){

     x <- x/sum(x)
     pi <- margI(x)
     pj <- margJ(x)
     pk <- margK(x)
     res$a <- diag(1/sqrt(pi)) %*% res$a	
     res$b <- diag(1/sqrt(pj)) %*% res$b
     res$cc <- diag(1/sqrt(pk)) %*% res$cc
     res
}
