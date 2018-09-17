margJ <-
function(m)
{
n <- dim(m)
dimnames(m) <- NULL
if(length(n) == 3) {
m <- aperm(m, c(2, 1, 3))
n2 <- n[1] * n[3]
dim(m) <- c(n[2], n2)
mm <- apply(m, 1, sum)
mm
}
else {
cat(" Erreur dimension m")
return(0)
}
}
