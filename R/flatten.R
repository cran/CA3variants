flatten <-
function(x) {
    nom <- dimnames(x)
    dimnames(x) <- NULL
    n <- dim(x)
    dim(x) <- c(n[1], n[2] * n[3])
    x
}
