loss1.3 <-
function(param, comp.old){
    xhat <- reconst3(param)
    criter(param$x, xhat)
}
