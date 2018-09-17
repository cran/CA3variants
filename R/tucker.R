tucker <-
function(x, p, q, r, test = 10^-6){
     loss.old <- criter(x, 0)
    loss.new <- rep(0, 4)
    param <- init3(x, p, q, r)
    param <- step.g3(param)
    param <- newcomp3(param)
    loss.new[1] <- loss1.3(param, 0)
    paramp <- param
    cont <- 0
    while(abs(loss.old - loss.new[1]) > test) {
        cont <- cont + 1
        a.old <- paramp$a
        b.old <- paramp$b
        c.old <- paramp$cc
        loss.old <- loss.new[1]
        paramp <- stepi3(paramp)
        paramp <- step.g3(paramp)
        loss.new[3] <- loss2(paramp, b.old)
        paramp <- stepi3(paramp)
        paramp <- step.g3(paramp)
        loss.new[4] <- loss2(paramp, c.old)
        paramp <- stepi3(paramp)
        paramp <- step.g3(paramp)
        loss.new[2] <- loss2(paramp, a.old)
        loss.new[1] <- loss1.3(paramp, a.old)
    }
    paramp$cont <- cont
    paramp
}
