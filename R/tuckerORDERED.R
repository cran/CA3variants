tuckerORDERED<-function (x, p, q, r, test = 10^-6, xi, norder=3) 
{
    loss.old <- criter(x, 0)
    loss.new <- rep(0, 4)
#browser()
param <- switch(norder, "1" = init3ordered1(x, p, q, r, xi),  "2" = 
        init3ordered2(x, p, q, r, xi),"3" = init3ordered(x, p, q, r, xi))

#if (norder == "one") {
  #      param <- init3ordered1(x, p, q, r, xi)
#param <- step.g3ordered(param)
#param <- newcomp3ordered1(param)
 # }
   # if (norder == "two") {
     #   param <- init3ordered2(x, p, q, r, xi)
#param <- step.g3ordered(param)
#param <- newcomp3ordered(param)
  #  }
    #if (norder =="three") {
      #  param <- init3ordered(x, p, q, r, xi)
#param <- step.g3ordered(param)
#param <- newcomp3ordered(param)
    #}
param <- step.g3ordered(param)
param <- newcomp3ordered(param)
  #  cat("devo entrare in newcomp3ordered\n")
  loss.new[1] <- loss1.3ordered(param, 0)
    paramp <- param
#browser()
    cont <- 0
        while (abs(loss.old - loss.new[1]) > test) {
        cont <- cont + 1
        a.old <- paramp$a
        b.old <- paramp$b
        c.old <- paramp$cc
        loss.old <- loss.new[1]
        paramp <- stepi3ordered(paramp)
        paramp <- step.g3ordered(paramp)
        loss.new[3] <- loss2(paramp, b.old)
        cat(cont, "b ")
        print(loss.new, 6)
        paramp <- stepi3ordered(paramp)
        paramp <- step.g3ordered(paramp)
        loss.new[4] <- loss2(paramp, c.old)
        cat(cont, "c ")
        print(loss.new, 6)
        paramp <- stepi3ordered(paramp)
        paramp <- step.g3ordered(paramp)
        loss.new[2] <- loss2(paramp, a.old)
        loss.new[1] <- loss1.3ordered(paramp, a.old)
        cat(cont, "a ")
        print(loss.new, 6)
    }
paramp$cont<-cont
    #cat("matrice Z\n")
    #paramp$Z <- param$Z
    #print(paramp$Z)
    return(paramp)
}
