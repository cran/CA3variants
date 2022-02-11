summary.CA3variants <-function(object,digits=3,...){
#UseMethod("summary")
#--------------------------------------------
#---------------------------------------------------
# cat("\n    RESULTS for 3-way Correspondence Analysis\n")
#if ((object$ca3type=="OCA3")||(object$ca3type=="ONSCA3")){
#cat("\n Index partionings\n\n")
#print(round(object$index3res$z,digits=digits))    
#print(round(object$index3,digits=digits))    
#print(round(object$index3res$zij,digits=digits))
#    print(round(object$index3res$zik,digits=digits))
#    print(round(object$index3res$zjk,digits=digits))
#      cat("\n\n")
#}
#if ((object$ca3type=="CA3")||(object$ca3type=="NSCA3")){
#cat("\n Index partionings\n\n")
#    print(round(object$index3,digits=digits))
#    cat("\n\n")}
  cat("Explained inertia (reduced dimensions)", object$inertiatot, "\n\n")
    cat("Total inertia (complete dimensions)", object$inertiaorig, "\n\n")
cat("Proportion of explained inertia (when reducing dimensions)\n\n")
    print(object$prp)
#--------------------------------------
#---------------------------------------------------
if ((object$ca3type=="OCA3")||(object$ca3type=="ONSCA3")){
cat("\n Index partionings\n\n")
print(round(object$index3res$z,digits=digits))    
print(round(object$index3,digits=digits))    
print(round(object$index3res$zij,digits=digits))
    print(round(object$index3res$zik,digits=digits))
    print(round(object$index3res$zjk,digits=digits))
      cat("\n\n")
}
if ((object$ca3type=="CA3")||(object$ca3type=="NSCA3")){
cat("\n Index partionings\n\n")
    print(round(object$index3,digits=digits))
    cat("\n\n")}
 # cat("\n    OBJECTS of 3-way Correspondence Analysis\n")
#UseMethod("summary")
}