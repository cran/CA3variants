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
#--------------------------------------
cat("Core table \n")
print(round(object$g/sqrt(object$cost),digits=digits))
cat("Squared core table\n")
    print(round(object$g^2/object$cost,digits=digits))

#---------------------------------------------------
  cat("Explained inertia (reduced dimensions)\n")
  print(round(object$inertiatot/object$cost,digits=digits))
cat("\n")
    cat("Total inertia (complete dimensions)\n")
  print(round(object$inertiaorig/object$cost,digits=digits))
cat("\n")
cat("Percentage of explained inertia (when reducing dimensions)\n")
#    print(round(object$inertiatot/object$inertiaorig), digits=digits)
    print(object$prp*100, digits=digits)
#if ((object$ca3type=="OCA3")||(object$ca3type=="ONSCA3")){
#cat("\n Index partionings\n\n")
#print(round(object$index3res$z,digits=digits))    
#print(round(object$index3,digits=digits))    
#print(round(object$index3res$zij,digits=digits))
 #   print(round(object$index3res$zik,digits=digits))
  #  print(round(object$index3res$zjk,digits=digits))
   #   cat("\n\n")
#}
#if ((object$ca3type=="CA3")||(object$ca3type=="NSCA3")){
#cat("\n Index partionings\n\n")
 #   print(round(object$index3,digits=digits))
   # cat("\n\n")}
 # cat("\n    OBJECTS of 3-way Correspondence Analysis\n")
#UseMethod("summary")
}