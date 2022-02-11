print.CA3variants <-function(x, printall=FALSE, digits=3,...) {
if (printall==FALSE){   
#--------------------------------------------
#cat("The three-way contingency table considered\n\n")
#print(x$Data)
#--------------------------------------------
if ((x$ca3type=="CA3")||(x$ca3type=="OCA3")) {

cat("Percentage contributions of the components to the total inertia for column-tube biplots\n\n")
    print(round(x$inertiacoltub,digits=digits))
#---------------------------------------------------
cat("Percentage contributions of the components to the total inertia for row-tube biplots\n\n")
    print(round(x$inertiarowtub,digits=digits))
#---------------------------------------------------
cat("Percentage contributions of the components to the total inertia for row-column biplots\n\n")
    print(round(x$inertiarowcol,digits=digits))
#---------------------------------------------------
}#end for symmetric
#-----------------------------------------------------------
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {
if (x$resp=="row"){
cat("Percentage contributions of the components to the total inertia for pred biplots\n\n")
    print(round(x$inertiacoltub,digits=digits))
}
#---------------------------------------------------
if ((x$resp=="column")||(x$resp=="col")){
cat("Percentage contributions of the components to the total inertia for pred biplots\n\n")
    print(round(x$inertiarowtub,digits=digits))
 }
#---------------------------------------------------
if (x$resp=="tube"){
cat("Percentage contributions of the components to the total inertia for pred biplots\n\n")
    print(round(x$inertiarowcol,digits=digits))
}
#---------------------------------------------------
}#end for non-symmetric


#-----------------------------------index partition
if ((x$ca3type=="OCA3")||(x$ca3type=="ONSCA3")){
cat("\n Index partition\n\n")
print(round(x$index3res$z,digits=digits))    
print(round(x$index3,digits=digits))    
print(round(x$index3res$zij,digits=digits))
    print(round(x$index3res$zik,digits=digits))
    print(round(x$index3res$zjk,digits=digits))
      cat("\n\n")
}
if ((x$ca3type=="CA3")||(x$ca3type=="NSCA3")){
cat("\n Index partition\n\n")
    print(round(x$index3,digits=digits))
    cat("\n\n")}
   
}#end printall
###############################################################################
if (printall==TRUE){   
#cat("The three-way contingency table \n\n")
#print(x$Data)
 cat("Number of iteration steps \n")
    print(x$iteration)
    cat("\n    RESULTS for 3-way Correspondence Analysis\n")
      #  if ((x$ca3type=="CA3")|(x$ca3type=="OCA3")){
      #  cat("Three-way Pearson standardised residuals \n")
      #  print(round(x$xs, digits = 2))
    #} 
#else {
 #       cat("Three-way (weighted) centered column profile table \n")
  #      print(round(x$xs, digits = 2))
   # }
cat("\n Row marginals\n\n")
     print(round(x$pi, digits = digits))
cat("\n Column marginals\n\n")
     print(round(x$pj, digits = digits))
cat("\n Tube marginals\n\n")
     print(round(x$pk, digits = digits))
cat("\n Row-Column marginals\n\n")
     print(round(x$pij, digits = digits))
cat("\n Column-Tube marginals\n\n")
     print(round(x$pjk, digits = digits))
cat("\n Row-Tube marginals\n\n")
     print(round(x$pik, digits = digits))
    cat("Explained inertia (reduced dimensions)", x$inertiatot, "\n\n")
    cat("Total inertia (complete dimensions)", x$inertiaorig, "\n\n")
#    cat("Percentage of explained inertia on total inertia  \n\n")
# cat("Percentage contribution of components to the total variation\n\n")
#   print(round(x$inertiapcsum,digits=digits))
#   print(round(x$inertiapc,digits=digits))
    #cat("Proportion of explained inertia (when reducing dimensions)\n\n")
    #print(x$prp)
    cat("\n Rows in principal coordinates\n\n")
    print(round(x$fi, digits = digits))
    cat("\n Rows in standard coordinates\n\n")
    print(round(x$fiStandard, digits = digits))
    cat("\n Column-tubes in principal coordinates\n\n")
    print(round(x$gjk, digits = digits))
 cat("\n Column-tubes in standard coordinates\n\n")
    print(round(x$gjkStandard, digits = digits))
    cat("\n Inner Product of coordinates (row x interactive)\n\n")
    print(round(x$iproductijk, digits = digits))
    cat("\n Inner Product of coordinates (col x interactive)\n\n")
    print(round(x$iproductjik, digits = digits))
    cat("\n Inner Product of coordinates (tube x interactive)\n\n")
    print(round(x$iproductkij, digits = digits))
    cat("Core array i.e. Generalised singular values  \n\n")
    print(round(x$g, digits = digits))
#--------------------------------------------
if ((x$ca3type=="CA3")||(x$ca3type=="OCA3")) {

cat("Percentage contributions of the components to the total inertia for row biplots\n\n")
    print(round(x$inertiarow,digits=digits))
cat("Percentage contributions of the components to the total inertia for column-tube biplots\n\n")
    print(round(x$inertiacoltub,digits=digits))
#---------------------------------------------------
cat("Percentage contributions of the components to the total inertia for column biplots\n\n")
    print(round(x$inertiacol,digits=digits))
cat("Percentage contributions of the components to the total inertia for row-tube biplots\n\n")
    print(round(x$inertiarowtub,digits=digits))
#---------------------------------------------------
cat("Percentage contributions of the components to the total inertia for tube biplots\n\n")
    print(round(x$inertiatube,digits=digits))
cat("Percentage contributions of the components to the total inertia for row-column biplots\n\n")
    print(round(x$inertiarowcol,digits=digits))
}#end for symmetric
#--------------------------------------
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {
if (x$resp=="row"){
cat("Percentage contributions of the components to the total inertia for resp biplots\n\n")
    print(round(x$inertiarow,digits=digits))
cat("Percentage contributions of the components to the total inertia for pred biplots\n\n")
    print(round(x$inertiacoltub,digits=digits))
}
#---------------------------------------------------
if ((x$resp=="column")||(x$resp=="col")){
cat("Percentage contributions of the components to the total inertia for resp biplots\n\n")
    print(round(x$inertiacol,digits=digits))
cat("Percentage contributions of the components to the total inertia for pred biplots\n\n")
    print(round(x$inertiarowtub,digits=digits))
 }
#---------------------------------------------------
if (x$resp=="tube"){
cat("Percentage contributions of the components to the total inertia for resp biplots\n\n")
    print(round(x$inertiatube,digits=digits))
cat("Percentage contributions of the components to the total inertia for pred biplots\n\n")
    print(round(x$inertiarowcol,digits=digits))
}
#---------------------------------------------------
}#end for non-symmetric

#--------------------------------------
    if ((x$ca3type=="OCA3")||(x$ca3type=="ONSCA3")){
cat("\n Index partition\n\n")
print(round(x$index3res$z,digits=digits))    
print(round(x$index3,digits=digits))    
print(round(x$index3res$zij,digits=digits))
    print(round(x$index3res$zik,digits=digits))
    print(round(x$index3res$zjk,digits=digits))
      cat("\n\n")
}
if ((x$ca3type=="CA3")||(x$ca3type=="NSCA3")){
cat("\n Index partition\n\n")
    print(round(x$index3,digits=digits))
    cat("\n\n")}

    cat("\n\n")
}
}