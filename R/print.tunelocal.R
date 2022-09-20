print.tunelocal<-function(x, digits=3,...) {
cat("Note that when boots = T,  the data samples generated are given in the object named 'XG' \n\n")
#print(x$XG)
#----
#cat("From tunelocal: Chi2 as goodness-of-fit criterion  for #choosing the optimal model dimension\n\n")
#print(x$risCAchi2)
#----
#cat("From tunelocal: Degree of freedom (df) as measure of #the complexity of the model for choosing the optimal model #dimension\n\n")
#print(x$risCAdf)
#----
cat("Results for choosing the optimal model dimension\n\n")
print(x$output1)
#-----
#cat("From tunelocal: Variant of three-way CA considered\n\n")
#print(x$ca3type)

}