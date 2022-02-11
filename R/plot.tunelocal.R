plot.tunelocal<-
function(x,...){
#----------------------------plot tunelocal
cat("From tunelocal: Convex hull for choosing the optimal model dimension -Chi2 criterion-\n\n")
 #plot(x, type = "b")
plot(x)  
title(sub="Chi2 criterion")
#---
#if (is.na(x2[1])==FALSE){
#plot(x2, type = "b")
 #  title(sub="Bootstrap Data-Chi2 criterion")
#}
#else{cat("No Bootstrap data samples considered in tunelocal!!\n")}
}