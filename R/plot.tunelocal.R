plot.tunelocal<-
function(x,...){
#----------------------------plot tunelocal
cat("Convex hull for assessing the optimal model dimension -Goodness criterion-\n\n")
if (x$boots==TRUE){
plot(x$output1, type = "b")
  title(sub="Bootstrap Data -Chi2 criterion-")
}
else{
plot(x$output1, type = "b")
title(sub="Original Data -Chi2 criterion-")
}#---
}