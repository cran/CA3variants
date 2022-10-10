\name{print.tunelocal}
\alias{print.tunelocal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print of tunelocal function results
}
\description{
This function prints the results of \code{tunelocal} for choosing the optimal model dimension of a variant of three-way correspondence analysis. 
When \code{boots = T} the number of different models that is assessed is based on the size of the original data being analysed.\cr
For example, for a 4 x 5 x 4, there are 80 different models that are assessed.\cr
When \code{boots = T},  the number of different models that is assessed is based on the size of all models \cr
obtained from the combination of dimensions of the bootstrapped data. \cr
For example, for a 4 x 5 x 4 array, there are 800 different models that are assessed. By default\code{nboots = 100}, \cr
you can change the parameter value in input of \code{tunelocal} function.\cr
}
\usage{
\S3method{print}{tunelocal}(x, digits = 3,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The name of the output of the function \code{tunelocal}.
}
  \item{digits}{
The input parameter specifying the digital number. By default, \code{digits = 3}.
}
\item{...}{Further arguments passed to or from other methods.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The value of output returned depends on the kind of sampling chosen.
The sampling for making the convex hull can be based on the 
 original data or on the bootstrapped data samples. 
In detail:
\item{XG}{The data samples used for assessing the optimal model dimension (original and/or bootstrapped).}
%\item{risCAchi2}{Chi2 as goodness-of-fit criterion  for choosing the optimal model dimension.}
%\item{risCAdf}{Degree of freedom as measure of complexity used for choosing the optimal model dimension.}
\item{output1}{The results of \code{tunelocal}.  It gives the goodness-of-fit criteria of models that are located \cr
on the boundary of the convex hull and selects the optimal model by means of the scree test values (st); \cr
see Ceulemans and Kiers  (2006).}
\item{ca3type}{It gives information about the kind of variant of three-way CA considered.}
\item{boots}{The flag parameter to perform the search of optimal dimensions using
bootstrap samples. By defaults, \code{boots = FALSE}.}
}
\references{
Wilderjans T F, Ceulemans E, and Meers K (2013) CHull: A generic convex hull based model
selection method. Behavior Research Methods, 45, 1-15.\cr
Ceulemans E, and Kiers H A L (2006) Selecting among three-mode principal component models
of different types and complexities: A numerical convex hull based method. British Journal of
Mathematical & Statistical Psychology, 59, 133-150.
}
\author{
Rosaria Lombardo,  Michel van de Velden and Eric J. Beh.
}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
%data(happy)
res.tunelocal<-tunelocal(happy, ca3type = "CA3") 
print(res.tunelocal)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tunelocal}
\keyword{print}% __ONLY ONE__ keyword per line
