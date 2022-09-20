\name{plot.tunelocal}
\alias{plot.tunelocal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical display resulting from  \code{tunelocal}
}
\description{
The function \code{plot.tunelocal} allows the analyst to graphically display 
the optimal model dimension using a convex hull. 

}
\usage{
\S3method{plot}{tunelocal}(x,...)
}
\arguments{
  \item{x}{
The results of the function \code{tunelocal}.  It shows the models that are located on the boundary of the convex hull 
and selects an optimal model by means of the scree test values (st).
When using \code{boots=F},  it gives the set of possible dimension combination
of the original data using only the original data array.
When using \code{boots=T},  it gives the set of possible dimension combination
of the original data using bootstrapped data arrays.
}
 % \item{col}{Vector of colors used for plots.
%}
\item{...}{Further arguments passed to or from other methods.}
}
\value{
Graphical displays of  a convex hull computed using the original data and the bootstrapped data when in \code{tunelocal} the input parameter \code{boot=TRUE} computed bootstrapped data too.
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
Wilderjans TF, Ceulemans E, and Meers K (2013) CHull: A generic convex hull based model
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
res.tunelocal<-tunelocal(happy, ca3type = "CA3",boots = FALSE, 
          nboots = 0)
plot(res.tunelocal)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphical display}
\keyword{convex hull}
\keyword{optimal dimension}% __ONLY ONE__ keyword per line
