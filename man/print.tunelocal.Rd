\name{print.tunelocal}
\alias{print.tunelocal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print of tunelocal function results
}
\description{
This function prints the results of \code{tunelocal} for choosing the optimal model dimension of a variant of three-way correspondence analysis. 
When the input parameter, in \code{print.tunelocal}, is \code{digits = 3},
the function prints all the results using three digital numbers.
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
dimension of only original data or on the dimension of bootstrapped data samples.
In detail:
\item{output1}{The results of \code{tunelocal}.  It gives the models that are located on the boundary of the convex hull 
and selects an optimal model by means of the scree test values (st).
}
%\item{output2}{The output of the model dimension using the bootstrapped data.}
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
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
%\examples{
%data(happy)
%res.tunelocal<-tunelocal(happy, ca3type = "CA3",boots = FALSE, 
%          nboots = 0) 
%print(res.tunelocal)
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tunelocal}
\keyword{print}% __ONLY ONE__ keyword per line
