\name{nsca3basic}
\alias{nsca3basic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Three-way Non-Symmetrical Correspondence Analysis
}
\description{
This function is used in the main function \code{CA3variants} when the input parameter is 
\code{catype="NSCA3"}. \cr
It decomposes the Marcotorchino index, computes principal axes, coordinates, weights of rows and columns,\cr
total inertia (equal to the Marcotorchino index) and the rank of the matrix.
}
\usage{
nsca3basic(x, p, q, r, test = 10^-6, ctr = T, std = T)
}
\arguments{
  \item{x}{
The three-way contingency table.
}
  \item{p}{
The number of components of the first mode.
}
  \item{q}{
The number of components of the second mode.
}
  \item{r}{
The number of components of the third mode.
}
  \item{test}{
The treshold used in the algorithm.
}
  \item{ctr}{
The flag parameter to center the data (T or F), if F the data are not centered.
}
  \item{std}{
The flag parameter to weight the data (T or F), if F the data are not weighted.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{x}{The original three-way contingency table.}
\item{xs}{The weighted three-way contingency table.}
\item{xhat}{The three-way contingency table reconstructed after Tuckals3 by means of the 
principal components and core array.}
\item{nxhat2}{The inertia of the three-way non-symmetrical correspondence analysis \cr
 for one response (the three-way Marcotorchino index).}
\item{prp}{The proportion of inertia reconstructed using the principal components 
and the core array to the total inertia.\cr
To select the model dimensions (number of principal components), we examine 
the inertia explained by the p, q, r principal components  with respect to the overall fit.}
\item{a}{The row principal components.}
\item{b}{The column principal components.}
\item{cc}{The tube principal components.}
\item{g}{The core array (generalized singular values) calculated by using the Tuckals3 algorithm. \cr
They help to explain the strength of the association among the three principal components.}
\item{iteration }{The number of iterations that are required for the TUCKALS3 algorithm to converge.}
}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}% __ONLY ONE__ keyword per line
\keyword{three-way non-symmetric correspondece analysis }% __ONLY ONE__ keyword per line
