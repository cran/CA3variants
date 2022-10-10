\name{tucker}
\alias{tucker}
\title{
Tucker3 decomposition of the three-way table.
}
\description{
The Tucker3 model, originally proposed by psychologist Ledyard R. Tucker, involves
the computation of principal components, which are derived for each of the three categorical 
variables, and of the core array which is akin to the generalised correlations between these components.
The function represents the heart of the Tuckals3 algorithm to perform the Tucker3 decomposition of the 
three-way array x.
}
\usage{
tucker(x, p, q, r, test = 10^-6)
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
}
\details{
The function \code{tucker} is utilised from the functions \code{ca3basic}, \code{nsca3basic} and 
\code{oca3basic}.
}
\value{
\item{a}{The final component derived from the Tucker3 decomposition for the first mode.}
\item{b}{The final component derived from the Tucker3 decomposition for the second mode.}
\item{cc}{The final component derived from the Tucker3 decomposition for the third mode.}
\item{g}{The core array.}
\item{x}{The three-way contingency table.}
\item{cont}{The number of iterations that are required for the Tucker3 algorithm to converge.}
}
\references{
Kroonenberg PM (1994) The TUCKALS line: a suite of programs for three-way data analysis. Computational Statistics and Data Analysis, 18, 73--96.
}
\author{
Rosaria Lombardo, Eric J Beh.
}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%## The function is currently defined as
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{multivariate}
\keyword{internal}% __ONLY ONE__ keyword per line
