\name{init3ordered1}
\alias{init3ordered1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initial components from the Trivariate Moment Decomposition algorithm
}
\description{
The function is utilised from the function \code{tuckerORDERED} to compute the 
initial components for the first ordered categorical variables.
}
\usage{
init3ordered1(x, p, q, r, x0)
}
%- maybe also 'usage' for other objects documented here.
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
 \item{x0}{
The original three-way contingency table.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The initial components for each of the three categorical
variables.
%%  If it is a LIST, use
\item{a}{The initial component derived from the Trivariate Moment Decomposition for the first mode.}
\item{b}{The initial component derived from  the Trivariate Moment Decomposition for the second mode.}
\item{cc}{The initial component derived from  the Trivariate Moment Decomposition for the third mode.}
\item{x}{The three-way contingency table.}
%% ...
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
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
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Trivariate Moment Decomposition}
\keyword{Tucker3}% __ONLY ONE__ keyword per line
