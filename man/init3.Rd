\name{init3}
\alias{init3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initial components from the Tuckals3 algorithm
}
\description{
The function is utilised from the function \code{tucker} to compute the 
initial components for each of the three categorical variables.
}
\usage{
init3(x, p, q, r)
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
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The initial components for each of the three categorical
variables.
\item{a}{The initial component derived from the Tucker3 decomposition for the first mode.}
\item{b}{The initial component derived from the Tucker3 decomposition for the second mode.}
\item{cc}{The initial component derived from the Tucker3 decomposition for the third mode.}
\item{x}{The three-way contingency table}
}
%\references{
%Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice %and New Strategies. John Wiley & Sons.
%}
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
\keyword{internal}% __ONLY ONE__ keyword per line
