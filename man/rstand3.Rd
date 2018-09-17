\name{rstand3}
\alias{rstand3}
\title{
Weighted centred three-way table for three-way non-symmetric correspondence analysis
}
\description{
The function computes the three-way weighted centred contingency table to perform three-way
non-symmetric correspondence analysis with one response and two predictors.
}
\usage{
rstand3(x, std = T, ctr = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The original three-way contingency table.
}
  \item{std}{
The flag parameter for weighting the original table. If \code{std=F} the original contingency table is not weighted.
}
  \item{ctr}{
The flag parameter for centering the original table. If  \code{ctr=F} the original array is not centered.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{xs}{The weighted array with respect to the three associated metrics. It is used 
when \code{CA3variants="NSCA"} and represents the three-way weighted and centred column profile table.}
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
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{weight}
\keyword{ multivariate }% __ONLY ONE__ keyword per line
