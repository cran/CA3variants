\name{makeindicator}
\alias{makeindicator}
\title{
Make an Indicator matrix 
}
\description{
From a three-way contingency table (as can be used in CA3variants),
it gives the N x total number of categories (rows+cols+tubs) indicator matrix
}
\usage{
makeindicator(X) 
}
\arguments{
  \item{X}{
The three-way data array. It must be an \code{R} object array.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
\item{Z}{Output: the N x total number of categories (rows+cols+tubs) indicator matrix}
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
}
\author{
Rosaria Lombardo, Michel van de Velden, Eric J Beh.
}
%\examples{
%data(happy)
%makeindicator(happy)
%}
\keyword{indicator}% use one of  RShowDoc("KEYWORDS")
\keyword{matrix}% __ONLY ONE__ keyword per line
