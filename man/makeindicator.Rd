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
\author{
Rosaria Lombardo, Eric J Beh.
}
%\examples{
%data(happy)
%makeindicator(happy)
%}
%\keyword{indicator}% use one of  RShowDoc("KEYWORDS")
\keyword{internal}% __ONLY ONE__ keyword per line
