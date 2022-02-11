\name{srtcor}
\alias{srtcor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sort the core array
%%  ~~function to do ... ~~
}
\description{
     This function sorts (a copy of) the core matrix and returns
     CORPTR, an p*q*r integer array holding the pointers to the greatest
     absolute values in CORE.
}
\usage{
srtcor(core, p, q, r)
}
\arguments{
  \item{core}{
The core array (generalized singular values).}
  \item{p}{
The dimension number of the first mode.}
  \item{q}{
The dimension number of the second mode.}
  \item{r}{
The dimension number of the third mode.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{coreptr}{The pointer to the ordered largest value of the core array.}
}
\references{
Kroonenberg PM (1994) The TUCKALS line: a suite of programs for three-way data analysis. Computational Statistics and Data Analysis, 18, 73--96.
}
\author{
Rosaria Lombardo and Pieter M Kroonenberg.}
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
%function (core, p, q, r) 
%{
%    coreptr = sort(abs(core), index.return = T, decreasing = T)$ix
%    list(coreptr = coreptr)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{pointer}
\keyword{ core}% __ONLY ONE__ keyword per line
