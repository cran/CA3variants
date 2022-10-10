\name{invcor}
\alias{invcor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Changing core sign}
\description{
This function is used from the function \code{signscore}. It changes the sign of the elements of core slice
}
\usage{
invcor(core, p, q, r, chgmode, chgcomp)
}
\arguments{
  \item{core}{
The core array.}
  \item{p}{
The dimension of the first mode.}
  \item{q}{

The dimension of the second mode.}
  \item{r}{
The dimension of the third mode.}
  \item{chgmode}{
One of the three mode to change.}
  \item{chgcomp}{
One of the three component to change.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{core}
\keyword{internal}% __ONLY ONE__ keyword per line
