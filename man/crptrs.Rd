\name{crptrs}
\alias{crptrs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pointing to the columns of the component matrices.
}
\description{
Given ICORE, i.e. the pointer to an element in CORE(p,q,r), this subroutine
     calculates the IA, IB and IC, pointing to the columns of the component
     matrices A, B and C that are responsible for the value in CORE(ICORE).
}
\usage{
crptrs(icore, p, q, r)
}
\arguments{
  \item{icore}{ The pointer to the core elements whose sign should be reversed.
}
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
\item{IA}{
The pointer to the columns of the first component matrix, given the pointer to an element in core. }
  \item{IB}{
The pointer to the columns of the second component matrix, given the pointer to an element in core. }
  \item{IC}{
The pointer to the columns of the third component matrix, given the pointer to an element in core. }
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
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{pointer}
\keyword{components}% __ONLY ONE__ keyword per line
