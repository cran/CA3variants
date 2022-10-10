\name{invcmp}
\alias{invcmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Changing component sign
}
\description{
This function changes the sign of the elements in column of the component
   matrix.
}
\usage{
invcmp(comp, nr, nc, chgcomp)
}
\arguments{
  \item{comp}{
One of the three component matrices.}
  \item{nr}{
The row number of the component matrix.}
  \item{nc}{
The column number of the component matrix.}
  \item{chgcomp}{
The pointers to the columns of the component matrix that are responsible for the value in the ordered core.
}
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
%\keyword{ ordered }
\keyword{internal}% __ONLY ONE__ keyword per line
