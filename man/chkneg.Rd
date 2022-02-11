\name{chkneg}
\alias{chkneg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check the sign of component values
}
\description{
This function is called from \code{signscore}. It checks the negativity of the column of an array
    AND the positivity of the columns of an array
     If NegPtr = 1 then there is an entirely negative component
     If PosPtr = 1 then there is an entirely positive component
     If BigPtr = 1 then maximum neg. abs > max pos
}
\usage{
chkneg(comp, nr, nc)
}
\arguments{
  \item{comp}{
One of three component matrix.
}
\item{nr}{
The row number of the component matrix.}
  \item{nc}{
The column number of the component matrix.}
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
Kroonenberg PM (1994) The TUCKALS line: a suite of programs for three-way data analysis. Computational Statistics and Data Analysis, 18, 73--96.\cr
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
\keyword{array}% __ONLY ONE__ keyword per line
