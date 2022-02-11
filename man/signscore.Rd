\name{signscore}
\alias{signscore}
\title{
Changing the sign of negative core values.
}
\description{
This function  makes the signs of the largest core elements positive \cr
to facilitate interpretation. The appropriate columns of the component matrices for inversion \cr
are determined and are reversed accordingly.
}
\usage{
signscore(a, b, cc, I, J, K, p, q, r, core, IFIXA, IFIXB, IFIXC)
}
\arguments{
  \item{a}{
The first component matrix A.}
  \item{b}{
The second component matrix B.}
  \item{cc}{
The third component matrix C.}
  \item{I}{
The row number of the first mode of the three-way contingency table.}
  \item{J}{
The column number of the second mode of the three-way contingency table.}
  \item{K}{
The tube number of the third mode of the three-way contingency table.}
  \item{p}{
The dimension number of the first mode.}
  \item{q}{
The dimension number of the second mode.}
  \item{r}{
The dimension number of the third mode.}
  \item{core}{
The core array (generalized singular values).}
  \item{IFIXA}{
The flag parameter to indicate whether the first component (A) belongs to a fixed mode. \cr
 Fixed modes should not have their signs changed.}
  \item{IFIXB}{
The flag parameter to indicate whether the second component (B) belongs to a fixed mode. \cr
 Fixed modes should not have their signs changed.}
  \item{IFIXC}{
The flag parameter to indicate whether the third component (C) belongs to a fixed mode. \cr
Fixed modes should not have their signs changed.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{g }{The core array.}
\item{gord}{the core array ordered with respect to the largest values (descending order).}
\item{a }{The first matrix of components.}
\item{aord }{The ordered first matrix of the components.}
\item{b }{The second matrix of the components.}
\item{bord }{The ordered second matrix of the components.}
\item{cc }{The third matrix of the components.}
\item{ccord}{The ordered third matrix of the components.}
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
\keyword{sign}
\keyword{core}% __ONLY ONE__ keyword per line
