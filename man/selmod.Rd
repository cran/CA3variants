\name{selmod}
\alias{selmod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selecting the mode.}
\description{
Select the mode in which the column has to be sign reversed.
Below is an heuristic algorithm but a fully rational choice is hard to come by.
Maximal number of sign reversals = p+q+r-2, but this number can be much smaller.
Sign reverse a component, determine which if any of p, q and r is available for reversal.
If one of them is wholly positieve way  never choose it,  else if one is wholly negative  choose that one from  A, B, C respectively;
else if there is a component with a largest absolute value which is negative  choose that one, or the one from A,B,C in that order;
else choose the column of the longest mode; end if  FreeA, FreeB, FreeC = 0 component is not available; = 1 component is available.
}
\usage{
selmod(aptr, bptr, cptr, posptrA, negptrA, bigptrA, posptrB, negptrB, bigptrB, 
posptrC, negptrC, bigptrC, IA, IB, IC, I, J, K, p, q, r, longest)
}
\arguments{
  \item{aptr}{
The pointer to the first component matrix.
}
  \item{bptr}{
The pointer to the second component matrix.
}
  \item{cptr}{
The pointer to the third component matrix.
}
  \item{posptrA}{
The pointer to the positive component of the first component matrix.}
  \item{negptrA}{
The pointer to the negative component of the first component matrix.
}
  \item{bigptrA}{
The pointer to the biggest component (a larger negative positive value than a positive one) of the first component matrix.}
  \item{posptrB}{
The pointer to the positive component of the first component matrix.}
  \item{negptrB}{
The pointer to the negative component of the second component matrix.
}
  \item{bigptrB}{
The pointer to the biggest component (a larger negative positive value than a positive one) of the second component matrix.}
  \item{posptrC}{
The pointer to the positive component of the third component matrix.}
  \item{negptrC}{
The pointer to the negative component of the third component matrix.
}
  \item{bigptrC}{
The pointer to the biggest component (a larger negative positive value than a positive one) of the third component matrix.}
  \item{IA}{
The pointer to the columns of the first component matrix, given the pointer to an element in core. }
  \item{IB}{
The pointer to the columns of the second component matrix, given the pointer to an element in core. }
  \item{IC}{
The pointer to the columns of the third component matrix, given the pointer to an element in core. }
  \item{I}{
The row number of the three-way contingency table.}
  \item{J}{
The column number of the three-way contingency table.}
  \item{K}{
The tube number of the three-way contingency table.}
  \item{p}{
The dimension number of the first mode.}
  \item{q}{
The dimension number of the second mode.}
  \item{r}{
The dimension number of the third mode.}
  \item{longest}{
The component matrix of the longest mode to change sign (when no special reason for selection could be found).}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{success }{The flag variable to indicate if one of the components has to be sign reversed.}
 \item{chgmode}{Select the mode (1, 2 or 3) in which the column has to be sign reversed.}
 \item{chgcomp}{The pointer to the columns of the component matrices (A, B or C) \cr
that are responsible for the value in the ordered core array.}

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
%\keyword{sign}
\keyword{core}% __ONLY ONE__ keyword per line
