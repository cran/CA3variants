\name{CA3variants}
\alias{CA3variants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correspondence Analysis variants for three-way contingency tables
}
\description{
This function performs four variants of three-way correspondence analysis (CA). It does the three-way symmetrical CA, when \code{ca3type = "CA3"}, and three-way non-symmetrical 
CA, when \code{ca3type = "NSCA3"}, by using the Tucker3 decomposition.
It also performs  ordered  three-way symmetrical CA, when \code{ca3type = "OCA3"}, and ordered
three-way non-symmetrical CA, when \code{ca3type = "ONSCA3"}, 
by using the Trivariate Moment Decomposition. The non-symmetrical variants consider the three 
variables asymmetrically related, such that one of the variables is the response to be predicted 
given the other two variables. It calculates the coordinates and inertia values of the chosen analyses. 
Furthermore, it allows to look at the index (Pearson's chi-squared or Marcotorchino's tau) partition.
}
\usage{CA3variants(Xdata, dims = c(p, q, r), ca3type = "CA3", test = 10^-6, 
resp = "row", norder = 3, sign = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xdata}{
The three-way data. It can be a \code{R} object array or raw data (n individuals by three categorical variables, for an example,
see \code{museum} data).
When a three-way non-symmetrical variant is performed, by default, the response variable is the row variable when an array is given, or 
the first of three columns when a raw data set is given. 
For changing, consider the parameter \code{resp = "col"} or  \code{resp = "tube"}.
}
  \item{dims}{
The number of components for the first, second and third mode. By default,  no \code{dims} is given. When using an
ordered variant of three-way CA recall to consider the complete dimension, i.e. the number of components 
for the first, second and third mode must be equal to the number of rows, columns and tubes, respectively.
}
   \item{ca3type}{
The specification of the analysis to be performed. 
If \code{ca3type = "CA3"}, then a three-way symmetrical correspondence analysis will be performed 
(default analysis). 
If \code{ca3type = "NSCA3"}, then three-way non-symmetrical correspondence analysis  will be performed.
If \code{ca3type = "OCA3"}, then ordered three-way symmetrical correspondence analysis  will be performed.
If \code{ca3type = "ONSCA3"}, then ordered three-way non-symmetrical correspondence analysis  will be performed.
}
  \item{test}{
Threshold used in the algorithm for stopping it after the convergence of the
solutions.
}
 \item{resp}{
The input parameter for specifying in non-symmetrical three-way correspondence analysis variants (\code{ca3type = "NSCA3"} 
and \code{ca3type = "ONSCA3"}) what is the response variable (logically antecendent to the others). 
By default, \code{resp = "row"}, but it could be \code{resp = "col"} or \code{resp = "tube"}.  
}
  \item{norder}{
The input parameter for specifying the number of ordered variable when \code{ca3type = "OCA3"} or \code{ca3type = "ONSCA3"}. 
By default, all three variables are ordered \code{norder = 3}. When  \code{norder = 1}, you assume that the ordered variable is the column variable.
When  \code{norder = 2}, you assume that the ordered variables are the row and column variable.
}
\item{sign}{The input parameter for changing the sign to the components according to the core sign.}
}
\details{
This function recall internally many other functions, depending on the setting of the input parameters.
After performing three-way symmetric or non-symmetric correspondence analysis, it recall two functions for printing and plotting the results. 
These two important functions are \code{print.CA3variants} and \code{plot.CA3variants}.}
\value{
The value of output returned depends on the kind of analysis performed.
For a detailed description of the output one can see: \cr
the output value of \code{ca3basic} if the input parameter is \code{ca3type="CA3"};
the output value of \code{nsca3basic} if the input parameter is \code{ca3type="NSCA3"};
the output value of \code{oca3basic} if the input parameter is \code{ca3type="OCA3"} 
the output value of \code{onsca3basic} if the input parameter is \code{ca3type="ONSCA3"} 
 }
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
Kroonenberg PM (1994) The TUCKALS line: a suite of programs for three-way data analysis. Computational Statistics and Data Analysis, 18, 73--96.\cr
Lombardo R, Beh EJ and Kroonenberg PM (2020-preprint) Symmetrical and Non-Symmetrical Variants of Three-Way Correspondence Analysis for Ordered Variables. Statistical Science Journal, p. 1-33.
}
\author{
Rosaria Lombardo,  Eric J Beh and Michel van de Velden.
}
\examples{
%data(museum)
%CA3variants(Xdata = museum, dims = c(p=2,q=2,r=2), ca3type = "CA3") 
data(ratrank)
CA3variants(Xdata = ratrank, dims = c(p=2,q=2,r=1), ca3type = "CA3") 
data(happy)
CA3variants(Xdata = happy, dims = c(p=2,q=2,r=2), ca3type = "NSCA3") 
CA3variants(Xdata = happy, dims = c(p=3,q=5,r=4), ca3type = "OCA3") 
CA3variants(Xdata = happy, dims = c(p=3,q=5,r=4), ca3type = "ONSCA3") 
}
% R documentation directory.
\keyword{three-way symmetrical correspondence analysis.}
\keyword{three-way non-symmetrical correspondence analysis.}% __ONLY ONE__ keyword per line
