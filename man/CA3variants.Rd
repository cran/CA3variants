\name{CA3variants}
\alias{CA3variants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correspondence Analysis variants for three-way contingency tables
}
\description{
This function performs the three-way symmetrical (when \code{ca3type = "CA3"}) and non-symmetrical 
correspondence analysis (when \code{ca3type = "NSCA3"}) by using the Tucker3 decomposition
and an ordered variant of three-way symmetrical correspondence analysis (when \code{ca3type = "OCA3"})
by using the Trivariate Moment Decomposition. The non-symmetrical variant considers the three 
variables asymmetrically related, such that one of the variables is the response to be predicted 
given the other two variables. It calculates the coordinates and inertia values of the chosen analyses. 
}
\usage{
CA3variants(Xtable, p = dim(Xtable)[[1]], q = dim(Xtable)[[2]], r = dim(Xtable)[[3]], 
ca3type = "CA3", test = 10^-6, norder = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xtable}{
The three-way data array. It must be an \code{R} object array.
When non-symmetrical analysis for one response variable is performed, the response mode is the row variable.
}
  \item{p}{
The number of components for the first mode. By default,  \code{p = dim(Xtable)[[1]]}.
}
  \item{q}{
The number of components for the second mode. By default, \code{q = dim(Xtable)[[2]]}.
}
  \item{r}{
The number of components for the third mode. By default, \code{r = dim(Xtable)[[3]]}.
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
  \item{norder}{
The input parameter for specifying the number of ordered variable when \code{ca3type = "OCA3"}.
}
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
%\item{ca3corporateresults}{The output of the kind of three-way correspondence analysis analysis considered.}
%\item{DataMatrix}{The original three-way contingency table.}
%\item{xs}{The centred and weighted three-way contingency table when the input parameters are \code{ctr="T"} and \code{std="T"}.}
%\item{xhat}{The three-way contingency table reconstructed by the p, q, and r components and core array.}
%\item{nxhat2}{The sum of squares of the xhat table elements.}
%\item{prp}{The ratio between the sum of squares of the xhat table elements and of the xs table elements.} 
%\item{fi}{The principal row coordinates.}
%\item{fiC}{The standard row coordinates.}
%\item{gjk}{The principal colum-tube coordinates.}
%\item{rows}{The row marginals of the three-way data table.}
%\item{cols}{The column marginals of the three-way data table.}
%\item{tubes}{The tube marginals of the three-way data table.}
%\item{flabels}{The row category labels.}
%\item{glabels}{The column category labels.}
%\item{maxaxes}{The maximum dimension to consider.}
%\item{inertia}{The total inertia of three-way symmetrical or non-symmetrical correspondence analysis.}
%\item{inertiapc0}{The percentage contribution of the three components to the total variation.}
%\item{inertiapc}{The vector of the percentage contributions of the three components to the total inertia.}
%\item{iproduct}{The inner product between the standard row coordinates (fi) and the column-tube principal coordinates (gjk).}
%\item{g}{The core array (i.e. the generalized singular values) calculated by using the Tuckals3 algorithm. } 
%\item{index3}{When \code{ca3type = "CA3"}, the index3 represents the partition of the Pearson index into three two-way association terms and one three-way association term.
%It also shows the C statistic of each term, its degrees of freedom and p-value.
%If \code{ca3type = "NSCA3"} index3 returns the partition of the Marcotorchino index into three two-way association terms 
%and one three-way association term. It also shows the C statistic of each term, its degrees of freedom and p-value.
%}
%\item{ca3type}{The specification of the analysis to be performed.\cr
%When \code{ca3type = "CA3"}, then a three-way symmetrical correspondence analysis will be performed 
%(default analysis).\\ 
%If \code{ca3type = "NSCA3"}, then three-way non-symmetrical correspondence analysis will be
%performed, where one of the variables is the response to be predicted given the other two variables.\cr
%}
%\item{iteration}{The number of iterations that are required for the Tuckals3 algorithm to converge.}
%}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
Kroonenberg PM (1994). The TUCKALS line: a suite of programs for three-way data analysis. Computational Statistics and Data Analysis, 18, 73--96.
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
\examples{
data(happy)
CA3variants(happy,p=2,q=2,r=2, ca3type = "CA3") 
CA3variants(happy,p=2,q=2,r=2, ca3type = "OCA3") 
CA3variants(happy,p=2,q=2,r=2, ca3type = "NSCA3") 
CA3variants(happy,p=2,q=2,r=2, ca3type = "ONSCA3") 
}
% R documentation directory.
\keyword{three-way symmetrical correspondence analysis.}
\keyword{three-way non-symmetrical correspondence analysis.}% __ONLY ONE__ keyword per line
