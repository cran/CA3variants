\name{print.CA3variants}
\alias{print.CA3variants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print of three-way correspondence analysis results
}
\description{
This function prints the results of three-way symmetrical or non-symmetrical correspondence analysis. 
If the input parameter, in CA3variants, is \code{ca3type="CA3"}, the function prints the results 
of three-way symmetrical correspondence analysis.
If the input parameter, in \code{CA3variants}, is \code{ca3type="NSCA3"},
the function prints the results of three-way non-symmetrical correspondence analysis.
If the input parameter, in \code{CA3variants}, is \code{ca3type="OCA3"},
the function prints the results of ordered three-way symmetrical correspondence analysis.
When the input parameter, in \code{print.CA3variants}, is \code{digits = 3},
the function prints all the results using three digital numbers.
}
\usage{
\S3method{print}{CA3variants}(x, digits = 3,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the name of the output of the main function CAvariants.
}
  \item{digits}{
The input parameter specifying the digital number. By default, \code{digits = 3}.
}
\item{...}{Further arguments passed to or from other methods.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The value of output returned depends on the kind of three-way correspondence analysis variant performed.
It also gives the number of the iteration of the algorithm to reach the convergence of the solution.
Depending on the variant of three-way correspondence analysis performed, it gives the related weighted
contingency table, the reconstructed table by the components and core array, the explained inertia,
the total inertia, the inertia in percentage, the proportion of explained inertia given the defined number
of the components, the row standard and principal coordinates, the interactive column-tube standard and 
principal coordinates, the inner-product matrix of coordinates, the core array and index partitioning.
In detail:
\item{CA3variants}{The output of the kind of three-way correspondence analysis analysis considered.}
\item{DataMatrix}{The original three-way contingency table.}
\item{xs}{The centred and weighted three-way contingency table when the input parameters are \code{ctr=T} 
and \code{std=T}.}
\item{xhat}{The three-way contingency table reconstructed by the three component matrices 
(of dimension Ixp, Jxq, and Kxr) and the core array.}
\item{nxhat2}{The sum of squares of the xhat table elements.}
\item{prp}{The ratio between the sum of squares of the xhat table elements and of the xs table elements.} 
\item{fi}{The principal row coordinates.}
\item{fiC}{The standard row coordinates.}
\item{gjk}{The principal colum-tube coordinates.}
\item{rows}{The row marginals of the three-way data table.}
\item{cols}{The column marginals of the three-way data table.}
\item{tubes}{The tube marginals of the three-way data table.}
\item{flabels}{The row category labels.}
\item{glabels}{The column category labels.}
\item{maxaxes}{The maximum dimension to consider.}
\item{inertia}{The total inertia of three-way symmetrical or non-symmetrical correspondence analysis.}
\item{inertiapc0}{The percentage contribution of the three components to the total variation.}
\item{inertiapc}{The vector of the percentage contributions of the three components to the total inertia.}
\item{iproduct}{The inner product between the standard row coordinates (fi) and the column-tube principal 
coordinates (gjk).}
\item{g}{The core array (i.e. the generalized singular values) calculated by using the Tuckals3 algorithm. } 
\item{index3}{When \code{ca3type = "CA3"}, the index3 represents the partition of the Pearson index 
into three two-way association terms and one three-way association term.
It also shows the C statistic of each term, its degrees of freedom and p-value.
If \code{ca3type = "NSCA3"} index3 returns the partition of the Marcotorchino index into three 
two-way association terms and one three-way association term. It also shows the C statistic of each term, 
its degrees of freedom and p-value.}
\item{ca3type}{The specification of the analysis to be performed.
When \code{ca3type = "CA3"}, then a three-way symmetrical correspondence analysis will be performed 
(default analysis). If \code{ca3type = "NSCA3"}, then three-way non-symmetrical correspondence analysis 
will be performed, where one of the variables is the response to be predicted given the other two variables. 
These two three-way variants use the Tucker3 method of decomposition.
When \code{ca3type = "OCA3"}, then an ordered three-way symmetrical correspondence analysis will be performed. 
Differently, this analysis uses a new method of decomposition called Trivariate Moment Decomposition.}
\item{iteration}{The number of iterations that are required for the Tuckals3 algorithm to converge.}
}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons
}
\author{
Rosaria Lombardo, Eric J Beh.
}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(happy)
ris.ca3<-CA3variants(happy,p=2,q=2,r=2, ca3type = "CA3") 
print(ris.ca3)
ris.nsca3<-CA3variants(happy,p=2,q=2,r=2, ca3type = "NSCA3") 
print(ris.nsca3)
ris.oca3<-CA3variants(happy,p=2,q=2,r=2, ca3type = "OCA3",norder=3) 
print(ris.oca3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
\keyword{partition}% __ONLY ONE__ keyword per line
