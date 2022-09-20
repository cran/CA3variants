\name{tunelocal}
\alias{tunelocal}
\title{
Dimension selection for three-dimensional correspondence biplot using convex hull.
}
\description{
This function allows to select the optimal dimension number 
for correspondence biplot, given the set of possible dimension combination
of the original data.  It determines the models that are located on the boundary of the convex hull 
and selects an optimal model by means of the scree test values (st).
For exploring, it is also possible to check the optimal model dimension by using
boostrap samples which have the same marginal proportions and the total number
of the original table. When the input parameter \code{boots = T}, it does bootstrap sampling. 
There are three kinds of possible bootstrap sampling. 
When \code{boottype = "bootnp"} it performs a non parametric bootstrap sampling.
When \code{boottype = "bootpsimple"} it performs a parametric simple bootstrap sampling.
When \code{boottype = "bootpstrat"}, it performs a parametric stratified bootstrap sampling.  
In particular in case of parametric bootstrap types,
when \code{resamptype=1} it considers a multinomial distribution, and when \code{resamptype = 2} 
it  considers a poisson distribution. 
}
\usage{
tunelocal(Xdata, ca3type = "CA3", resp = "row", norder = 3, digits = 3, boots = FALSE,  
nboots = 0, boottype= "bootpsimple", resamptype = 1, PercentageFit = 0.01) 
}
\arguments{
\item{Xdata}{
The three-way data. It can be a \code{R} object array or raw data (n individuals by three categorical variables, for an example,
see \code{museum} data).
When a three-way non-symmetrical variant is performed, by default, the response variable is the row variable when an array is given, or 
the first of three columns when a raw data set is given. 
For changing, consider the parameter \code{resp = "col"} or  \code{resp = "tube"}.
}
 \item{ca3type}{
The specification of the analysis to be performed. 
If \code{ca3type = "CA3"}, then a three-way (symmetrical) correspondence analysis will be performed 
(default analysis). 
If \code{ca3type = "NSCA3"}, then three-way non-symmetrical correspondence analysis  will be performed.
If \code{ca3type = "OCA3"}, then ordered three-way symmetrical correspondence analysis  will be performed.
If \code{ca3type = "ONSCA3"}, then ordered three-way non-symmetrical correspondence analysis  will be performed.
}
\item{resp}{
The input parameter for specifying in non-symmetrical three-way correspondence analysis variants (\code{ca3type = "NSCA3"} 
and \code{ca3type = "ONSCA3"}) what is the response variable (logically antecendent to the others). 
By default \code{resp = "row"}, but it could be the column variable \code{resp = "col"} or the tube
variable \code{resp = "tube"}.  
}
  \item{norder}{
The input parameter for specifying the number of ordered variable when \code{ca3type = "OCA3"} or \code{ca3type = "ONSCA3"}.
}
  \item{digits}{
The input parameter specifying the digital number. By default, \code{digits = 3}.
}
  \item{boots}{
The flag parameter to perform the search of optimal dimensions using
bootstrap samples. By defaults, \code{boots = FALSE}.}
  \item{nboots}{
The number of bootstrap samples to generate when \code{boots = TRUE}. Note that when \code{boots = FALSE},\cr
by default \code{nboots = 0}, but when \code{boots = TRUE}, by default \code{nboots = 100}. 
}
\item{boottype}{The specification of the kind of bootstrap sampling to be performed. 
If \code{boottype = "bootpsimple"}, then a parametric bootstrap using a simple sampling scheme will 
be performed (default sampling).
If \code{boottype = "bootpstrat"}, then a parametric bootstrap using a stratified sampling scheme will 
be performed.
If \code{boottype = "bootnp"}, then a non-parametric bootstrap using a simple sampling scheme will 
be performed.}
\item{resamptype}{When the kind of bootstrap is parametric you can set the data distribution using 
the input parameter \code{resamptype} according to two distribution: \code{resamptype=1} corresponds 
to multinomial distribution and \code{resamptype=2} to Poisson distribution.}
\item{PercentageFit}{Required proportion of increase in fit of a more complex model. By default, 
 \code{PercentageFit  =  0.01}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%\item{XG}{The list of tables on which is performed the three-way CA variant.
%It consists of the original array and (when \code{boots=T}) bootstrapped arrays.}
\item{output1}{Chi-square criterion and df of models on the convex hull. 
 It gives the criterion values of the models that are located on the boundary of the convex hull 
and selects the optimal model by means of the scree test values (st).
When using \code{boots = FALSE},  it gives the set of possible dimension combination
of the original data using only the original data array.
When using \code{boots = TRUE},  it gives the set of possible dimension combination
of the original data using bootstrapped data arrays.
}
%\item{output2}{Chi-square criterion and df of models on the convex hull when using bootstrapped arrays.}
%\item{output3}{Badness of fit criterion and df of models on the convex hull when using the original array.}
%\item{output4}{Badness of fit criterion and df of models on the convex hull when using bootstrapped arrays.}
%\item{output5}{Goodness of fit criterion and df of models on the convex hull when using the original array.}
%\item{output6}{Goodness of fit criterion and df of models on the convex hull when using bootstrapped arrays.}
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
Wilderjans T F, Ceulemans E, and Meers K (2013) CHull: A generic convex hull based model
selection method. Behavior Research Methods, 45, 1-15.\cr
Ceulemans E, and Kiers H A L (2006) Selecting among three-mode principal component models
of different types and complexities: A numerical convex hull based method. British Journal of
Mathematical & Statistical Psychology, 59, 133-150.
}
\author{
Rosaria Lombardo, Michel van de Velden, Eric J Beh.
}
\examples{
%data(happy)
tunelocal(Xdata = happy, ca3type = "CA3")
%tunelocal(happy, ca3type = "NSCA3", resp="row", boots=FALSE)
%tunelocal(happy,ca3type="OCA3", boots=TRUE, boottype="bootpsimple", nboots=5)
%tunelocal(happy,ca3type="ONSCA3", boots=TRUE, boottype="bootnp", nboots=10)
}
\keyword{bootstrap}% use one of  RShowDoc("KEYWORDS")
\keyword{dimension}% __ONLY ONE__ keyword per line
