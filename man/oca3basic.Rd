\name{oca3basic}
\alias{oca3basic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Three-way Ordered Symmetrical Correspondence Analysis
}
\description{
This function is used in the main function \code{CA3variants} when the input parameter is \code{ca3type="OCA3"}. \cr
It performs the three-way symmetric correspondence analysis by TUCKALS3.
}
\usage{
oca3basic(x, p, q, r, test = 10^-6, ctr = T, std = T, norder = 3, sign = TRUE)
}
\arguments{
  \item{x}{The three-way contingency table.
}
 \item{p}{
The number of components of the first mode.
}
  \item{q}{
The number of components of the second mode.
}
  \item{r}{
The number of components of the third mode.
}
  \item{test}{
The treshold used in the algorithm TUCKALS3.
}
  \item{ctr}{
The flag parameter (T or F), if F the analysis is not centered.
}
  \item{std}{
The flag parameter (T or F) if F the analysis is not standardized.
}
 \item{norder}{
The number of ordered variables considered.
}
\item{sign}{The input parameter for changing the sign to the components according to the core sign.}
}
\value{
\item{x}{The original three-way contingency table.}
\item{xs}{The weighted three-way contingency table.}
\item{xhat}{Three-way contingency table reconstructed after Tuckals3 by principal components and core array}
\item{nxhat2}{The inertia of three-way symmetric correspondence analysis (Three-way Pearson ratio).}
\item{prp}{The proportion of inertia reconstructed using the p, q, r principal components 
and the core array to the total inertia.
To select the model dimensions (number of principal components), we examine 
the inertia explained by the p, q, r principal components  with respect to the overall fit.}
\item{a}{The row principal components.}
\item{b}{The column principal coordinates.}
\item{cc}{The tube principal coordinates.}
\item{g }{The core array
calculated by using the Tuckals3 algorithm and can be interpreted 
as generalised singular value table. 
They help to explain the strength of the association between the three principal components.}
\item{iteration }{The number of iterations that are required for the TUCKALS3 algorithm to converge.}
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Eric J Beh.
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
\keyword{ordered}
