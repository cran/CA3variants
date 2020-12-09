\name{coord}
\alias{coord}
\title{The weighted components of the Tucker3 algorithm}
\description{The function computes the weighted components from the Tucker3 algorithm 
(to take into account the different weight systems in row, column and tube spaces) for symmetrical
three-way correspondence analysis.
}
\usage{
coord(res, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{The component matrices resulting from the Tucker3 algorithm.}
  \item{x}{
The original three-way contingency table.
}
}
%\value{The criterion used in Tuckals3 is the sum of squares of the 
%differences between the weighted data array and the reconstructed data array. 
%}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\\
}
\author{
Rosaria Lombardo, Eric J Beh.
}
\keyword{criterion}% __ONLY ONE__ keyword per line
