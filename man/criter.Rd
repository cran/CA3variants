\name{criter}
\alias{criter}
\title{The stopping criteria for the Tucker3 algorithm}
\description{The function computes the stopping criteria needed for the Tucker3 algorithm.
}
\usage{
criter(x, xhat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The three-way contingency table}
  \item{xhat}{
The reconstruction of the data array using the three component matrices and the core array.
}
}
\value{The criterion used in Tuckals3 is the sum of squares of the 
differences between the weighted data array and the reconstructed data array. 
}
\references{
Kroonenberg PM (1994) The TUCKALS line: a suite of programs for three-way data analysis. Computational Statistics and Data Analysis, 18, 73--96.
}
\author{
Rosaria Lombardo, Eric J Beh.
}
\keyword{internal}% __ONLY ONE__ keyword per line
%\keyword{criterion}% __ONLY ONE__ keyword per line
