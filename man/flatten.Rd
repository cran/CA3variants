\name{flatten}
\alias{flatten}
\title{
Flattened table
}
\description{
The function flattens the three-way table into the concatenation of two-way matrices.
}
\usage{
flatten(x)
}
\arguments{
  \item{x}{The three-way contingency table.}
}
\details{
It is utilised by a number of functions: \code{CA3variants}, \code{reconst3}, \code{newcomp3} and 
\code{step.g3}.}
\value{
\item{x}{The flattened table of size I,JK where I, J and K are the number of the categories
 of rows, columns and tubes, respectively.}
}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Eric J Beh.
}
\keyword{multivariate}


