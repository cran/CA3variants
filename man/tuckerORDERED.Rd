\name{tuckerORDERED}
\alias{tuckerORDERED}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trivariate moment decomposition of the three-way table.
}
\description{
The Trivariate moment decomposition (TMD) represents the heart of a new algorithm to perform the 
decomposition of the three-way ordered contingency tables. It is based on the orthogonal
polynomials (Emerson 1968) computed for each categorical ordered variable.
}
\usage{
tuckerORDERED(x, p, q, r, test = 10^-6,xi, norder=3)
}
\arguments{
  \item{x}{
The three-way contingency table.
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
The treshold used in the algorithm.
}
\item{xi}{The original three-way contingency table.}
 \item{norder}{
The number of ordered variables.
}
}
\details{
The function \code{tuckerORDERED} is utilised from the function \code{oca3basic}.
}
\value{
\item{a}{The final component derived from the TMD decomposition for the first mode.}
\item{b}{The final component derived from the TMD decomposition for the second mode.}
\item{cc}{The final component derived from the TMD decomposition for the third mode.}
\item{g}{The core array.}
\item{x}{The three-way contingency table.}
\item{cont}{The number of iterations that are required for the TMD algorithm to converge. If all variables are
ordered, the convergence is reached in one step, differently if we have mixed variables. Indeed,
the decmposition will become hybrid, a mix of TMD algorithm and Tuckals3 algorithm.}
}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
Emerson PL 1968  Numerical construction of orthogonal polynomials from a general recurrence formula. Biometrics, 24 (3), 695-701.\cr
Lombardo R Beh EJ Variants of Simple Correspondence Analysis. The R Journal, 8 (2), 167--184.\cr
Lombardo R Beh EJ and Kroonenberg PM (2016) Modelling Trends in Ordered Correspondence Analysis Using Orthogonal Polynomials. Psychometrika, 325--349.
}
\author{
Rosaria Lombardo, Eric J Beh.
}
\keyword{multivariate}
\keyword{Tucklas3}% __ONLY ONE__ keyword per line
