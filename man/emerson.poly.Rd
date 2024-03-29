\name{emerson.poly}
\alias{emerson.poly}
\title{
Orthogonal polynomials}
\description{
This function is called  from the function \code{oca3basic} when in \code{CA3variants} we set 
\code{ca3type = "OCA3"}.  It allows the analyst to compute the orthogonal polynomials of 
the ordered categorical variable. The number of the polynomials is equal to the variable category less one.
The function computes the polynomial transformation of the ordered categorical variable.}
\usage{
emerson.poly(mj, pj)
}
\arguments{
\item{mj}{
The ordered scores of an ordered variable. By default \code{mj = NULL}, the natural scores 
(1,2,...) are computed.
}
\item{pj}{
The marginals, relative frequencies of the ordered variable. 
}
}
\value{
Describe the value returned
\item{B}{The matrix of the orthogonal polynomials without the trivial polynomial.}
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis: Theory, Practice and New Strategies. John Wiley & Sons.\cr
Emerson PL (1968)  Numerical construction of orthogonal polynomials from a general recurrence formula. Biometrics, 24 (3), 695-701.\cr
Lombardo R Beh EJ (2016) Variants of Simple Correspondence Analysis. The R Journal, 8 (2), 167--184.
}
\author{
Rosaria Lombardo and Eric J Beh.
}
\note{
Note that the sum of the marginals of the ordered variables should be one.
}
%\examples{
%emerson.poly(c(1,2,3,4,5), as.vector(c(.1,.2,.3,.2,.2)))
%}
\keyword{internal}% __ONLY ONE__ keyword per line
