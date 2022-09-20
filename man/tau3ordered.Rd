\name{tau3ordered}
\alias{tau3ordered}
\title{
The partition of the Marcotorchino three-way index.
}
\description{
When three categorical variables are symmetrically related, we can analyse the strength of
the association using the three-way Marcotorchino index.
The function chi3 partitions the Marcotorchino statistic using orthogonal polynomials
when, in \code{CA3variants}, we set the parameter \code{ca3type = "ONSCA3"}.
}
\usage{
tau3ordered(f3, digits = 3)
}
\arguments{
\item{f3}{
The three-way contingency array given as an input parameter in \code{CA3variants}.
}
\item{digits}{
The number of decimal digits. By default digits=3.
}
}
\value{
The partition of the Marcotorchino index into three two-way non-symmetrical association terms and one 
three-way association term. It also shows the polynomial componets of inertia, the 
percentage of explained inertia, the degrees of freedom and p-value of each term of the partition.
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
}
\author{
Rosaria Lombardo, Eric J Beh.
}
\examples{
#data(olive)
tau3ordered(f3 = olive, digits = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{three way index}% __ONLY ONE__ keyword per line
