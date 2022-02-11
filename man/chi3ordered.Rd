\name{chi3ordered}
\alias{chi3ordered}
\title{
The partition of the Pearson three-way index.
}
\description{
When three categorical variables are symmetrically related, we can analyse the strength of
the symmetrical  association using the three-way Pearson  statistic.
The function \code{chi3ordered} partitions the Pearson phi-squared statistic using orthogonal polynomials
when, in \code{CA3variants}, we set the parameter \code{ca3type = "OCA3"}.
}
\usage{
chi3ordered(f3, digits = 3)
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
The partition of the Pearson index into three two-way association terms and one 
three-way association term. It also shows the polynomial componets of inertia, the 
percentage of explained inertia, the degrees of freedom and p-value of each term of the partition.
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
\examples{
data(happy)
chi3ordered(f3=happy, digits=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{three way index}% __ONLY ONE__ keyword per line
