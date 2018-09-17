\name{stepi3ordered}
\alias{stepi3ordered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Component matrices from the Trivariate Moment Decomposition decomposition
}
\description{
The function computes the polynomial component matrices from the Emerson's recurrence formula for 
the ordered categorical variables of the analysis.
}
\usage{
stepi3ordered(param)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{param}{
The weighted contingency table and the matrices of the row, column and tube components derived via 
the Trivariate Moment Decomposition model.}
}
\details{
The functions \code{newcomp3ordered}, \code{stepi3ordered}, \code{init3ordered} and 
\code{step.g3ordered} compute the polynomial component matrices and core array in the Trivariate Moment Decomposition. 
They are all utilised from the function \code{tuckerORDERED}.
}
\value{
Component matrices from the Trivariate Moment Decomposition decomposition.}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Tucklas3}
\keyword{multivariate}% __ONLY ONE__ keyword per line
