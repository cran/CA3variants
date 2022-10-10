\name{stepi3}
\alias{stepi3}
\title{
Component matrices from the Tucker3 decomposition
}
\description{
The function computes the component matrices from the Tuckals3 algorithm.
}
\usage{
stepi3(param)
}
\arguments{
\item{param}{
The weighted contingency table and the matrices of the row, column and tube components derived via 
the Tucker3 model.}
}
\details{
The functions \code{newcomp3}, \code{stepi3}, \code{init3} and \code{step.g3} compute the 
component matrices and core array in the iterative steps of Tuckals3. They are all utilised from
the function \code{tucker}.
}
\value{
Component matrices from the Tucker3 decomposition.}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{Tuckals3}
\keyword{internal}% __ONLY ONE__ keyword per line
