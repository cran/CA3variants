\name{loss2}
\alias{loss2}
\title{
Difference between two successive components
}
\description{
The function computes the difference between two successive components in the iteration of the 
Tuckals3 algorithm.
}
\usage{
loss2(param, comp.old)
}
\arguments{
  \item{param}{
The matrices of the row, column and tube components derived via the Tucker3 model.
}
  \item{comp.old}{
The matrices of the row, column and tube components derived in the foregoing iteration of the Tuckals3 
algorithm.}
}
\value{
The difference between two successive components in the iteration of the Tuckals3 algorithm.
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo and Eric J Beh.
}
%\note{
%%  ~~further notes~~
%}
\keyword{loss}% __ONLY ONE__ keyword per line
