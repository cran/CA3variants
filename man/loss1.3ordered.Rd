\name{loss1.3ordered}
\alias{loss1.3ordered}
\title{
General loss criterion
}
\description{
This function represents the general loss function on which is based the Trivariate Moment Decomposition
and calculates the difference between two arrays, x and xhat, where x is the three-way contingency table and xhat 
is the reconstruction of this table by means of components and core array.}
\usage{
loss1.3ordered(param, comp.old)
}
\arguments{
  \item{param}{
The matrices of the row, column and tube components derived via the Trivariate Moment Decomposition model.}
  \item{comp.old}{
The matrices of the row, column and tube components derived in the foregoing iteration of 
the Trivriate Moment Decomposition algorithm.}
}
\value{
The difference between three-way contingency table and its reconstruction from the Trivariate Moment Decomposition model.
}
%\references{
%Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice %and New Strategies. John Wiley & Sons.
%}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{criterion}
\keyword{internal}% __ONLY ONE__ keyword per line
