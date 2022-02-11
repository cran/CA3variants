\name{prod3}
\alias{prod3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Products among arrays
}
\description{
The function calculates the products among arrays.
}
\usage{
prod3(m, a1, a2, a3)
}
\arguments{
  \item{m}{
The original three-way contingency table.}
  \item{a1}{
The weight matrix related to the rows of the table.}
  \item{a2}{
The weight matrix related to the columns of the table.
}
  \item{a3}{
The weight matrix related to the tubes of the table.
}
}
\details{
It is utilised in \code{standtab}, \code{rstand3} and \code{rstand3delta} in order to weight the 
contingency table with respect to the three weigth matrices defined in the row, column and tube spaces
differently for the three variants of three-way correspondence analysis.
}
\value{
The three-way contingency table weighted with respect the suitable weight matrices (depending on 
the analysis variants).
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
%\note{
%%%  ~~further notes~~
%}
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{product}
\keyword{weighted array}% __ONLY ONE__ keyword per line
