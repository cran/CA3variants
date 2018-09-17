\name{olive}
\alias{olive}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Three-way contingency table
}
\description{
This three-way contingency table represents an historical data set found in Agresti (1990).}
\usage{data(olive)}
\format{
  The format is:
% num [1:13, 1:9] 81 66 150 667 67 24 47 430 743 132 ...\cr
row names [1:6] "A", "B", "C", "D", "E", "F" 
col names [1:3] "NW", "NE", "SW"
tube names [1:2] "urban", "rural"
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Beh EJ and Lombardo R 2014 Correspondence Analysis: Theory, Practice and New Strategies. John Wiley & Sons. \cr
}
\examples{
olive <-
structure(c(20L, 15L, 12L, 17L, 16L, 28L, 18L, 17L, 18L, 18L, 
6L, 25L, 12L, 9L, 23L, 21L, 19L, 30L, 30L, 22L, 21L, 17L, 8L, 
12L, 23L, 18L, 20L, 18L, 10L, 15L, 11L, 9L, 26L, 19L, 17L, 24L
), .Dim = c(6L, 3L, 2L), .Dimnames = list(c("A", "B", "C", "D", 
"E", "F"), c("NW", "NE", "SW"), c("urban", "rural")))
dim(olive)
}
\keyword{datasets}
