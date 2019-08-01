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
olive <-structure(c(20, 15, 12, 17, 16, 28, 18, 17, 18, 18, 
6, 25, 12, 9, 23, 21, 19, 30, 30, 22, 21, 17, 8, 
12, 23, 18, 20, 18, 10, 15, 11, 9, 26, 19, 17, 24
), .Dim = c(6L, 3L, 2L), .Dimnames = list(c("A", "B", "C", "D", 
"E", "F"), c("NW", "NE", "SW"), c("urban", "rural")))
dim(olive)
data(olive)
}
\keyword{datasets}
