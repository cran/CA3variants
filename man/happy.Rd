\name{happy}
\alias{happy}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Three-way contingency table
}
\description{
This three-way contingency table represents an historical data set found in Beh and Lombardo (2014).}
\usage{data(happy)}
\format{
  The format is:
% num [1:13, 1:9] 81 66 150 667 67 24 47 430 743 132 ...\cr
row names [1:3] "H1", "H2", "H3" 
col names [1:5] "S1", "S2", "S3", "S4", "S5" 
tube names [1:4] "E1", "E2", "E3", "E4"
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
happy <-
structure(c(15, 17, 7, 34, 53, 20, 36, 70, 23, 22, 67, 16, 61, 
79, 36, 31, 60, 5, 60, 96, 12, 46, 45, 11, 25, 40, 12, 26, 31, 
7, 35, 63, 5, 45, 74, 10, 30, 39, 4, 13, 24, 4, 8, 7, 3, 18, 
15, 2, 14, 15, 1, 3, 9, 2, 3, 2, 0, 4, 1, 1), .Dim = c(3L, 5L, 
4L), .Dimnames = list(c("H1", "H2", "H3"), c("S1", "S2", "S3", 
"S4", "S5"), c("E1", "E2", "E3", "E4")))
dim(happy)
}
\keyword{datasets}
