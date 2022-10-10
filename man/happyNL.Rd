\name{happyNL}
\alias{happyNL}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Raw data: Three variables from a Dutch  survey on happiness
}
\description{
This raw data table represents a possible data set selected from a large survey on happiness.
The rows are individuals. The first column concerns four level of happiness, the second column concerns the number of households in a family, 
and the third column their level of Education.}
\usage{data(museum)}
\format{
  The format is:
row names [1:4] "low", "middle", "high", "very-high"
col names [1:5] "HS1", "HS2", "HS3", "HS4", ">HS5"
tube names [1:4] "ED1", "ED2", "ED3", "ED45"
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Lombardo R, van de Velden M and Beh E J (2022) Three-way Correspondence Analysis in R. (submitted) \cr
}
\examples{
happyNL<-structure(c(11L, 12L, 15L, 7L, 2L, 6L, 17L, 13L, 0L, 2L, 4L, 
6L, 0L, 5L, 7L, 3L, 0L, 3L, 3L, 1L, 14L, 56L, 52L, 22L, 11L, 
39L, 70L, 65L, 1L, 14L, 19L, 14L, 5L, 12L, 16L, 20L, 2L, 3L, 
10L, 4L, 14L, 44L, 44L, 15L, 6L, 27L, 79L, 47L, 4L, 17L, 40L, 
27L, 2L, 25L, 49L, 38L, 1L, 12L, 12L, 11L, 10L, 41L, 66L, 24L, 
4L, 32L, 100L, 90L, 1L, 8L, 40L, 28L, 3L, 15L, 49L, 35L, 1L, 
4L, 23L, 15L), .Dim = c(4L, 5L, 4L), .Dimnames = list(happy = c("low", 
"middle", "high", "very-high"), hhmmb = c("HS1", "HS2", "HS3", 
"HS4", ">HS5"), edulvla = c("ED1", "ED2", "ED3", "ED45")), class = "table")
dim(happyNL)
data(happyNL)
}
\keyword{datasets}
