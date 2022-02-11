\name{happy}
\alias{happy}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Three-way contingency table
}
\description{
This three-way contingency table was generated from the database of the European Social Survey 2016. The variables that we selected for our analysis are Education, Households and Happiness.}
\usage{data(happy)}
\format{
  The format is:
% num [1:13, 1:9] 81 66 150 667 67 24 47 430 743 132 ...\cr
row names [1:3] "ED1", "ED2", "ED3", "ED45" 
col names [1:5] "S1", "S2", "S3", "S4", "S5" 
tube names [1:4] "low", "middle", "high", "very-high"
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
happy <-
structure(c(325, 411, 793, 602, 239, 374, 827, 583, 63, 
181, 361, 303, 42, 129, 229, 224, 16, 49, 89, 54, 
11, 37, 31, 21, 357, 477, 1049, 929, 327, 610, 1447, 
1446, 115, 303, 763, 832, 64, 250, 591, 638, 35, 105, 
183, 185, 15, 56, 99, 71, 265, 327, 769, 928, 342, 
565, 1461, 1808, 104, 314, 768, 1006, 69, 312, 729, 
977, 21, 122, 215, 362, 14, 57, 126, 129, 214, 241, 
554, 660, 419, 561, 1467, 1861, 130, 290, 786, 938, 
89, 319, 741, 1022, 36, 121, 289, 408, 35, 87, 153, 
171), .Dim = c(4, 6, 4), .Dimnames = list(c("ED1", 
"ED2", "ED3", "ED45"), c("HS1", "HS2", "HS3", "HS4", "HS5", "HS>5"
), c("low", "middle", "high", "very-high")))
dim(happy)
}
\keyword{datasets}
