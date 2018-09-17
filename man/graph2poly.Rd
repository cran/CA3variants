\name{graph2poly}
\alias{graph2poly}
\title{
Two-dimensional correspondence  plot or biplot.
}
\description{
This function produces a two-dimensional correspondence biplot, 
given the set of computed coordinates. 
In case of ordered correspondence analysis, the biplot uses polynomial coordinates.
}
\usage{
graph2poly(f, g, ni, nj, nk, a1 = 1, a2 = 2, prop = 0.8, cex = 1, 
length = 0.01,  cex.lab = 0.8, cols = c(1, 4), mar = c(5, 4, 4, 2) + 0.1, 
arrow = F, type = "l", pos = 2)
}
\arguments{
  \item{f}{
The polynomial coordinates of the row categories.
}
  \item{g}{
The polynomial coordinates of the column-tube categories.
}
  \item{ni}{
The category number of row variable.}
  \item{nj}{
The category number of column variable.}
  \item{nk}{
The category number of tube variable.}
  \item{a1}{
The dimension reflected along the horizontal principal axis in the graphical display. By default n1=1.
}
  \item{a2}{
The dimension reflected along the vertical principal axis in the graphical display. By default n1=2.
}
  \item{prop}{
The scaling parameter for changing the area of plotting region. By default, prop = 1.
}
  \item{cex}{
The parameter that specifies the size of character labels of points in graphical displays. By default, it is equal to 0.5.
}
  \item{length}{
The length of the arrows whenever graphically dipicted.
}
  \item{cex.lab}{
The parameter for setting the size of character labels of axes in graphical displays. 
}
  \item{cols}{
The parameter for setting the column size of data table.
}
  \item{mar}{
The parameter for setting the size of plot area in graphical displays.
}
  \item{arrow}{
The flag parameter to dipict arrows (T or F), if T the distances between the column-tube categories are represented as arrows.
}
   \item{type}{
The input parameter for specifying the type of line to draw between coordinates. By default, 
it is equal to \code{l} a line.}
  \item{pos}{
The input parameter for specifying the label position. By default, it is equal to 2.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Eric J Beh.
}
%\note{
%%  ~~further notes~~
%}
\keyword{plot}% use one of  RShowDoc("KEYWORDS")
\keyword{biplot}% __ONLY ONE__ keyword per line
