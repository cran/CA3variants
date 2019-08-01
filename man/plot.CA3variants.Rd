\name{plot.CA3variants}
\alias{plot.CA3variants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical display resulting from  \code{CAvariants3}
}
\description{
The function \code{plot.CA3variants} allows the analyst to graphically display 
the biplot from their analysis. 
When the input parameter is \code{biptype = "column-tube"}, the function displays the column-tube 
interactive biplot.  
When the input parameter is \code{biptype = "row"}, the function displays the row interactive biplot.  
By default, \code{biptype = "column-tube"}.
}
\usage{
\S3method{plot}{CA3variants}(x, firstaxis = x$firstaxis, lastaxis = x$lastaxis, cex = 0.8,  
biptype="column-tube", prop = 1, scaleplot = 1,  pos=1, 
size=2, size2=3,...)
}
\arguments{
  \item{x}{
The output parameters of the main function \code{CA3variants}.
}
  \item{firstaxis}{
The dimension reflected along the horizontal axis.
}
  \item{lastaxis}{
The dimension reflected along the vertical axis.
}
  \item{cex}{
The parameter that specifies the size of character labels of points in graphical displays. 
By default, it is equal to 0.8.
}
%  \item{plottype}{
%The input parameter for specifying what kind of graphical display is requested. By default, 
%it is equal to biplot.
%}
 \item{biptype}{
The input parameter for specifying what kind of biplot is requested. By default, 
it is equal to \code{column-tube}, but could be \code{row}.
}
  \item{prop}{
The scaling parameter for changing the limits of the plotting area. By default, it is equal to 1.
}
  \item{scaleplot}{
The scaling parameter for pos=1,size=2,cols=c(1,4). By default, it is equal to 1.
}
%  \item{arrow}{
%The flag parameter to display arrows (T or F), if T the distances between the column-tube categories are represented as arrows.
%}
\item{pos}{The input parameter for changing the label position. By default, it is equal to 1.
}
\item{size}{The input parameter for specifying the size of points. By default, it is equal to 2.
}
\item{size2}{The input parameter for specifying the label size. By default, it is equal to 3.
}
\item{...}{Further arguments passed to or from other methods.}
}
\details{
It is utilised by the main function \code{CA3variants} and uses the secondary graphical function \code{graph2poly}.
}
\value{
Graphical displays of three-way correspondence analysis variants. Interactive plots or biplots are
the graphical results of this function.
}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(happy)
ris.ca3<-CA3variants(happy,p=2,q=2,r=2, ca3type = "CA3") 
plot(ris.ca3)
ris.nsca3<-CA3variants(happy,p=2,q=2,r=2, ca3type = "NSCA3") 
plot(ris.nsca3)
ris.oca3<-CA3variants(happy, ca3type = "OCA3",norder=3) 
plot(ris.oca3)
ris.onsca3<-CA3variants(happy, ca3type = "ONSCA3",norder=3) 
plot(ris.onsca3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphical display}
\keyword{interactive plot}
\keyword{interactive biplot}% __ONLY ONE__ keyword per line
