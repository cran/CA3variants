\name{plot.CA3variants}
\alias{plot.CA3variants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical display resulting from  \code{CA3variants}
}
\description{
The function \code{plot.CA3variants} allows the analyst to graphically display 
six types of biplots for symmetrical 3-way variants and two types of biplots for non-symmetrical 3-way variants.
The six types of biplots for \code{CA3} and \code{OCA3} are the following.
When the input parameter is \code{biptype = "column-tube"} (or \code{biptype = "col-tube"}), the function displays the column-tube 
interactive biplot, where the column and tube variables are coded 
interactively and have principal coordinates and the row variable has standard coordinates.  
When the input parameter is \code{biptype = "row"}, the function displays the row biplot, where the rows have principal coordinates.  
When the input parameter is \code{biptype = "col"}, the function displays the column  biplot, where the columns have principal coordinates.  
When the input parameter is \code{biptype = "row-tube"}, the function displays the row-tube biplot, where the row-tubes have principal coordinates.  
When the input parameter is \code{biptype = "tube"}, the function displays the tube biplot, where the tubes have principal coordinates
When the input parameter is \code{biptype = "row-column"} (or \code{biptype = "row-col"}), the function displays the row-column 
interactive biplot, where the row-columns have principal coordinates.  
The two types of biplots for \code{NSCA3} and \code{ONSCA3} are the following.
When the input parameter is \code{biptype = "pred"}, the function displays the biplot where the predictors are coded 
interactively and have principal coordinates and the response has standard coordinates.  
When the input parameter is \code{biptype = "resp"}, the function displays the  biplot where the response variable has principal coordinates
and the predictors (interactively coded) have standard coordinates.  

By default, \code{biptype = "column-tube"}.
}
\usage{
\S3method{plot}{CA3variants}(x, firstaxis = 1, lastaxis = 2, thirdaxis = 3, cex = 0.8,  
biptype="column-tube", scaleplot = NULL,  plot3d = FALSE, pos = 1, 
size1 = 1, size2 = 3, addlines = TRUE,...)
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
 \item{thirdaxis}{
The dimension reflected along the third axis when \code{plot3d = TRUE}.
}
  \item{cex}{
The parameter that specifies the size of character labels of points in graphical displays. 
By default, it is equal to 0.8.
}
 \item{biptype}{
The input parameter for specifying what kind of biplot is requested. By default, 
it is equal to \code{column-tube}, but could be \code{row-tube}, \code{row-column}, \code{row},
\code{column} and \code{tube}.
}
\item{scaleplot}{
The scaling parameter for biplots to pull points away from the origin (see gamma biplot in Gower et al 2011). 
By default, it is equal to the overall average 
for the sum of squares of the two sets of coordinates (principal and standard ones), because of
the average sum of squares for the two sets of points is the same (see Van de Velden et al 2017).
}
\item{plot3d}{The logical parameter specifies whether a 3D plot is to be included 
in the output or not. By default, \code{plot3d = FALSE}.
}
\item{pos}{The input parameter for changing the label position. By default, it is equal to 1.
}
\item{size1}{The input parameter for specifying the size of pointers. By default, it is equal to 1.
}
\item{size2}{The input parameter for specifying the label size. By default, it is equal to 2.
}
\item{addlines}{The input parameter for plotting lines in  biplots (the points in standard coordinates 
are represented using lines). By default, it is equal to \code{addlines = TRUE}.}
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
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
Van de Velden M, Iodice D'Enza A, Palumbo F (2017) Cluster Correspondence Analysis. Psychometrika, 82, 158--185.\cr
Gower JC, Lubbe SG, and Le Roux, NJ (2011) Understanding biplots. New York: Wiley.\cr
Lombardo R, Beh EJ and Kroonenberg PM (2021) Symmetrical and Non-Symmetrical Variants of Three-Way Correspondence Analysis for Ordered Variables. Statistical Science Journal, p. 1-33.
}
\author{
Rosaria Lombardo, Eric J Beh and Michel van de Velden.
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
res.ca3<-CA3variants(happy, dims = c(p = 2, q = 2, r = 2), ca3type = "CA3") 
plot(res.ca3)
res.nsca3<-CA3variants(happy, dims = c(p = 2, q = 2, r = 2), ca3type = "NSCA3") 
plot(res.nsca3, biptype = "resp", plot3d = TRUE)
res.oca3<-CA3variants(happy, dims = c(p = 3, q = 5, r = 4), ca3type = "OCA3", norder = 3) 
plot(res.oca3, biptype = "tube",firstaxis=4,lastaxis=7)
res.onsca3<-CA3variants(happy, dims = c(p = 3, q = 5, r = 4), ca3type = "ONSCA3", norder = 3) 
plot(res.onsca3,biptype="resp",firstaxis=6,lastaxis=7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphical display}
\keyword{interactive plot}
\keyword{interactive biplot}% __ONLY ONE__ keyword per line
