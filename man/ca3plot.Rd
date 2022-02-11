\name{ca3plot}
\alias{ca3plot}
\title{
Row isometric biplot or Column isometric biplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is used in the main plot function  when the plot type parameter is 
\code{plottype = "biplot"} and the variants of three-way CA are not ordered. It can produce a row or a column biplot. 
}
\usage{
ca3plot(frows, gcols, firstaxis, lastaxis, inertiapc, size1, size2, biptype, addlines)
}
\arguments{
\item{frows}{
The row principal or standard coordinates.
}
\item{gcols}{
The column principal or standard coordinates.
}
\item{firstaxis}{
The first axis number.
}
\item{lastaxis}{
The second axis number. 
}
\item{inertiapc}{
The percentage of the explained inertia by each dimension.
}
\item{size1}{
The size of the plotted symbol for categories in biplots. 
}
\item{size2}{
The size of the plotted text for categories in biplots. 
}
\item{biptype}{The input parameter for specifying what kind of biplots is requested. By default, 
it is equal to \code{column-tube}, but could be \code{row}.
}
\item{addlines}{The input parameter for plotting lines in  biplot. By default, it is equal to \code{addlines = TRUE}.}
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis: Theory, Practice and New Strategies. John Wiley & Sons.\cr
Lombardo R Beh EJ (2016) Variants of Simple Correspondence Analysis. The R Journal, 8 (2), 167--184.\cr
Van de Velden M, Iodice D'Enza A, Palumbo F (2017) Cluster Correspondence Analysis. Psychometrika, 82, 158--185.\cr
Gower JC, Lubbe SG, and Le Roux, NJ (2011) Understanding biplots. New York: Wiley.
}
\author{
Rosaria Lombardo, Eric J. Beh and Michel van de Velden.
}
\note{
This function depends on the \code{R} library \code{plotly}.
}
%\examples{
%data(asbestos)
%cabasic(asbestos)
%}
\keyword{biplot}
