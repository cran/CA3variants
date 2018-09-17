\name{reconst3}
\alias{reconst3}
\title{
Reconstruction of the three-way centred profile table
}
\description{
The function reconstructs the three-way centred profile table using the component matrices from 
the Tucker3 decomposition and the core array.
}
\usage{
reconst3(param)
}
\arguments{
  \item{param}{
The matrices of the row, column and tube components derived via the Tucker3 model.
}
}
\value{
The three-way reconstructed table of centred profiles. 
}
\references{
Beh E J and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
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
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{reconstruction}
\keyword{multivariate}% __ONLY ONE__ keyword per line
