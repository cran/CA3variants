\name{newcomp3ordered}
\alias{newcomp3ordered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Updated component matrices
}
\description{
The function computes the updated component matrices of the Trivariate Moment Decomposition. 
It is supposed that the number of the ordered categorical variables is equal to 3.
}
\usage{
newcomp3ordered(param)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
The initial matrices of the row, column and tube components derived via the \code{init3} function.
}
}
\details{
It is utilised from the function \code{tuckerORDERED}.
}
%\value{
%Updated component matrices of the row, column and tube components derived via the Tucker3 model.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
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
%## The function is currently defined as
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Tucker3 components}
\keyword{multivariate}% __ONLY ONE__ keyword per line
