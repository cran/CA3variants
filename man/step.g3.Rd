\name{step.g3}
\alias{step.g3}
\title{
The core array derived via the Tucker3 model.
}
\description{The Tucker3 model involves the computation of principal components, which are derived for 
each of the three categorical variables, and of the core array which is akin to the generalised 
correlations between these components. The function \code{step.g3} computes the core array.
}
\usage{
step.g3(param)
}
\arguments{
  \item{param}{
The weighted three-way table and the matrices of the row, column and tube components 
derived via the Tuckals3 algorithm.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The core matrix whose the general element can be interpreted as a generalized singular value.
}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Eric J Beh, Ida Camminatiello.
}
%\note{
%%  ~~further notes~~
%}
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
\keyword{core}
\keyword{multivariate}% __ONLY ONE__ keyword per line
