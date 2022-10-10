\name{step.g3ordered}
\alias{step.g3ordered}
\title{
The core array derived via the Trivariate Moment Decomposition model.
}
\description{The Trivariate Moment Decomposition model involves the computation of 
principal polynomial components, which are derived for each  of the three categorical
variables, and of the polynomial core array which is akin to the generalised correlations 
between these components. The function \code{step.g3ordered} computes the core array.
}
\usage{
step.g3ordered(param)
}
\arguments{
  \item{param}{
The weighted three-way table and the matrices of the row, column and tube components 
derived via the Trivariate Moment Decomposition algorithm.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The core matrix whose the general element can be interpreted as a generalized singular value.
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
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
%\keyword{core}
\keyword{internal}% __ONLY ONE__ keyword per line
