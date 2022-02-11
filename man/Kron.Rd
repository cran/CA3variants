\name{Kron}
\alias{Kron}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kronecker product
}
\description{
The function performs the Kronecker product. Starting from two matrices of dimension \cr
IxP and JxQ the resulting  matrix will be of dimension IxJ,PxQ. 
}
\usage{
Kron(a, b)
}
\arguments{
  \item{a}{The first matrix of dimension IxP involved in the kronecker product.
}
  \item{b}{
The second matrix of dimension JxQ involved in the kronecker product.
}
}
\details{
This function is utilised from several other functions like \code{CA3variants}, 
\code{newcomp3}, \code{step.g3} and \code{reconst3}.
}
%\value{
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
\keyword{multivariate}
\keyword{product}% __ONLY ONE__ keyword per line
