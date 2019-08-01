\name{simulabootstrat}
\alias{simulabootstrat}
\title{
Generation of parametric bootstrap samples
}
\description{
This function allows to generate parametric bootstrap samples in order to check the 
optimal dimension number of three-way correspondence analysis. The boostrap samples 
have the same marginal proportions and total number of the original table.
Te adopted sampling scheme is stratified.
}
\usage{
simulabootstrat(Xtable,nboots=100,resamptype=1)}
\arguments{
\item{Xtable}{
The three-way data array. It must be an \code{R} object array.
When non-symmetrical analysis for one response variable is performed, the response mode is the row variable.
}
\item{nboots}{
The number of bootstrap samples to generate when \code{boots = T}. By default \code{nboots = 0}.
}
\item{resamptype}{Set value of resamptype according to two methods: \code{resamptype=1} corresponds to multinomial distribution
and \code{resamptype=2} to Poisson distribution.}
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
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. Wiley.
}
\author{
Rosaria Lombardo, Michel van de Velden, Eric J Beh.
}
%\note{
%%  ~~further notes~~
%}
\keyword{bootstrap}% use one of  RShowDoc("KEYWORDS")
\keyword{dimension}% __ONLY ONE__ keyword per line
