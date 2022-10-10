\name{threewayboot}
\alias{threewayboot}
\title{
Generation of non-parametric bootstrap samples
}
\description{
This function allows to generate non-parametric bootstrap samples in order to check the 
optimal dimension number of three-way correspondence analysis. The boostrap samples 
have the same marginal proportions and the total number of the original table.
Do nboots bootstrap on the indicator matrix X (observations x (rows+cols+tubs) categories).
From a three-way contingency table, it makes the indicator using \code{makeindicator}.
The output is a list of three-way tables.
}
\usage{
threewayboot(Xdata,nboots=100)}
\arguments{
\item{Xdata}{
The three-way contingency array. It must be an \code{R} object array.
}
\item{nboots}{
The number of bootstrap samples to generate when \code{boots = T}. By default \code{nboots = 0}.
}
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
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.
}
\author{
Rosaria Lombardo, Michel van de Velden, Eric J Beh.
}
%\note{
%%  ~~further notes~~
%}
%\keyword{bootstrap}% use one of  RShowDoc("KEYWORDS")
\keyword{internal}% __ONLY ONE__ keyword per line
