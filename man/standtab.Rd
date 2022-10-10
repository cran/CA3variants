\name{standtab}
\alias{standtab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Three-way centred column profile table for the three-way symmetric correspondence analysis
}
\description{
The function computes the three-way centred column profile table to perform 
three-way symmetric correspondence analysis.
}
\usage{
standtab(x, std = T, ctr = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The original three-way contingency table.
}
  \item{std}{
The flag parameter for weighting the original table. If F the original contingency table is not weighted.
}
  \item{ctr}{
The flag parameter for centering the original table. If F the original array is not centered.
}
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
\keyword{internal}
