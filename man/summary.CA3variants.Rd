\name{summary.CA3variants}
\alias{summary.CA3variants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of three-way correspondence analysis results
}
\description{
This function prints the summary of the  results of three-way symmetrical or non-symmetrical correspondence analysis. 
In particular it gives information on inertia and index partitioning.
If the input parameter, in \code{CA3variants}, is \code{ca3type="CA3"}, the function gives the Pearson index partitioning 
for three-way symmetrical correspondence analysis.
If the input parameter, in \code{CA3variants}, is \code{ca3type="NSCA3"},
the function gives  the Marcotorchino index partitioning for three-way non-symmetrical correspondence analysis.
If the input parameter, in \code{CA3variants}, is \code{ca3type="OCA3"},
the function gives  the Pearson index partitioning for ordered three-way symmetrical correspondence analysis.
If the input parameter, in \code{CA3variants}, is \code{ca3type="ONSCA3"},
the function gives  the Marcotorchino index partitioning for ordered three-way non-symmetrical correspondence analysis.
}
\usage{
\S3method{summary}{CA3variants}(object, digits=3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
The name of the output of the main function \code{CA3variants}.
}
  \item{digits}{
The input parameter specifying the digital number. By default, \code{digits = 3}.
}
\item{...}{Further arguments passed to or from other methods.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The value of output returned in short depends on the kind of three-way correspondence analysis variant performed.
Depending on the variant of three-way correspondence analysis performed, it gives  the explained inertia,
the total inertia  and the index partitioning.
}
\references{
Beh EJ and Lombardo R (2014) Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\cr
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
ris.ca3<-CA3variants(happy, dims= c(p=2,q=2,r=2), ca3type = "CA3") 
summary(ris.ca3)
ris.nsca3<-CA3variants(happy, dims = c(p=2,q=2,r=2), ca3type = "NSCA3") 
summary(ris.nsca3)
ris.oca3<-CA3variants(happy, dims = c(p=3,q=5,r=4), ca3type = "OCA3",norder=3) 
summary(ris.oca3)
ris.onsca3<-CA3variants(happy, dims = c(p=3,q=5,r=4), ca3type = "ONSCA3",norder=3) 
summary(ris.onsca3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
\keyword{partition}% __ONLY ONE__ keyword per line
