\name{tau3}
\alias{tau3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partition of the Marcotorchino three-way index
}
\description{
When the association among three categorical variables is asymmetric such that one variable is a 
logical response variable to the other variables, we recommend calculating the non-symmetrical 
three-way measure of predictability such as the Marcotorchino index (Marcotorchino, 1985).
The function \code{tau3} partitions the Marcotorchino statistic when, in \code{CA3variants},
we set the parameter \code{ca3type = "NSCA3"}.
}
\usage{
tau3(f3, digits = 3)
}
\arguments{
  \item{f3}{
Three-way contingency array given as an input parameter in \code{CA3variants}.
}
  \item{digits}{
Number of decimal digits. By default digits=3.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{z}{The partition of the Marcotorchino index into three two-way association terms and one three-way 
association term. It also shows the C statistic of each term, its degrees of freedom and p-value.}
\item{CM}{the C statistic of the Marcotorchino index.}
\item{devt}{The denominator of the Marcotorchino index.}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}

\examples{
data(happy)
tau3(happy,digits=3)
}
\keyword{partition}
\keyword{index}% __ONLY ON__ keyword per line
