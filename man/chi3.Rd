\name{chi3}
\alias{chi3}
\title{
The partition of the Pearson three-way index
}
\description{
When three categorical variables are symmetrically related, we can analyse the strength of
the association using the three-way Pearson mean square contingency coefficient, named the chi-squared index.
The function \code{chi3} partitions the Pearson phi-squared statistic when in \code{CA3variants}
we set the parameter \code{ca3type = "CA3"}.
}
\usage{
chi3(f3, digits = 3)
}
\arguments{
\item{f3}{
The three-way contingency array given as an input parameter in CA3variants.
}
\item{digits}{
The number of decimal digits. By default digits=3.
}
}
\value{
The partition of the Pearson index into three two-way association terms and one three-way 
association term. It also shows the explained inertia, the degrees of freedom and 
p-value of each term of the partition.
}
\references{
Beh EJ and Lombardo R (2014). Correspondence Analysis, Theory, Practice and New Strategies. John Wiley & Sons.\\
Carlier A and Kroonenberg P (1996). Decompositions and biplots in three-way correspondence analysis. Psychometrika, 61, 355-373.
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
\examples{
data(happy)
chi3(f3=happy, digits=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{three-way index}% __ONLY ONE__ keyword per line
