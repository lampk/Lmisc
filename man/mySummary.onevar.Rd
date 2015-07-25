\name{mySummary.onevar}
\alias{mySummary.onevar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To summarise one variable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mySummary.onevar(varname, variable, group = NULL, continuous = NA, contSummary = "med.IQR", test = FALSE, digits = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{
Variable to summarise
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Phung Khanh Lam (modified from Marcel Wolbers's code)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
mySummary.onevar("Species", iris$Sepal.Length, iris$Species)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
