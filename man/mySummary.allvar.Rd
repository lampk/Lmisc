\name{mySummary.allvar}
\alias{mySummary.allvar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To summary a dataset
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mySummary.allvar(formula, data, pooledGroup = FALSE, contSummary = "med.IQR",
                             caption = NULL, kable = FALSE, test = FALSE, continuous = NA,
                             digits = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset to summarise
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
mySummary.allvar(Species ~ Sepal.Length + Sepal.Width, data = iris, pooledGroup = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
