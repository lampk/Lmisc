\name{summary.uni}
\alias{summary.uni}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To display univariate analysis
}
\description{
To display univariate analysis
}
\usage{
summary.uni(fit, adjust = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
Multivariable model includes all variables for univariate analysis
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
Phung Khanh Lam
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fit <- lm(Sepal.Length ~ Species + Sepal.Width + Petal.Width, data = iris)
summary.uni(fit, adjust = "Petal.Length")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
