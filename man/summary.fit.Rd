\name{summary.fit}
\alias{summary.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To display fitted model
}
\description{
To display fitted model
}
\usage{
summary.fit(fit, meta = NULL, exp = FALSE, method = 'LRT', digits = 2, sep = '-', p = 0.001, tidy = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
Fitted model to display
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
Phung Khanh Lam (original code from Marcel Wolbers)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fit <- lm(Sepal.Length ~ Species, data = iris)
summary.fit(fit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
