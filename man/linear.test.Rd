\name{linear.test}
\alias{linear.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To assess linearity assumption of a statistical model
}
\description{
To assess linearity assumption of a statistical model
}
\usage{
linear.test(model, var, trans = c("quadratic", "spline"), df = 4, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
Fitted model to assess
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
linear.test(lm(Petal.Length ~ Species + Sepal.Width, data = iris), var = ~ Sepal.Width, data = iris)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
