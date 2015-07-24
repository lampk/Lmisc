\name{format.each}
\alias{format.each}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To format each variable in a dataset
}
\description{
To format each variable in a dataset
}
\usage{
format.each(x, label = NA, type, unit = NA, scale = NA, center = NA, value = NA, levels = NA, missing = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
variable to format
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
test <- format.each(iris$Sepal.Length, type = "numeric")
str(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
