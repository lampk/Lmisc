\name{report.error}
\alias{report.error}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To report error
}
\description{
To report error
}
\usage{
report.error(data, id, criteria, message, csv.file, print = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset to check
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
info <- data.frame(varname = c("Sepal.Length", "Petal.Width", "Species"),
                   label = c("Length of sepal", "Width of petal", "Species"),
                   type = c("numeric", "numeric", "factor"),
                   unit = NA,
                   value = NA,
                   levels = NA,
                   missing = NA)
formatted_dat <- format.data(iris, info = info)
report.error(data = "formatted_dat", criteria = "Sepal.Length < 5", message = "Sepal.Length < 5")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
