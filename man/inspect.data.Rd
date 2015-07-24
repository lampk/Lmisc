\name{inspect.data}
\alias{inspect.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To check a dataset
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inspect.data(data, info, id, check_missing, plot = FALSE, prefix = "", outdir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset to be checked
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
invisible(inspect.data(formatted_dat, info = info, plot = FALSE, prefix = ""))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
