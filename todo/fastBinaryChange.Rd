\name{fastBinaryChange}
\alias{fastBinaryChange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fastBinaryChange(x, inverse = FALSE, opts = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{inverse}{
%%     ~~Describe \code{inverse} here~~
}
  \item{opts}{
%%     ~~Describe \code{opts} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, inverse = FALSE, opts = NULL) 
{
    x <- as.ordered(x)
    n <- length(x)
    typecase <- storage.mode(x)
    if (storage.mode(x) == "complex") 
        stop("Numeric variables must not be complex numbers")
    if (length(levels(x)) > 2) 
        stop("Non binary level variable")
    if (length(levels(x)) == 1) 
        typecase <- "unary"
    bin <- if (inverse) 
        1:0
    else 0:1
    if (!is.null(opts)) {
        typecase <- "special"
        if (length(as.character(opts)) != 2) 
            stop("'opts' should be a character vector of length 2")
    }
    if (typecase == "unary") {
        levels(x) <- if (inverse) 
            0
        else 1
        x <- suppressWarnings(as.numeric(as.character(x)))
    }
    else if (typecase == "special") {
        tmpf <- factor(x, levels = opts, labels = 0:1)
        x <- suppressWarnings(as.numeric(as.character(tmpf)))
    }
    else {
        levels(x) <- bin
        x <- suppressWarnings(as.numeric(as.character(x)))
    }
    x[is.na(x)] <- if (inverse) 
        1
    else 0
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
