\name{mroPara}
\alias{mroPara}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mroPara(obj, conf.levels = 1.96, nonparallel = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{conf.levels}{
%%     ~~Describe \code{conf.levels} here~~
}
  \item{nonparallel}{
%%     ~~Describe \code{nonparallel} here~~
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
function (obj, conf.levels = 1.96, nonparallel = NULL) 
{
    cl <- match.call()
    Tb <- deparse(substitute(obj))
    if (!is.null(nonparallel)) 
        Tb <- nonparallel
    if (inherits(obj, "mro")) 
        obj <- obj[[1]]
    n <- nrow(obj)
    estP <- colMeans(obj)
    SesDiff <- ses.diff(obj)
    covs <- cov(obj)/n
    variance <- diag(covs)
    mromoecalc2 <- moecalc(x = SesDiff, est = estP)
    mromoecalc2$fit$df <- n
    multi <- multicomp(mromoecalc2)
    out <- list(Topic = Tb, Variance = variance, Cov = covs, 
        Mromoecalc = mromoecalc2, Multicom = multi)
    class(out) <- "mrocalc"
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
