\name{by.mro}
\alias{by.mro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
by.mro(mro.obj, formula, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mro.obj}{
%%     ~~Describe \code{mro.obj} here~~
}
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (mro.obj, formula, FUN, ...) 
{
    Data <- mro.obj$df
    Dframe <- mro.obj[[1]]
    if (formula == "~1") 
        return(FUN(Dframe, ...))
    Indice <- model.frame(formula, Data, na.action = na.pass)
    if (ncol(Indice) > 1) {
        Indna <- as.logical(rowSums(is.na(Indice)))
        Indice <- Indice[!Indna, ]
        Indice <- lapply(Indice, fastNumchange)
    }
    else {
        nm <- colnames(Indice)
        Indna <- is.na(Indice)
        Indice <- Indice[!Indna]
        Indice <- fastNumchange(Indice)
        Indice <- as.data.frame(Indice)
        colnames(Indice) <- nm
    }
    Dframe <- Dframe[!Indna, ]
    Data <- Data[!Indna, ]
    if (substitute(FUN) == "mroPara") {
        out <- by(Dframe, Indice, FUN, nonparallel = names(mro.obj)[1])
        class(out) <- c("by", "bymrocalc")
    }
    else {
        out <- by(Dframe, Indice, FUN, ...)
    }
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
