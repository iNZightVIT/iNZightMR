\name{seModel}
\alias{seModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
seModel(model, idx, base = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{idx}{
%%     ~~Describe \code{idx} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
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
function (model, idx, base = TRUE) 
{
    covs = vcov(model)[idx, idx, drop = FALSE]
    vars = diag(covs)
    ses = sqrt(vars)
    ses.diffs = outer(vars, vars, "+") - 2 * covs
    diag(ses.diffs) = 0
    ses.diffs = sqrt(ses.diffs)
    if (base) {
        ses.diffs = rbind(c(0, ses), cbind(ses, ses.diffs))
        ses = c(0, ses)
    }
    ses.moecalc(ses, ses.diffs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
