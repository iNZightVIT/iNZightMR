\name{typeofconflict}
\alias{typeofconflict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
typeofconflict(signiferr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signiferr}{
%%     ~~Describe \code{signiferr} here~~
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
function (signiferr) 
{
    upper.idx = (signiferr != 0) * (upper.tri(signiferr)) != 
        0
    type = signiferr[upper.idx]
    idx.r = row(signiferr)[upper.idx]
    idx.c = col(signiferr)[upper.idx]
    list(type = type, idx.r = idx.r, idx.c = idx.c)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
