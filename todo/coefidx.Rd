\name{coefidx}
\alias{coefidx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coefidx(model, labelnames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{labelnames}{
%%     ~~Describe \code{labelnames} here~~
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
function (model, labelnames) 
{
    term = model$terms
    labelnames.idx = which(attr(term, "term.labels") \%in\% labelnames)
    if (grepl(":", labelnames, fixed = TRUE)) {
        if (!any(labelnames.idx)) {
            labelnames = paste(rev(unlist(strsplit(labelnames, 
                ":"))), collapse = ":")
            labelnames.idx = which(attr(term, "term.labels") \%in\% 
                labelnames)
        }
    }
    coef.idx = attr(model.matrix(model), "assign") == labelnames.idx
    coef.idx
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
