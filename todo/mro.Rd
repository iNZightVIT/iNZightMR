\name{mro}
\alias{mro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mro(frm, data, Labels = NULL, inverse = FALSE, combi = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frm}{
%%     ~~Describe \code{frm} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{Labels}{
%%     ~~Describe \code{Labels} here~~
}
  \item{inverse}{
%%     ~~Describe \code{inverse} here~~
}
  \item{combi}{
%%     ~~Describe \code{combi} here~~
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
function (frm, data, Labels = NULL, inverse = FALSE, combi = NULL, 
    ...) 
{
    display <- with(data, {
        mro.mat <- model.frame(frm, data, na.action = na.pass, 
            ...)
        details <- attributes(mro.mat)
        variables <- attr(details$terms, "variables")
        mro.mat[is.na(mro.mat)] <- 0
        mro.mat <- sapply(mro.mat, fastBinaryChange, inverse)
        labelname <- if (is.null(Labels)) {
            attr(details$terms, "term.labels")
        }
        else if (storage.mode(Labels) == "function") {
            Labels(attr(details$terms, "term.labels"))
        }
        else {
            Labels
        }
        colnames(mro.mat) <- if (is.list(labelname)) 
            labelname$Varname
        else labelname
        if (!is.null(combi)) {
            combination.index <- combn(ncol(mro.mat), combi)
            nc <- ncol(combination.index)
            com.mro.mat <- vector("list", nc)
            com.lablename <- character(nc)
            for (j in seq_len(nc)) {
                com.mro.mat[[i]] <- mro.mat[, combination.index[1, 
                  j]] * mro.mat[, combination.index[2, j]]
                com.lablename[j] <- paste(labelname[combination.index[1, 
                  j]], labelname[combination.index[2, j]], sep = ":")
            }
            mro.mat <- do.call("cbind", com.mro.mat)
            colnames(mro.mat) <- com.lablename
            labelname <- com.lablename
        }
        Ix <- order(colSums(mro.mat), decreasing = TRUE)
        mro.mat <- mro.mat[, Ix]
        labelname <- labelname[Ix]
        out <- list(mro.mat = mro.mat, Labels = labelname, df = data)
        class(out) <- "mro"
        out
    })
    display
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
