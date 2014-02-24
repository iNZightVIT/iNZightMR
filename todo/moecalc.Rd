\name{moecalc}
\alias{moecalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
moecalc(x, factorname = NULL, levelnames = NULL, coef.idx = NULL, est = NULL, ci = NULL, base = TRUE, basename = "base", conf.level = 1.96)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{factorname}{
%%     ~~Describe \code{factorname} here~~
}
  \item{levelnames}{
%%     ~~Describe \code{levelnames} here~~
}
  \item{coef.idx}{
%%     ~~Describe \code{coef.idx} here~~
}
  \item{est}{
%%     ~~Describe \code{est} here~~
}
  \item{ci}{
%%     ~~Describe \code{ci} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{basename}{
%%     ~~Describe \code{basename} here~~
}
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
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
function (x, factorname = NULL, levelnames = NULL, coef.idx = NULL, 
    est = NULL, ci = NULL, base = TRUE, basename = "base", conf.level = 1.96) 
{
    obj <- x
    modelcall = NULL
    if (!any(class(obj) == "ses.moecalc")) {
        modelcall = obj$call
        if (!xor(is.null(factorname), is.null(coef.idx))) 
            stop("must have factorname or coefficient index only")
        if (length(unlist(strsplit(factorname, ":"))) > 2) 
            stop("Interactions must not have more than 2 terms")
        if (is.null(coef.idx)) 
            coef.idx = coefidx(obj, factorname)
        est = obj$coefficients[coef.idx]
        if (base) 
            est = c(0, est)
        temp = seModel(obj, coef.idx, base)
        ses = temp$ses
        ses.diffs = temp$ses.diffs
        if (!is.null(factorname)) {
            isfactor = chkfactor(obj, factorname)
            if (sum(isfactor) == 1) 
                levelnames = obj$xlevels[[names(isfactor)[which(isfactor)]]]
            if (sum(isfactor) == 2) {
                stop("Two-factor interactions not supported yet")
                levelnames = names(est)
                levelnames = gsub(names(which(isfactor)[1]), 
                  "", levelnames)
                levelnames = gsub(names(which(isfactor)[2]), 
                  "", levelnames)
            }
            if ((length(est) - 1) == length(levelnames)) {
                out = paste("length(est) is more than length(factor), may need to set base=FALSE")
                warning(out, call. = FALSE)
                levelnames = c(basename, levelnames)
            }
        }
        else {
            levelnames = names(obj$coefficients)[coef.idx]
            if (base) 
                levelnames = c(basename, levelnames)
        }
        out = paste("confidence interval of baseline is 0")
        warning(out)
    }
    else {
        if (!is.null(est)) {
            levelnames = names(est)
            est = as.numeric(est)
        }
        ses = obj$ses
        ses.diffs = obj$ses.diffs
    }
    n = if (is.null(est)) 
        NULL
    else length(est)
    xlevels = makexlv(factorname, levelnames, n)
    factorname = names(xlevels)[1]
    levelnames = xlevels[[factorname]]
    if (!is.null(est)) 
        names(est) = levelnames
    if (any(class(obj) == "ses.moecalc")) {
        moe.diffs = conf.level * ses.diffs
    }
    else {
        moe.diffs = ses.diffs
    }
    dimnames(moe.diffs) = list(levelnames, levelnames)
    k = ncol(moe.diffs)
    if (nrow(moe.diffs) != k | !is.matrix(moe.diffs) | k <= 1) 
        stop("moe.diffs must be square matrix")
    if (k >= 3) {
        keep = col(moe.diffs) > row(moe.diffs)
        k2 = sum(keep)
        if (!any(class(obj) == "ses.moecalc")) {
            multiplier = qtukey(0.95, k, obj$df.residual)/sqrt(2)
            moe.diffs = moe.diffs * multiplier
        }
        Xr = row(moe.diffs)[keep]
        Xc = col(moe.diffs)[keep]
        X = outer(1:k2, 1:k, function(x, y) {
            y == Xr[x] | y == Xc[x]
        })
        ErrBars = drop(solve(crossprod(X)) \%*\% t(X) \%*\% moe.diffs[keep])
    }
    else {
        if (!is.null(ci)) {
            if (length(ci) != 2) 
                stop("When dimension=2, must have length(ci)=2")
            ErrBars = moe.diffs[1, 2] * ci/sum(ci)
        }
        else ErrBars = moe.diffs[1, 2] * 0.5 * c(1, 1)
    }
    moe.diffs.approx = outer(ErrBars, ErrBars, "+")
    diag(moe.diffs.approx) = 0
    names(ErrBars) = levelnames
    dimnames(moe.diffs.approx) = list(levelnames, levelnames)
    errpercent = 100 * round(((moe.diffs.approx - moe.diffs)/moe.diffs), 
        2)
    diag(errpercent) = 0
    signiferr = NULL
    if (!is.null(est)) {
        if (length(est) != k) 
            stop("length(est) must=ncol(moe.diffs)")
        est.diffs = outer(est, est, "-")
        ciL = est.diffs - moe.diffs
        ciU = est.diffs + moe.diffs
        ciL.approx = est.diffs - moe.diffs.approx
        ciU.approx = est.diffs + moe.diffs.approx
        signiftrue = (sign(ciL) + sign(ciU) == 0)
        signifapprox = (sign(ciL.approx) + sign(ciU.approx) == 
            0)
        signiferr = signiftrue - signifapprox
        if (!is.null(ci)) 
            diag(moe.diffs) = ci
    }
    confL = confU = compL = compU = NULL
    if (!is.null(est)) {
        confL = est - conf.level * ses
        confU = est + conf.level * ses
        compL = est - ErrBars
        compU = est + ErrBars
        names(confL) = names(confU) = names(compL) = names(compU) = levelnames
    }
    ret = list(fit = obj, est = est, est.diffs = est.diffs, ErrBars = ErrBars, 
        errpercent = errpercent, MaxErrProp = errpercent[which.max(abs(errpercent))], 
        signiferr = signiferr, moe.diffs = moe.diffs, moe.diffs.approx = moe.diffs.approx, 
        modelcall = modelcall, xlevels = xlevels, ses = ses, 
        ses.diffs = ses.diffs, confL = confL, confU = confU, 
        compL = compL, compU = compU)
    class(ret) = "moecalc"
    if (abs(ret$MaxErrProp) >= 1) 
        warningErrProp(ret)
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
