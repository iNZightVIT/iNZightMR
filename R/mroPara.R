#' Calculate MRO inference
#'
#' Calculates required proportions, their differences, variance-covariance
#' matrices, standard errors of differences, and comparison intervals for differences,
#' over all of the data. To compute values over various subsets of another
#' explanatory variable, see \link{by}.
#'
#' @param obj an MRO object created by \code{iNZightMR}
#' @param conf.levels confidence level to use, default is 1.96 for 95\% intervals
#' @param nonparallel Should these things be parallel?
#' @return An object of class \code{mrocalc}
#' @seealso \link{iNZightMR}
#' @export
#' @examples
#' mr <- iNZightMR(online ~ onlinegame + onlinevideo + onlinemusic,
#'     data = census.at.school.5000)
#' mrp <- mroPara(mr)
mroPara <- function(obj, conf.levels = 1.96, nonparallel=NULL) {
    cl <- match.call()
    Tb <- names(obj)[1]

    ## nonparallel is playing the role as changing the topic names
    if (! is.null(nonparallel))
        Tb <- nonparallel

    if (is.null(obj$design)) {

        if (inherits(obj, "mro"))
            obj <- obj[[1]]

        n <- nrow(obj)
        estP <- colMeans(obj)
        SesDiff <- seMRprops(obj)

        covs <- cov(obj) / n
        variance <- diag(covs) #*(n-1)/n

    } else {
        # handle survey design
        n <- nrow(obj[[1]])
        f <- eval(parse(text = sprintf("~%s", paste(obj$Labels, collapse = "+"))))
        estP <- coef(survey::svymean(f, obj$design))
        SesDiff <- seMRprops(obj$design, f)

        sv <- survey::svyvar(f, obj$design)
        covs <- var(sv)
        variance <- diag(covs)
    }

    mromoecalc2 <- moecalc(x = SesDiff, est = estP)
    mromoecalc2$fit$df <- n
    multi <- multicomp(mromoecalc2)
    out <- list(
        Topic = Tb,
        Variance = variance,
        Cov = covs,
        Mromoecalc = mromoecalc2,
        Multicom = multi
    )
    class(out) <- "mrocalc"
    out
}
