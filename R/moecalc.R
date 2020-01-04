#' Computes the margin of error for various objects.
#'
#' If \code{x} is a model, must have factorname or coefficient index (coef.idx)
#' If input factorname, will compute ErrBars by factorname (for given model)
#' If input coefficient index, will compute ErrBars simply by index only (even they are not factor)
#' If \code{x} is \code{ses.moecalc} object, will compute ErrBars simply by given \code{ses.moecalc} object
#' @title Margin of Error Calculation
#' @param x the object for which we compute margins of error
#' @param factorname name of factor
#' @param levelnames names of factor levels
#' @param coef.idx index of coefficient to use
#' @param est estimates
#' @param ci confidence intervals
#' @param base baseline
#' @param basename name of baseline
#' @param conf.level level of confidence to use
#' @return a \code{moecalc} object
#' @author Junjie Zeng
#' @export
moecalc <- function(x, factorname = NULL, levelnames = NULL, coef.idx = NULL,
                    est = NULL, ci = NULL, base = TRUE, basename = "base",
                    conf.level = 1.96) {
    obj <- x
    modelcall <- NULL
    ## obtain standard error difference matrix
    if (!any(class(obj) == "ses.moecalc")) {
        modelcall <- obj$call

        if(!xor(is.null(factorname), is.null(coef.idx)))
            stop("must have factorname or coefficient index only")

        if (length(unlist(strsplit(factorname, ":"))) > 2)
            stop("Interactions must not have more than 2 terms")

        if (is.null(coef.idx))
            coef.idx <- coefidx(obj, factorname)

        est <- obj$coefficients[coef.idx]
        if (base)
            est <- c(0, est)

        temp <- seModel(obj, coef.idx, base)
        ses <- temp$ses
        ses.diffs <- temp$ses.diffs

        ## Guess levelnames
        if (!is.null(factorname)){
            ## by factorname
            isfactor <- chkfactor(obj, factorname)
            if (sum(isfactor) == 1)
                levelnames <- obj$xlevels[[ names(isfactor)[which(isfactor)] ]]

            if (sum(isfactor) == 2) {
                ## For now, don't do anything
                stop("Two-factor interactions not supported yet")

                levelnames = names(est)
                levelnames = gsub(names(which(isfactor)[1]), "", levelnames)
                levelnames = gsub(names(which(isfactor)[2]), "", levelnames)
                # levelnames = gsub(":", "/", levelnames)
            }

            if ((length(est) - 1) == length(levelnames)) {
                out <- paste("length(est) is more than length(factor), may need to set base=FALSE")
                warning(out, call. = FALSE)
                levelnames <- c(basename, levelnames)
            }
        } else {
            ## by coef.idx
            levelnames = names(obj$coefficients)[coef.idx]
            if (base)
                levelnames = c(basename, levelnames)
        }
        out = paste("confidence interval of baseline is 0")
        warning(out)

    } else {
        if (!is.null(est)) {
            levelnames <- names(est)
            est <- as.numeric(est)
        }

        ses <- obj$ses
        ses.diffs <- obj$ses.diffs
    }

    n <- if (is.null(est)) NULL else length(est)
    xlevels <- makexlv(factorname, levelnames, n)
    factorname <- names(xlevels)[1]
    levelnames <- xlevels[[factorname]]
    if (!is.null(est))
        names(est) <- levelnames

    ## Margins of error for differences
    if (any(class(obj) == "ses.moecalc")) {
        moe.diffs = conf.level * ses.diffs
    } else {
        moe.diffs = ses.diffs
    }
    dimnames(moe.diffs) <- list(levelnames, levelnames)

    k <- ncol(moe.diffs)
    if (nrow(moe.diffs) != k | !is.matrix(moe.diffs) | k <= 1)
        stop("moe.diffs must be square matrix")

    if (k >= 3) { # get ErrBars by solving least squares problem
        keep <- col(moe.diffs) > row(moe.diffs) # Upper triangle, above diag.
        k2 <- sum(keep) # number of unique moe.diffs for diffs without redundancies
        if (!any(class(obj) == "ses.moecalc")) {
            #%%%## Multiple comparisons adjustment
            multiplier = qtukey(0.95, k, obj$df.residual) / sqrt(2)
            moe.diffs = moe.diffs * multiplier
        }
        Xr <- row(moe.diffs)[keep] # (going down cols)
        Xc <- col(moe.diffs)[keep]
        ## each row of X contains 2 ones, representing a pair of levels to split moe between
        X <- outer(1:k2, 1:k,
            function(x, y) {
                y == Xr[x] | y == Xc[x]
            }
        )
        ## crossprod same as t(x) %*% x
        ErrBars <- drop(solve(crossprod(X)) %*% t(X) %*% moe.diffs[keep])
    } else {
        # k=2 have infinite number of solutions. Make bar lengths proportional to ci
        if (!is.null(ci)) {
            if (length(ci) != 2)
                stop("When dimension=2, must have length(ci)=2")

            ErrBars <- moe.diffs[1, 2] * ci / sum(ci)
        } else {
            ErrBars <- moe.diffs[1, 2] * 0.5 * c(1,1)
        }
    }

    moe.diffs.approx <- outer(ErrBars,ErrBars,'+')
    diag(moe.diffs.approx) <- 0

    names(ErrBars) <- levelnames
    dimnames(moe.diffs.approx) <- list(levelnames, levelnames)

    errpercent <- 100 * round(((moe.diffs.approx - moe.diffs) / moe.diffs), 2)
    diag(errpercent) <- 0
    signiferr <- NULL

    if (!is.null(est)) {
        if (length(est) != k)
            stop("length(est) must=ncol(moe.diffs)")
        est.diffs <- outer(est, est, '-')
        ciL <- est.diffs - moe.diffs
        ciU <- est.diffs + moe.diffs
        ciL.approx <- est.diffs - moe.diffs.approx
        ciU.approx <- est.diffs + moe.diffs.approx
        # Trap situations where Errbars overlap but CIs do not contain zero or
        # ErrBars do not overlap but CIs contain zero
        signiftrue <- (sign(ciL) + sign(ciU) == 0)
        signifapprox <- (sign(ciL.approx) + sign(ciU.approx) == 0)
        signiferr <- signiftrue - signifapprox
        if (!is.null(ci))
            diag(moe.diffs) <- ci # put ci of estimates on redundant diagonal
    }

    confL <- confU <- compL <- compU <- NULL
    if (!is.null(est)) {
        # confidence limits
        confL <- est - conf.level * ses
        confU <- est + conf.level * ses
        # comparison limits
        compL <- est - ErrBars
        compU <- est + ErrBars
        names(confL) <- names(confU) <- names(compL) <- names(compU) <- levelnames
    }

    ret <- list(fit = obj, est = est, est.diffs = est.diffs, ErrBars = ErrBars, errpercent = errpercent,
                MaxErrProp = errpercent[which.max(abs(errpercent))],
                signiferr = signiferr, moe.diffs = moe.diffs,
                moe.diffs.approx = moe.diffs.approx, modelcall = modelcall,
                xlevels = xlevels, ses = ses, ses.diffs = ses.diffs,
                confL = confL, confU = confU, compL = compL, compU = compU)
    class(ret) <- "moecalc"

    if(abs(ret$MaxErrProp) >= 1)
        warningErrProp(ret)

    ret
}

#' @export
print.moecalc <- function(x, ...) {
    obj <- x
    if (!is.null(obj$est)) {
        out <- cbind(obj$est, obj$ErrBars, obj$compL, obj$compU)
        colnames(out) <- c("Est", "ErrBar", "compL", "compU")
        rownames(out) <- obj$xlevels[[1]]
        print(out)
    } else {
        cat("Errbars\n")
        print(obj$ErrBars)
    }
}

#' @export
summary.moecalc <- function(object, ...) {
    obj <- object
    factorname <- names(obj$xlevels)[1]
    levelnames <- obj$xlevels[[factorname]]

    if (!is.null(obj$est)) {
        coeff <- cbind(
            obj$est, obj$ErrBars, obj$compL, obj$compU,
            obj$confL, obj$confU
        )
        colnames(coeff) <- c("Est", "ErrBar", "compL", "compU", "confL", "confU")
        rownames(coeff) <- levelnames
    } else{
        coeff <- obj
    }

    conflict <- NULL
    if (!is.null(obj$signiferr))
        if (any(obj$signiferr != 0))
            conflict <- typeofconflict(obj$signiferr)

    x <- list(
        coeff = coeff,
        conflict = conflict,
        modelcall = obj$modelcall,
        MaxErrProp = obj$MaxErrProp,
        xlevels = obj$xlevels
    )
    class(x) = "summary.moecalc"
    x
}

#' @export
print.summary.moecalc <- function(x, ...) {
    obj <- x
    if (!is.null(obj$modelcall)) {
        cat("Call:\n")
        print(obj$modelcall)
        cat("\n")
    }

    print(obj$coeff)
    out <- paste0(
        "\nMax error betw. approx. and true moe is ",
        obj$MaxErrProp, "%")
    cat(out, "\n")

    if (!is.null(obj$conflict))
        warningConflict(obj$xlevels, obj$conflict)
}


#' @export
plot.moecalc <- function(x, horiz = FALSE, xlevels = NULL, ...) {

    ## Disable confidence intervals for now
    conf <- FALSE

    obj <- x
    if (is.null(obj$est))
        stop("No estimates, cannot plot interval")

    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))
    n <- length(obj$est)

    if  (is.null(xlevels))
        xlevels <- obj$xlevels
    factorname <- names(xlevels)[1]
    levelnames <- xlevels[[factorname]]

    ## Special case if we are plotting an interaction
    ## Need to split into panels for each level of one of the factors
    isInteraction <- FALSE
    if (all(grepl(":", names(obj$est)[-1]))) {
        isInteraction <- TRUE
        twoFactors <- unlist(strsplit(factorname, ":"))
        ## Number of panels needed
        nPanels <- length(levels(obj$fit$model[,twoFactors[1]])) - 1
        sqrt.nPanels <- sqrt(nPanels)

        ## Make the plot array as square as possible
        nRows <- ifelse(sqrt.nPanels == floor(sqrt.nPanels),
            sqrt.nPanels, ceiling(sqrt.nPanels)
        )
        nCols <- ifelse(sqrt.nPanels == floor(sqrt.nPanels),
            nRows, ceiling(nPanels / nRows)
        )
        layout(
            matrix(
                c(
                    rep(nPanels + 1, nCols),
                    c(1:nPanels, rep(0, nRows * nCols - nPanels))
                ),
                nrow = nRows + 1,
                byrow = TRUE
            ),
            widths = rep(1 / nRows, nRows),
            heights = c(lcm(1.9), rep(1 / nCols, nCols))
        )
    }

    if (isInteraction)
        opar <- par(mar = c(2.5, 4.1, 2.1, 2.1))

    comparisonCol <- "red"
    confidenceCol <- "black"
    comparisonLwd <- 3
    confidenceLwd <- 1

    if (horiz) {
        if (!isInteraction) {
            ylim <- c(1, n) + c(-0.5, 0.5)
            print(ylim)
            xlim <- range(obj$confL, obj$confU, obj$compL, obj$compU)
            plot(xlim, ylim, type = "n", axes = F, ylab = factorname, xlab = "estimate")
            axis(2, at = 1:n, labels = levelnames)
            axis(1)
            box()
            ## Dotted line at 0
            lines(rep(0, 2), ylim, lty = "dashed", col = "black")
            ## Dotted lines along comparison interval endpoints
            segments(obj$compU, rep(ylim[1], n), obj$compU, rep(ylim[2], n),
                lty = "dotted",
                col = "grey"
            )
            segments(obj$compL, rep(ylim[1], n), obj$compL, rep(ylim[2], n),
                lty = "dotted",
                col = "grey"
            )
            if (conf)
                segments(obj$confL, 1:n, obj$confU, 1:n,
                    lwd = confidenceLwd,
                    col = confidenceCol
                )
            segments(obj$compL, 1:n, obj$compU, 1:n,
                lwd = comparisonLwd,
                col = comparisonCol
            )
            points(obj$est, 1:n, pch = 19)
            ## Thick bar for baseline
            lines(rep(0, 2), 1 + c(-1, 1) * 0.1, lwd = 3)
        } else {
            nLevelsPerPanel <- (n - 1) / nPanels
            for (i in 1:nPanels) {
                indices <- c(1, (i - 1) * nLevelsPerPanel + 1:nLevelsPerPanel + 1)
                ylim <- c(1, nLevelsPerPanel + 1) + c(-0.5, 0.5)
                xlim <- range(
                    obj$confL[indices],
                    obj$confU[indices],
                    obj$compL[indices],
                    obj$compU[indices]
                )
                plot(xlim, ylim, type = "n", axes = F, ylab = "", xlab = "")
                ylabs <- levels(obj$fit$model[,twoFactors[1]])
                axis(2, at = 1:(nLevelsPerPanel + 1), labels = ylabs)
                axis(1)
                box()
                mtext(levels(obj$fit$model[,twoFactors[2]])[i+1], 3,
                    cex = .75,
                    line = .5
                )
                lines(rep(0, 2), ylim, lty = "dashed", col = "black")
                lines(rep(0, 2), 1 + c(-1, 1) * 0.1, lwd = 3)
                segments(
                    obj$compU[indices],
                    rep(ylim[1], length(indices)),
                    obj$compU[indices],
                    rep(ylim[2], length(indices)),
                    lty = "dotted",
                    col = "grey"
                )
                segments(
                    obj$compL[indices],
                    rep(ylim[1], length(indices)),
                    obj$compL[indices],
                    rep(ylim[2], length(indices)),
                    lty = "dotted",
                    col = "grey"
                )
                if (conf) {
                    segments(
                        obj$confL[indices],
                        1:(nLevelsPerPanel + 1),
                        obj$confU[indices],
                        1:(nLevelsPerPanel + 1),
                        lwd = confidenceLwd,
                        col = confidenceCol
                    )
                }
                segments(
                    obj$compL[indices],
                    1:(nLevelsPerPanel + 1),
                    obj$compU[indices],
                    1:(nLevelsPerPanel + 1),
                    lwd = comparisonLwd,
                    col = comparisonCol
                )
                points(obj$est[indices], 1:(nLevelsPerPanel + 1), pch = 19)
            }
        }
    } else {
        if (!isInteraction) {
            xlim <- c(1, n) + c(-0.5, 0.5)
            ylim <- range(obj$confL, obj$confU, obj$compL, obj$compU)
            plot(xlim, ylim, type = "n", axes = F, xlab = factorname, ylab = "estimate")
            axis(1, at = 1:n, labels = levelnames)
            axis(2)
            box()
            lines(xlim, rep(0, 2), lty = "dashed", col = "black")
            segments(rep(xlim[1], n), obj$compU, rep(xlim[2], n), obj$compU,
                lty = "dotted",
                col = "grey"
            )
            segments(rep(xlim[1], n), obj$compL, rep(xlim[2], n), obj$compL,
                lty = "dotted",
                col = "grey"
            )
            if (conf)
                segments(1:n, obj$confL, 1:n, obj$confU,
                    lwd = confidenceLwd,
                    col = confidenceCol
                )

            segments(1:n, obj$compL, 1:n, obj$compU,
                lwd = comparisonLwd,
                col = comparisonCol
            )
            points(1:n, obj$est, pch = 19)
            lines(1 + c(-1, 1) * 0.1, rep(0, 2), lwd = 3)
        } else {
            nLevelsPerPanel <- (n - 1) / nPanels
            for (i in 1:nPanels) {
                indices <- c(1, (i - 1) * nLevelsPerPanel + 1:nLevelsPerPanel + 1)
                xlim <- c(1, nLevelsPerPanel + 1) + c(-0.5, 0.5)
                ylim <- range(
                    obj$confL[indices],
                    obj$confU[indices],
                    obj$compL[indices],
                    obj$compU[indices]
                )
                plot(xlim, ylim, type = "n", axes = F, ylab = "", xlab = "")
                xlabs <- levels(obj$fit$model[,twoFactors[1]])
                axis(1, at = 1:(nLevelsPerPanel + 1), labels = xlabs)
                axis(2)
                box()
                mtext(levels(obj$fit$model[,twoFactors[2]])[i+1], 3,
                    cex = .75,
                    line = .5
                )
                lines(xlim, rep(0, 2), lty = "dashed", col = "black")
                segments(
                    rep(xlim[1], length(indices)),
                    obj$compU[indices],
                    rep(xlim[2], length(indices)),
                    obj$compU[indices],
                    lty = "dotted",
                    col = "grey"
                )
                segments(
                    rep(xlim[1], length(indices)),
                    obj$compL[indices],
                    rep(xlim[2], length(indices)),
                    obj$compL[indices],
                    lty = "dotted",
                    col = "grey"
                )

                if (conf) {
                    segments(
                        1:(nLevelsPerPanel + 1),
                        obj$confL[indices],
                        1:(nLevelsPerPanel + 1),
                        obj$confU[indices],
                        lwd = confidenceLwd,
                        col = confidenceCol
                    )
                }
                segments(
                    1:(nLevelsPerPanel + 1),
                    obj$compL[indices],
                    1:(nLevelsPerPanel + 1),
                    obj$compU[indices],
                    lwd = comparisonLwd,
                    col = comparisonCol
                )
                points(1:(nLevelsPerPanel + 1), obj$est[indices], pch = 19)
                ## Thick black bar on baseline bar
                lines(1 + c(-1,1) * 0.1, rep(0, 2), lwd = 3)
            }
        }
    }

    if (isInteraction) {
        par(opar)
        opar <- par(mar = rep(0.3, 4))
        plot.new()
        text(0.5, 0.75, paste("Factor level effects for", factorname),
            cex = 1.75,
            font = 2
        )
    } else {
        title(
            paste(
                "Factor level effects of", factorname,
                "on", attr(obj$fit$model, "names")[1]
            )
        )
    }

    ## Legend
    legendNames <- "Comparison interval "
    legendCols <- comparisonCol
    legendLwds <- comparisonLwd
    if (conf) {
        legendNames <- c(legendNames, "Confidence interval")
        legendCols <- c(legendCols, confidenceCol)
        legendLwds <- c(legendLwds, confidenceLwd)
    }
    if (isInteraction) {
        legendX <- 0.5
        legendY <- 0.5
        xjust <- 0.5
        legendCex <- 1.4
    } else {
        xyLims <- par("usr")
        legendX <- xyLims[1] + .02 * diff(xyLims[1:2])
        legendY <- xyLims[4] + .02 * diff(xyLims[3:4])
        xjust <- 0
        legendCex <- 1
    }
    legend(x = legendX, y = legendY, legendNames,
        col = legendCols,
        lwd = legendLwds,
        bty = "n",
        horiz = TRUE,
        xjust = xjust,
        cex = legendCex
    )

    if (obj$MaxErrProp >= 1)
        warningErrProp(obj)
}

warningErrProp <- function(obj) {
    out <- paste0("Max error betw. approx. and true moe is ",
        obj$MaxErrProp, "%"
    )
    warning(out, call. = FALSE)
}

## obj : xlevels
## conflict: output from function "typeofconflict"
warningConflict <- function(xlevels, conflict) {
    factorname <- names(xlevels)[1]
    levelnames <- xlevels[[factorname]]
    x <- conflict
    cat("Significance conflict: \n")

    isovlp <- (x$type != 1)
    if (any(isovlp)) {
        cat(" should overlap between factor", factorname, ":\n")
        for(i in isovlp)
        cat(" ", levelnames[x$idx.r[i]], "and", levelnames[x$idx.c[i]], "\n")
    }
    if (any(!isovlp)) {
        cat(" should not overlap between factor:\n")
        for(i in !isovlp)
        cat(" ", levelnames[x$idx.r[i]], "and", levelnames[x$idx.c[i]], "\n")
    }
}

## identify type of conflict by signiferr matrix
typeofconflict <- function(signiferr) {
    upper.idx <- (signiferr != 0) * (upper.tri(signiferr)) != 0
    type <- signiferr[upper.idx]
    idx.r <- row(signiferr)[upper.idx]
    idx.c <- col(signiferr)[upper.idx]
    ## type == 1 : CI not overlap, ErrBars overlap
    ## type == -1 : CI overlap, ErrBars not overlap
    list(type = type, idx.r = idx.r, idx.c = idx.c)
}



## find coeff. index of model by given names
coefidx <- function(model, labelnames) {
    term <- model$terms
    labelnames.idx <- which(attr(term, "term.labels") %in% labelnames)
    ## if interaction, make it work for var1:var2 and var2:var1
    if (grepl(":", labelnames, fixed = TRUE)) {
        if (!any(labelnames.idx)) {
            labelnames <- paste(rev(unlist(strsplit(labelnames, ":"))), collapse = ":")
            labelnames.idx <- which(attr(term, "term.labels") %in% labelnames)
        }
    }
    coef.idx <- attr(model.matrix(model), "assign") == labelnames.idx
    coef.idx
}

## check variables for given labelnames is factor or not
chkfactor <- function(model, labelnames) {
    term <- model$terms
    labelnames.idx <- attr(term, "term.labels") %in% labelnames
    ## if interaction, make it work for var1:var2 and var2:var1
    if (grepl(":", labelnames, fixed = TRUE)) {
        if (!any(labelnames.idx)) {
            labelnames <- paste(rev(unlist(strsplit(labelnames, ":"))), collapse = ":")
            labelnames.idx <- attr(term, "term.labels") %in% labelnames
        }
    }
    if(sum(labelnames.idx)!=1)
        stop("not only one labelnames is matched")

    vars.idx <- (attr(term, "factors")[, labelnames] != 0)
    vars <- names(vars.idx)[vars.idx]
    isfactor <- attr(term, "dataClasses")[vars.idx] == "factor"
    isfactor
}

## generate xlevels by factorname, levelnames or number of levels
makexlv <- function(factorname = NULL, levelnames = NULL, n = NULL) {
    if (all(is.null(c(factorname, levelnames, n))))
        return(NULL)

    if (is.null(factorname))
        factorname <- "Level"
    if (!is.null(n) && is.null(levelnames))
        levelnames <- 1:n

    temp <- list(levelnames)
    names(temp) <- factorname
    temp
}



multicomp <- function(x, ...) {
    UseMethod("multicomp")
}

multicomp.moecalc <- function(x, ...) {
    if (! is.null(x$est)) {
        cols <- c("Estimate", "Lower", "Upper", "p-value (unadj.)")
        levelnames <- x$xlevels[[1]]
        k <- length(levelnames)
        nr <- k * (k - 1) / 2
        result.matrix <- matrix(0, nrow = nr, ncol = length(cols))
        row <- 1
        names <- character(nr)
        df <- x$fit$df

        for (i in 1:(k - 1)) {
            for (j in (i + 1):k) {
                est <- - x$est.diffs[j, i]
                bounds <- est + c(-1, 1) * x$moe.diffs[j, i]
                # TODO: Adjust p-values for multiple comparisons using Tukey or Bonf
                pval <- pt(abs(est / x$ses.diffs[j, i]), df = df, lower.tail = FALSE)
                result.matrix[row, ] <- c(est, bounds, pval)
                names[row] <- paste(levelnames[i], " - ", levelnames[j])
                row <- row + 1
            }
        }
        rownames(result.matrix) <- names
        colnames(result.matrix) <- cols
        class(result.matrix) <- "multicomp"
        result.matrix
    } else {
        cat("Errbars\n")
        print(x$ErrBars)
    }
}

print.multicomp <- function(x, ...) {
    printCoefmat(x, P.values = TRUE, has.Pvalue = TRUE, ...)
    invisible(x)
}
