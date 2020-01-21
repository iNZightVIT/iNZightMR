r01 <- function(x, inverse = FALSE, opts = NULL) {

    # r01 will do the following task:
    # test1: not accept non binary level variable
    # test2: zero-variance variable, call "unary"
    # change1: no/yes => 0/1 false/true=>0/1

    x <- as.ordered(x)
    n <- length(x)
    typecase <- storage.mode(x)

    if (storage.mode(x) == "complex")
        stop("Numeric variables must not be complex numbers")

    # not accept non binary variable
    if (length(levels(x)) > 2)
        stop("Non binary level variable")
    # a special care of the unary variable.
    if (length(levels(x)) == 1)
        typecase <- "unary"

    # 2 mode of binary. because the order is following alphabetic
    # or numeric, we may want a inverse to fix question that researcher
    # are interested people not response.
    bin <- if (inverse) 1:0 else 0:1

    if (! is.null(opts)) {
        typecase <- "special"
        if (length(as.character(opts)) != 2)
            stop("'opts' should be a character vector of length 2")
    }

    if (typecase == "unary") {
        levels(x) <- if (inverse) 0 else 1
        x <- suppressWarnings(as.numeric(as.character(x)))
    } else if (typecase == "special") {
        tmpf <- factor(x, levels = opts, labels = 0:1)
        x <- suppressWarnings(as.numeric(as.character(tmpf)))
    } else {
        levels(x) <- bin
        x <- suppressWarnings(as.numeric(as.character(x)))
    }
    x[is.na(x)] <- if (inverse) 1 else 0
    x
}


#' Create iNZightMR multiple response object (MRO)
#'
#' Creates a multiple response object (MRO) containing binary response matrix
#' (zeros and ones) as well as the input data source.
#'
#' The individual response variable names can be detected from the
#' variable name by passing \code{Labels = substrsplit}.
#' For example, in `ethniceng` and `ethnicmri`, `ethnic` is common
#' to both, so the labels will be `eng` and `mri`.
#'
#' If a user wants to inverse the response (zeros becomes ones), then pass
#' \code{inverse = TRUE}. This is useful when the responses are characters
#' (such as "correct" and "wrong", where correct would be given a zero)
#' and the order needs reversing (so that correct is 1 instead).
#'
#' @param frm formula containing the response variables
#' @param data a data.frame containing response and explanatory variables
#' @param Labels labels for the response categories; by default, the function
#'               will attempt to. Can also be the function \code{substrsplit},
#'               which will detect a common base in the variables
#'               (see Details)
#' @param inverse if \code{TRUE}, binary responses will be reversed (see details)
#' @param ... additional arguments, passed to \code{\link{model.frame}}
#' @return An \code{mro} object containing a multiple response binary matrix and input
#'   data source
#' @seealso \link{barplotMR}, \link{mroPara}
#' @examples
#' mr <- iNZightMR(online ~ onlinegame + onlinevideo + onlinemusic,
#'     data = census.at.school.5000)
#' @importFrom grDevices dev.flush dev.hold
#' @import graphics stats utils
#' @export
iNZightMR <- function(frm, data, Labels = NULL, inverse = FALSE, ...) {
    if (length(frm[[2]]))
        classnames <- as.character(frm[[2]])

    display <- with(data, {
        # grab variable name from the formual (frm) in the data file (data))
        mro.mat <- model.frame(frm[-2], data, na.action = na.pass, ...)
        details <- attributes(mro.mat)
        variables <- attr(details$terms, "variables")

        # 23/11/2014 naive test...
        colId = names(mro.mat)
        mro.mat = as.data.frame(lapply(mro.mat, as.factor))[, colId]

        # test binary level
        if (all(unique(sapply(mro.mat, nlevels)) == 2)) {
            mro.mat <- sapply(mro.mat, r01, inverse)
            ### mro function treat NA response as absent response in the original data set
        } else if (sum(which(sapply(mro.mat, nlevels) == 2)) == 0) {
            stop("Hard to detect binary pattern")
        } else {
            # use the levels of the first variables that have 2 levels
            index <- which(sapply(mro.mat, nlevels) == 2)[[1]]
            Commonlevels <- levels(mro.mat[, index])
            mro.mat <- sapply(mro.mat, r01, opts = Commonlevels)
        }

        labelname <-
            if (is.null(Labels)) {
                attr(details$terms, "term.labels")
            } else if (mode(Labels) == "function") {
                Labels(attr(details$terms, "term.labels"))
            } else {
                Labels
            }

        colnames(mro.mat) <-
            if (is.list(labelname))
                labelname$Varname
            else
                labelname

        Ix <- order(colSums(mro.mat), decreasing = TRUE)
        mro.mat <- mro.mat[, Ix]
        labelname <- labelname[Ix]

        out <- list(mro.mat = mro.mat, Labels = labelname, df = data)
        if (!is.null(classnames))
        names(out)[1] <- classnames

        class(out) <- "mro"
        out
    })
    display
}
