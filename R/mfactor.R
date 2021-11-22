#' Create a multi-response factor variable
#'
#' @param x a dataframe of columns for each response option
#' @param levels vector of level names (defaults to names of `x`)
#' @param indicator if values in `x` are not coercible to logical,
#'        then indicator can either be the value used to indicate
#'        a selection (e.g., 'yes'), or a function if a more complex
#'        criteria is required (e.g., `function(x) x > 0`).
#' @param na.value usually, missing values in some columns will be treated
#'        as `FALSE`/zero, but this can be overridden by specifying
#'        an alternative value (1, or any other value). Note that
#'        currently missing values are considered 'non-response',
#'        and so are treated in exactly the same way as a 'zero' response.
#'        If all values for a row are missing, then the result will always be `NA`.
#'
#' @return A vector of class `mfactor` where each item is a character
#'         string of zeros and ones indicating, in order,
#'         the response value associated with that row.
#'
#' @md
#' @export
#' @author Tom Elliott
#' @examples
#' mat <- data.frame(
#'   A = c(1, 0, 0, 1),
#'   B = c(1, 0, 1, 0),
#'   C = c(0, 1, 1, 1),
#'   D = c(0, 0, 1, 0)
#' )
#' (var <- mfactor(mat))
#'
#' # can plot, either on its own or in a data.frame
#' plot(var)
#'
#' data <- data.frame(var = var)
#' plot(var ~ 1, data = data)
mfactor <- function(x, levels = names(x), indicator = NULL, na.value = 0L) {
    force(levels)
    if (is.null(indicator)) i_fun <- function(v) as.logical(v)
    else if (is.function(indicator)) i_fun <- indicator
    else i_fun <- function(v) v == indicator

    x <- lapply(x, function(v) as.integer(i_fun(v)))
    x <- do.call(cbind, x)

    # handle missing values
    x <- apply(x, 1L, function(v) {
        if (all(is.na(v))) return(NA)
        if (any(is.na(v))) v[is.na(v)] <- na.value
        paste(v, collapse = "")
    })

    attr(x, "levels") <- levels
    class(x) <- "mfactor"
    x
}

level_matrix <- function(x) {
    z <- t(sapply(x, function(v) strsplit(v, "")[[1]]))
    dimnames(z) <- NULL
    mode(z) <- "integer"
    mode(z) <- "logical"
    z
}

#' @export
format.mfactor <- function(x, nmax = 3L, ...) {
    z <- level_matrix(x)
    l <- apply(z, 1, function(v) levels(x)[v])
    l <- sapply(l, function(z) {
        if (length(z) > nmax) {
            sprintf("%s and %d more",
                paste(z[1:nmax], collapse = ", "),
                length(z) - nmax
            )
        } else {
            paste(z, collapse = ", ")
        }
    })
    l <- sapply(l, function(z) sprintf("[%s]", z))
    format(structure(as.character(l), names = names(x), dim = dim(x)),
        ...
    )
}

#' @export
print.mfactor <- function(x, quote = FALSE, nmax = 3L, ...) {
    ## pluck out levels for each entry
    l <- format(x, nmax = nmax, ...)
    print(l, quote = quote)
}

#' @export
`[.mfactor` <- function(x, ...) {
    y <- NextMethod("[")
    attr(y, "levels") <- attr(x, "levels")
    class(y) <- oldClass(x)
    y
}

#' @export
as.data.frame.mfactor <- function(x, row.names = NULL, optional = FALSE, ...,
                                  nm = deparse1(substitute(x))) {
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) ==
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!",
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L)
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names))
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x)))
        names(x) <- NULL
    value <- list(x)
    if (!optional)
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}

#' @export
plot.mfactor <- function(x, y, xlab, ylab, ...) {

    if (missing(ylab)) {
        ylab <- if (missing(y)) deparse(substitute(x)) else deparse(substitute(y))
    }
    xn <- ylab
    if (!missing(y)) {
        oy <- y
        y <- x
        x <- oy
        yn <- xlab
    }

    m <- level_matrix(x)
    d <- as.data.frame(m)
    names(d) <- levels(x)
    if (!missing(y)) d[[yn]] <- y
    fmla <- sprintf("%s ~ %s",
        xn,
        paste(levels(x), collapse = " + ")
    )
    fmla <- eval(parse(text = fmla))
    mr <- iNZightMR(fmla, d)

    if (missing(y)) {
        barplotMR(mroPara(mr))
    } else {
        barplotMR(
            byMRO(mr, eval(parse(text = sprintf("~%s", yn))), mroPara)
        )
    }
}
