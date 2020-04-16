fastNumchange <- function(x) {
    if (class(x) == "factor" | class(x) == "character")
        return(x)

    style <- fivenum(x)
    if (length(unique(x)) < 10) factor(x)
    else cut(x, style + c(-1, 0, 0, 0, 0))
}

#' Calculate MRO inference for subsets
#'
#' Constructs a multiple response object (MRO) subset by another explanatory variable.
#'
#' @param mro.obj an mro object (created by \code{iNZightMR})
#' @param formula variable for subsetting, as a formula (e.g., \code{~x})
#' @param FUN the function to apply to subsets
#' @param ... additional arguments passed to \code{FUN}
#' @return an object with classes of \code{by} and \code{bymrocalc}
#' @export
#' @seealso \link{mroPara}
byMRO <- function(mro.obj, formula, FUN, ...) {
    Data <- mro.obj$df
    Dframe <- mro.obj[[1]]

    if (formula == "~1")
        return(FUN(Dframe, ...))

    Indice <- model.frame(formula, Data, na.action = na.pass)

    if (ncol(Indice) > 1) {
        Indna <- as.logical(rowSums(is.na(Indice)))
        Indice <- Indice[! Indna, ]
        Indice <- lapply(Indice, fastNumchange)
    } else {
        nm <- names(Indice)
        Indna <- is.na(Indice)
        Indice <- Indice[! Indna,]

        # must be return the data frame so that the by can detect name
        Indice <- fastNumchange(Indice)
        Indice <- as.data.frame(Indice, stringsAsFactors = TRUE)
        colnames(Indice) <- nm
    }

    Dframe <- Dframe[!Indna, ]
    Data <- Data[!Indna, ]

    if (substitute(FUN) == "mroPara") {
        out <- by(Dframe, Indice, FUN,
            nonparallel = names(mro.obj)[1]
        )
        #out[which(sapply(out,is.null))] <- "Missing Observations"
        class(out) <- c("by", "bymrocalc")
    } else {
        out <- by(Dframe, Indice, FUN, ...)
    }

    out
}
