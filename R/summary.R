
#' @export
summary.mrocalc <- function(object, ...) {
    list(
        df = object$Mromoecalc$fit$df,
        Mromoecalc = round(summary(object$Mromoecalc)$coef, 3),
        Multicom = round(object$Multicom, 3)
    )
}

#' @export
summary.within <- function(object, ...) {
    Title <-
        if (length(dim(object)) < 2) {
            paste0(names(dimnames(object)), ":", dimnames(object)[[1]])
        } else {
            commonTitle <-
                paste0(names(dimnames(object)[1]), ":", names(dimnames(object)[2]))
            groupTitle <- outer(
                dimnames(object)[[1]],
                dimnames(object)[[2]],
                paste,
                sep = " & "
            )
            paste(commonTitle, groupTitle, sep = " = ")
        }
    Multicom.Group <- vector("list", length(object))
    for (i in seq_along(object)) {
        cat(Title[i] , "\n")
        if (is.null(object[[i]])) {
            cat("Missing Observations","\n")
            cat("\n")
            cat("----------------------------------------------------------------------")
            cat("\n")
            next
        }
        # TODO: Magic numbers, use names instead
        WithinVars <- as.data.frame(
            object[[i]]$Mromoecalc[c(2, 12, 14, 15)],
            stringsAsFactors = TRUE
        )
        print(round(WithinVars, 3))
        cat("\n")
        cat("Chisq test for uniformity: chisq =",
            chisq.mro(object[[i]]),
            " , df =",
            length(object[[i]]$Mromoecalc$est),
            " p-value =",
            pchisq(q = chisq.mro(object[[i]]),
                df = length(object[[i]]$Mromoecalc$est),
                lower.tail = FALSE
            ),
            "\n"
        )
        cat("\n")
        cat(
            paste0("95% CIs for diffs in propns within ",
                Title[i],
                " distribution"
            ),
            "\n"
        )
        cat("(rowname - colname)", "\n")
        cat("\n")
        print(round(object[[i]]$Multicom, 3))
        Multicom.Group[[i]] <- object[[i]]$Multicom
        cat("\n")
        cat("----------------------------------------------------------------------")
        cat("\n")
    }
    invisible(
        list(
            Title = Title,
            WithinVars = WithinVars,
            Multicom.Group = Multicom.Group
        )
    )
}

#' @export
summary.between <- function(object, bymro,...) {
    stop("This function is on hiatus.")
}


#' @export
summary.bymrocalc <- function(object, comp = "basic", ...) {
    cat("Proportions:\n")
    tabprops <- crossTab(object)
    cat("\n")
    print(tabprops, digits = 3)

    if (comp == "within") {
        cat("--------------------------------------------------------\n")
        class(object) <- "within"
        summary(object)
    }
    if (comp == "between"){
        cat("--------------------------------------------------------\n")
        src <- between(object)
        summary(src, object)
    }
}
