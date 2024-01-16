rm.na <- function(variable) {
    sum(is.na(variable)) > 0
}


#' Calculates the summary of missingness in a data set.
#' @title Calculate missing observation summary
#' @param obj An object
#' @param MRO.case does something with rownames
# #' @param print logical, should we print the thing?
# #' @param final logical, if `TRUE` only the result table is returned
#' @param ... additional arguments
#' @return Missing value object
#' @author Junjie Zeng
#' @export
#' @examples
#' calcmissing(census.at.school.5000[, 1:20])
#' @seealso plotcombn
calcmissing <- function(obj, ...) {
    UseMethod("calcmissing")
}


#' @describeIn calcmissing Method for a dataframe
#' @export
calcmissing.data.frame <- function(obj, MRO.case = FALSE,
                                   print = TRUE, final = TRUE, ...) {
    data <- obj
    sortby <- "row"

    index.column <- sapply(data, rm.na)

    if (!any(index.column)) {
        out <- "Data Clean"
        class(out) <- "non-missing" # build class here for future use.
        return(out)
    }

    x <- data[, index.column]
    x1 <- as.numeric(apply(x, 2, function(x) length(which(is.na(x)))))
    row4col.order <- order(x1)
    x1 <- c(x1, nrow(x))
    z1 <- ifelse(is.na(x), 0, 1)
    tab <- table(apply(z1, 1, paste, collapse = ","))
    tab <- tab[order(names(tab), decreasing = TRUE)]
    tab <- data.frame(
        combination = names(tab),
        count = as.numeric(tab),
        stringsAsFactors = TRUE
    )
    tabp <- t(apply(tab, 1, function(x) {
        as.numeric(unlist(strsplit(x, ",", fixed = TRUE)))
    }))
    tabp <- as.data.frame(tabp, stringsAsFactors = TRUE)
    tabp <- tabp[, c(row4col.order, max(row4col.order) + 1)]
    tabp <- rbind(tabp, x1[c(row4col.order, max(row4col.order) + 1)])
    names(tabp) <- c(names(x)[row4col.order], "Total")
    row.names(tabp) <- c(seq_len(nrow(tab)), "Total")


    if (sortby == "variable") {
        tabfinal <- tabp
    }
    if (sortby == "row") {
        tabfinal <- tabp[-nrow(tabp), ]
        tabfinal <- tabfinal[order(tabfinal$Total, decreasing = TRUE), ]
        tabfinal <- rbind(tabfinal, tabp[nrow(tabp), ])
    }
    if (sortby == "column") {
        tabfinal <- tabp[, -ncol(tabp)]
        vals <- unlist(tabfinal[nrow(tabfinal), ])
        tabfinal <- tabfinal[order(vals, decreasing = TRUE)]
        tabfinal <- cbind(tabfinal, Total = tabp$Total)
    }
    if (sortby == "both") {
        tabf <- tabp[-nrow(tabp), ]
        tabf <- tabf[order(tabf$Total, decreasing = TRUE), ]
        tabf <- rbind(tabf, tabp[nrow(tabp), ])
        tabfinal <- tabf[, -ncol(tabf)]
        vals <- unlist(tabfinal[nrow(tabfinal), ])
        tabfinal <- tabfinal[order(vals, decreasing = TRUE)]
        tabfinal <- cbind(tabfinal, Total = tabf$Total)
    }

    finaltable <- tabfinal

    Name <- names(finaltable)
    i <- nrow(finaltable)
    j <- ncol(finaltable)
    index <- order(x1[-j], decreasing = FALSE)
    numMiss <- x1[c(index, j)]
    percMiss <- round(numMiss / numMiss[j], 3)

    TolTab <- rbind(count = numMiss, percentage = percMiss)
    if (MRO.case) {
        rownames(TolTab) <- c("nSelect", "%Select")
    }
    colnames(TolTab) <- Name
    TolTab <- as.data.frame(TolTab, stringsAsFactors = TRUE)
    TolTab[2, ] <- paste0(round(TolTab[2, ] * 100, 2), "%")
    TolTab[1, ] <- as.character(TolTab[1, ])

    colnames(finaltable)[j] <- "Freq"
    class(finaltable) <- c("missing_combn", class(finaltable))

    class(TolTab) <- c("missing_summary", class(TolTab))

    structure(
        list(total = TolTab, combinations = finaltable),
        class = "calcmissing"
    )
}

#' @export
print.calcmissing <- function(x, what = c("both", "total", "combinations"), ...) {
    what <- match.arg(what)

    if (what == "both") {
        print(x$total)
        cat("\n\n")
        print(x$combinations)
    }
    if (what == "total") {
        print(x$total)
    }
    if (what == "combinations") {
        print(x$combinations)
    }

    invisible(x)
}

#' @export
print.missing_summary <- function(x, ...) {
    cat("# Number and percent of missing variables in each variable\n\n")
    print.data.frame(x)
    invisible(x)
}

#' @export
print.missing_combn <- function(x, ...) {
    tbl <- capture.output(
        print.data.frame(
            data.frame(
                x,
                Percentage = 100 *
                    round(x[, "Freq"] / max(x[, "Freq"]), 3),
                stringsAsFactors = TRUE
            )
        )
    )

    mnum <- gregexpr("^[0-9]+", tbl)
    sp <- sapply(mnum, function(m) {
        if (m == 1) {
            paste(rep(" ", attr(m, "match.length")), collapse = "")
        } else {
            NA
        }
    })
    for (i in seq_along(mnum)) {
        if (mnum[[i]] == -1) next
        tbl[i] <- gsub("^[0-9]+", sp[i], tbl[i])
    }
    cat("# Combinations of missing values:\n\n")
    cat(tbl, sep = "\n")

    invisible(x)
}

#' @describeIn calcmissing accepts a whole mr.object , which is first mro.mat, second element labels,
#'   third element the input data frame.
#' @export
calcmissing.mro <- function(obj, ...) {
    mro <- obj
    mat <- mro[[1]]
    mat[mat == 0] <- NA
    calcmissing(as.data.frame(mat, stringsAsFactors = TRUE), MRO.case = TRUE, ...)
}

#' Plot of Missing Value combinations
#'
#' @title Missing Value plot
#' @param obj a calcmissing object
#' @return summarised info for plot
#' @author Junjie Zeng
#' @export
#' @examples
#' plotcombn(census.at.school.5000[, 10:25])
plotcombn <- function(obj) {
    Subtitle <- NULL
    if (inherits(obj, "Sub")) {
        Subtitle <- class(obj)[3]
        class(obj) <- "mro"
    }
    if (!inherits(obj, "data.frame") && !inherits(obj, "mro")) {
        stop("Invalid input. Must be 'data.frame' or 'mro'")
    }

    finaltable <- calcmissing(obj)$combinations
    if (inherits(finaltable, "non-missing")) {
        plot.new()
        plot.window(0:1, 0:1)
        text(0.5, 0.5, "Dataset has no missing values!")
        return(finaltable)
    }


    dev.hold()
    on.exit(dev.flush())

    layout(
        rbind(
            c(0, 0, 0, 0, 0),
            c(0, 4, 4, 4, 0),
            c(0, 0, 0, 0, 0),
            c(0, 2, 0, 5, 0),
            c(0, 0, 0, 0, 0),
            c(0, 1, 0, 3, 0),
            c(0, 0, 0, 0, 0)
        ),
        heights = c(lcm(1), lcm(1), lcm(1), lcm(5), lcm(1), 1, lcm(1)),
        widths = c(lcm(0.5), 1, lcm(0.1), lcm(3), lcm(0.5))
    )

    x <- finaltable
    row.x <- nrow(x)
    col.x <- ncol(x)
    row.num <- row.x - 1
    col.num <- col.x - 1
    total <- x[row.x, col.x]
    p2 <- 1 - x[row.x, 1:col.num] / total
    p3 <- x[1:row.num, col.x] / total

    x.fit <- finaltable[seq_len(row.num), seq_len(col.num)]

    opa <- par(mar = rep(0, 4))
    on.exit(par(opa), add = TRUE)

    plot(2 * col.num, min(row.num, 30),
        type = "n",
        xlim = c(0, 2 * col.num),
        ylim = c(0, min(row.num, 30)),
        axes = FALSE,
        xlab = "",
        ylab = ""
    )
    x1 <- seq(0, 2 * col.num - 2, by = 2)
    x2 <- seq(2, 2 * col.num, by = 2)
    y1 <- min(row.num, 30) - 1
    y2 <- min(row.num, 30)
    cols <- c("gray", "red")
    # showing maximum first 30 combinations
    for (i in 0:(min(row.num, 30) - 1)) {
        index <- as.numeric(x.fit[i + 1, ]) + 1
        rect(x1, y1 - i, x2, y2 - i, col = cols[index], border = "Alice Blue")
    }

    plot(2 * col.num, 1,
        type = "n",
        xlim = c(0, 2 * col.num),
        ylim = c(0, 1.3),
        axes = FALSE,
        xlab = "",
        ylab = ""
    )
    xmed <- seq(1, 2 * col.num, by = 2)
    ### x value is not fixed.... the "% of present" is not fixed.....
    text(col.num^(1 / 10), 1.2, "% Present", cex = 2)
    rect(xmed - 0.3, 0, xmed + 0.3, 1, col = "gray")
    rect(xmed - 0.3, 0, xmed + 0.3, p2, col = "red")
    if (col.num <= 10) {
        axis(1, xmed, colnames(x.fit), tick = FALSE)
    } else {
        text(xmed, -0.025,
            srt = 65,
            adj = 1,
            labels = colnames(x.fit),
            xpd = NA
        )
    }

    ## 3rd blue bars, the relative length
    cons <- min(row.num, 30)
    p3 <- p3[1:cons]
    bb <- p3[length(p3):1]
    bb <- log1p(bb) ## for any size data, the more the size the larger chance the first row leads a lot than others
    ## we use log1p() to avoid the exponential increasing for the proportion
    ## in the same time give more weight to those proportion with too little value.
    bb <- seq(0.1, 0.2, len = length(bb)) + bb / max(bb)
    plot(max(bb), cons,
        type = "n",
        xlim = c(0, max(bb)),
        ylim = c(0, cons),
        xlab = "",
        ylab = "",
        axes = FALSE
    )
    ymed <- seq(0.5, cons - 0.5, by = 1)

    border <- rep(NA, length(bb))
    border[length(bb)] <- "red"
    rect(0, ymed - 0.2, bb, ymed + 0.2, col = "blue", border = border)
    mtext("Pattern", line = 1)
    mtext("Frequency", line = -0.5)

    # 4th plot--title control
    plot.new()
    if (inherits(obj, "data.frame")) {
        text(.5, .5, "Missing Value Plot", cex = 1.5, font = 2)
    }
    if (is.null(Subtitle) && inherits(obj, "mro")) {
        text(.5, .5, "Combination Plot", cex = 1.5, font = 2)
    }
    if (!is.null(Subtitle) && inherits(obj, "mro")) {
        text(.5, .3, paste("Combination Plot", Subtitle, sep = "---"),
            cex = 1.5,
            font = 1
        )
    }

    # 5th plot -- legend control
    plot.new()
    legend.name <- c("Missing", "Present", "NotSelected", "Selected")
    legend.index <- ifelse(class(obj)[1] == "mro", 3, 1)
    legend(0, 0.5, legend.name[c(legend.index, legend.index + 1)],
        fill = c("gray", "red"),
        col = c("red", "gray"),
        xpd = NA
    )


    x
}
