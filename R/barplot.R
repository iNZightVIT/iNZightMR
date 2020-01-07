
### notice, because the iNZightPlot function considering svydesign.
### a safe way to pass a pseudo x variable inside I tried is repeat one single
### levels with 10x times, depending on the y/g1/g2 variables levels...
### one of the benefit of doing this is this will generate one factor level with
### 100% percentage thus we avoid to configure the ylim and ymax scale inside.

#' Multiple response barplot
#'
#' Draws a barplot of a multiple response object (MRO), showing response rates for
#' each option along with confidence intervals and comparison intervals.
#'
#' @param obj an \code{mrocalc} object (from \code{mroPara()})
#' @param g1.level vector of subset variable 1 levels to show
#' @param g2.level vector of subset variable 2 levels to show
#' @param ... additional parameters, currently not used
#' @return NULL
#' @author Junjie Zheng
#' @import grid
#' @export
barplotMR <- function(obj, ...)
    UseMethod("barplotMR", obj)


#' @describeIn barplotMR method for class \code{mrocalc}
#' @export
barplotMR.mrocalc <- function(obj, ...) {
    if (!requireNamespace(iNZightPlots))
        stop("Please install the iNZightPlots package to use this function.")

    s1 <- switcher(obj)
    s1$ErrBars <- validateRange2(s1$ErrBar)
    s1$confL <- validateRange2(s1$confL)
    s1$confU <- validateRange2(s1$confU)
    s1$compL <- validateRange2(s1$compL)
    s1$compU <- validateRange2(s1$compU)
    s1 <- validateRange1(s1)
    x <- factor(rep(levels(s1$var)[1], 10), levels = names(obj$Variance))

    pl <- iNZightPlots::iNZightPlot(x, layout.only = TRUE, varnames = list(x = obj$Topic))

    gen <- pl$gen
    opts <- gen$opts
    seekViewport("VP:locate.these.points")

    p <- matrix(s1$est, nrow=1)
    nx <- ncol(p)

    widths <- 1
    edges <- c(0,1)

    edges <- rep(edges * 0.9 + 0.05, each = 4)
    edges <- edges[3:(length(edges) - 2)]
    xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))

    tops <- apply(
        matrix(rep(1, nlevels(s1$var)), nrow = 1),
        2,
        function(x) rbind(0, x, x, 0)
    )
    ymax <- c(tops)
    tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
    yy <- c(tops)


    id <- rep(1:prod(dim(p)), each = 4)
    colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill else rep(gen$col.args$b.cols, nx)


    grid.polygon(unit(xx, "native"), unit(ymax, "native"),
        id = id,
        gp = gpar(
            fill = "grey",
            col = "#000000",  # fill = grey first time, red or whatever the second ...
            lwd = opts$bar.lwd
        )
    )

    grid.polygon(unit(xx, "native"), unit(yy, "native"),
        id = id,
        gp = gpar(
            fill = "red",
            col = "#000000",  # fill = grey first time, red or whatever the second ...
            lwd = opts$bar.lwd
        )
    )


    medpoint.mat <- matrix(xx, ncol = 4, byrow = T)


    lapply(1:length(s1$compL),
        function(i) {
          grid.abline(
              intercept = unit(s1$compL[i], "native"),
              slope = unit(0,"native"),
              gp = gpar(col = "gold", lty = 2)
          )
          grid.abline(
              intercept = unit(s1$compU[i], "native"),
              slope = unit(0,"native"),
              gp = gpar(col = "gold", lty = 2)
          )
        }
    )

    grid.segments(
        x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y0 = unit(s1$compL, "native"),
        x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y1 = unit(s1$compU, "native"),
        gp = gpar(col = "green", lwd = opts$inf.lwd.comp)
    )

    grid.segments(
        x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y0 = unit(s1$confL, "native"),
        x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y1 = unit(s1$confU, "native"),
        gp = gpar(col = "black", lwd = opts$inf.lwd.conf)
    )
}


#' @describeIn barplotMR method for class \code{bymrocalc}
#' @export
barplotMR.bymrocalc <- function(obj, g1.level = NULL, g2.level = "_MULTI",...) {
    if (!requireNamespace(iNZightPlots))
        stop("Please install the iNZightPlots package to use this function.")

    s1 <- switcher(obj)

    s1$ErrBars <- validateRange2(s1$ErrBar)
    s1$confL <- validateRange2(s1$confL)
    s1$confU <- validateRange2(s1$confU)
    s1$compL <- validateRange2(s1$compL)
    s1$compU <- validateRange2(s1$compU)
    s1 <- validateRange1(s1)
    if (which((names(s1) == "var")) < 3) {
        TYPE <- names(s1)[1]
        names(s1)[1] <- "type"

        if (!is.null(g1.level)) {
            if (!g1.level %in% levels(s1$type))
            return("Can't find correct level")

            s1 <- subset(s1, s1$type %in% g1.level)
            s1$type <- droplevels(s1$type)
        }

        x <- factor(rep(levels(s1$var)[1], nlevels(s1$type) * 10),
            levels = names(obj[[1]]$Variance)
        )
        g1 <- factor(unique(as.character(s1$type)),
            levels = unique(as.character(s1$type))
        )
        pl <- iNZightPlots::iNZightPlot(x, g1 = g1, g1.level = g1.level,
            layout.only = TRUE,
            varnames = list(x = obj[[1]]$Topic, g1 = TYPE)
        )

        gen <- pl$gen
        opts <- gen$opts

        ### You need to do the following TWICE - once for all p = 1, then for the actual p's

        praw <- matrix(s1$Est, nrow = nlevels(s1$type), byrow = TRUE)

        for (i in 1:nrow(praw)) {
            nameVP <- paste0("VP:locate.these.points1", i)
            seekViewport(nameVP)
            p <- matrix(praw[i, ], nrow = 1)
            nx <- ncol(p)

            #widths = { 1 OR 1 / length(levels(y)) , whichever is required }
            widths <- 1
            #edges <- { c(0, 1) - for one variable (no y),
            #           OR c(0, cumsum(widths)) - for x and y ... }
            edges <- c(0, 1)

            edges <- rep(edges * 0.9 + 0.05, each = 4)
            edges <- edges[3:(length(edges) - 2)]
            xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))

            tops <- apply(
                matrix(rep(1, nlevels(s1$var)), nrow = 1),
                2,
                function(x) rbind(0, x, x, 0)
            )
            ymax <- c(tops)
            tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
            yy <- c(tops)


            id <- rep(1:prod(dim(p)), each = 4)
            colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill
                else rep(gen$col.args$b.cols, nx)


            grid.polygon(unit(xx, "native"), unit(ymax, "native"),
                id = id,
                gp = gpar(
                    fill = "grey",
                    col = "#000000",  # fill = grey first time, red or whatever the second ...
                    lwd = opts$bar.lwd
                )
            )

            grid.polygon(unit(xx, "native"), unit(yy, "native"),
                id = id,
                gp = gpar(
                    fill = "red",
                    col = "#000000",  # fill = grey first time, red or whatever the second ...
                    lwd = opts$bar.lwd
                )
            )

            medpoint.mat <- matrix(xx, ncol = 4,byrow = T)

            compL <- matrix(s1$compL, nrow = nlevels(s1[, 1]), byrow = TRUE)[i, ]
            compU <- matrix(s1$compU, nrow = nlevels(s1[, 1]), byrow = TRUE)[i, ]
            confL <- matrix(s1$confL, nrow = nlevels(s1[, 1]), byrow = TRUE)[i, ]
            confU <- matrix(s1$confU, nrow = nlevels(s1[, 1]), byrow = TRUE)[i, ]


            lapply(1:length(compL),
                function(i) {
                    grid.abline(
                        intercept = unit(compL[i], "native"),
                        slope = unit(0,"native"),
                        gp = gpar(col = "gold", lty = 2)
                    )
                    grid.abline(
                        intercept = unit(compU[i], "native"),
                        slope = unit(0, "native"),
                        gp = gpar(col = "gold", lty = 2)
                    )
                }
            )


            grid.segments(
                x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                y0 = unit(compL, "native"),
                x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                y1 = unit(compU, "native"),
                gp = gpar(
                    col = "green",  # fill = grey first time, red or whatever the second ...
                    lwd = opts$inf.lwd.comp
                )
            )

            grid.segments(
                x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                y0 = unit(confL, "native"),
                x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                y1 = unit(confU, "native"),
                gp = gpar(
                    col = "black",  # fill = grey first time, red or whatever the second ...
                    lwd = opts$inf.lwd.conf
                )
            )

        }
    } else {
        TYPE1 <- names(s1)[2]
        names(s1)[2] <- "type1"
        TYPE2 <- names(s1)[1]
        names(s1)[1] <- "type2"

        if (!is.null(g1.level)) {
            if (!g1.level %in% levels(s1$type1))
            return("Can't find correct level")

            s1 <- subset(s1, s1$type1 %in% g1.level)
            s1$type1 <- droplevels(s1$type1)
        }

        if (g2.level != "_MULTI") {
            if (!g2.level %in% levels(s1$type2))
            return("Can't find correct level")

            s1 <- subset(s1, s1$type2 %in% g2.level)
            s1$type2 <- droplevels(s1$type2)
        }

        x <- factor(
            rep(levels(s1$var)[1], nlevels(s1$type1) * nlevels(s1$type2) * 10),
            levels = names(obj[[1]]$Variance)
        )
        g1 <- factor(
            rep(unique(as.character(s1$type1)), each = nlevels(s1$type2)),
            levels = unique(as.character(s1$type1))
        )
        g2 <- factor(
            unique(as.character(s1$type2)),
            levels = unique(as.character(s1$type2))
        )
        pl <- iNZightPlots::iNZightPlot(x, g1 = g1, g2 = g2, g1.level = g1.level,
            layout.only=TRUE, g2.level = g2.level,
            varnames = list(x = obj[[1]]$Topic, g1 = TYPE1, g2 = TYPE2)
        )

        gen <- pl$gen
        opts <- gen$opts

        ### You need to do the following TWICE - once for all p = 1, then for the actual p's

        praw <- matrix(s1$Est, nrow = nlevels(s1$type1) * nlevels(s1$type2), byrow = TRUE)

        for (j in 1:nlevels(s1$type2)) {
            for (i in 1:nlevels(s1$type1)) {
                k <- (j-1) * nlevels(s1$type1) + i
                nameVP <- paste0("VP:locate.these.points", j, i)
                seekViewport(nameVP)
                p <- matrix(praw[k, ], nrow = 1)
                nx <- ncol(p)

                #widths = { 1 OR 1 / length(levels(y)) , whichever is required }
                widths <- 1
                #edges <- { c(0, 1) - for one variable (no y),
                #           OR c(0, cumsum(widths)) - for x and y ... }
                edges <- c(0, 1)

                edges <- rep(edges * 0.9 + 0.05, each = 4)
                edges <- edges[3:(length(edges) - 2)]
                xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))

                tops <- apply(
                    matrix(rep(1, nlevels(s1$var)), nrow = 1),
                    2,
                    function(x) rbind(0, x, x, 0)
                )
                ymax <- c(tops)
                tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
                yy <- c(tops)


                id <- rep(1:prod(dim(p)), each = 4)
                colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill
                    else rep(gen$col.args$b.cols, nx)


                grid.polygon(unit(xx, "native"), unit(ymax, "native"),
                    id = id,
                    gp = gpar(
                        fill = "grey",
                        col = "#000000",  # fill = grey first time, red or whatever the second ...
                        lwd = opts$bar.lwd
                    )
                )

                grid.polygon(unit(xx, "native"), unit(yy, "native"),
                    id = id,
                    gp = gpar(
                        fill = "red",
                        col = "#000000",  # fill = grey first time, red or whatever the second ...
                        lwd = opts$bar.lwd
                    )
                )

                medpoint.mat <- matrix(xx, ncol = 4, byrow = T)

                compL <- matrix(s1$compL,
                    nrow = nlevels(s1$type1) * nlevels(s1$type2),
                    byrow = TRUE
                )[k, ]
                compU <- matrix(s1$compU,
                    nrow = nlevels(s1$type1) * nlevels(s1$type2),
                    byrow = TRUE
                )[k, ]
                confL <- matrix(s1$confL,
                    nrow = nlevels(s1$type1) * nlevels(s1$type2),
                    byrow = TRUE
                )[k, ]
                confU <- matrix(s1$confU,
                    nrow = nlevels(s1$type1) * nlevels(s1$type2),
                    byrow = TRUE
                )[k, ]

                lapply(1:length(compL),
                    function(i){
                        grid.abline(
                            intercept = unit(compL[i], "native"),
                            slope = unit(0, "native"),
                            gp = gpar(col = "gold", lty = 2)
                        )
                        grid.abline(
                            intercept = unit(compU[i], "native"),
                            slope = unit(0, "native"),
                            gp = gpar(col = "gold", lty = 2)
                        )
                    }
                )

                grid.segments(
                    x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                    y0 = unit(compL, "native"),
                    x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                    y1 = unit(compU, "native"),
                    gp = gpar(col = "green", lwd = opts$inf.lwd.comp)
                )

                grid.segments(
                    x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                    y0 = unit(confL, "native"),
                    x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
                    y1 = unit(confU, "native"),
                    gp = gpar(col = "black", lwd = opts$inf.lwd.conf)
                )
            } # for (i)
        } # for (j)
    }
}


#' @describeIn barplotMR method for class \code{between}
#' @export
barplotMR.between <- function(obj, ...) {
    if (!requireNamespace(iNZightPlots))
        stop("Please install the iNZightPlots package to use this function.")

    s2 <- switcher(obj)

    s2$ErrBars <- validateRange2(s2$ErrBars)
    s2$confL <- validateRange2(s2$confL)
    s2$confU <- validateRange2(s2$confU)
    s2$compL <- validateRange2(s2$compL)
    s2$compU <- validateRange2(s2$compU)
    s2 <- validateRange1(s2)
    TYPE <- names(s2)[1]
    names(s2)[1] <- "type"

    x <- factor(rep(levels(s2$var)[1], nlevels(s2$type) * 10),
        levels = levels(s2$var)
    )
    y <- factor(unique(as.character(s2$type)),
        levels = unique(as.character(s2$type))
    )

    pl <- iNZightPlots::iNZightPlot(x, y,
        layout.only = TRUE,
        varnames = list(x = attr(obj, "Topic"), y = TYPE)
    )

    gen <- pl$gen
    opts <- gen$opts
    seekViewport("VP:locate.these.points")

    p <- matrix(s2$est, nrow = nlevels(s2$type))
    nx <- ncol(p)

    #widths = { 1 OR 1 / length(levels(y)) , whichever is required }
    widths <- rep(1 / nlevels(y), nlevels(y))
    #edges <- { c(0, 1) - for one variable (no y),
    #           OR c(0, cumsum(widths)) - for x and y ... }
    edges <- c(0, cumsum(widths))

    edges <- rep(edges * 0.9 + 0.05, each = 4)
    edges <- edges[3:(length(edges) - 2)]
    xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))


    tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
    yy <- c(tops)

    id <- rep(1:prod(dim(p)), each = 4)
    colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill else rep(gen$col.args$b.cols, nx)


    grid.polygon(unit(xx, "native"), unit(yy, "native"),
        id = id,
        gp = gpar(
            fill = colz,
            col = "#000000",  # fill = grey first time, red or whatever the second ...
            lwd = opts$bar.lwd
        )
    )

    group.range <- apply(matrix(xx, ncol = 4 * nlevels(y), byrow = TRUE), 1, range)
    compL <- matrix(s2$compL, nrow = nx, byrow = TRUE)
    compU <- matrix(s2$compU, nrow = nx, byrow = TRUE)

    lapply(1:ncol(group.range),
        function(j) {
            left <- group.range[1, j]
            right <- group.range[2, j]
            grid.segments(
                x0 = unit(left, "native"),
                y0 = unit(compL[j, ], "native"),
                x1 = unit(right, "native"),
                y1 = unit(compL[j, ], "native"),
                gp = gpar(
                    col = "gold",  # fill = grey first time, red or whatever the second ...
                    lty = 2
                )
            )
            grid.segments(
                x0 = unit(left, "native"),
                y0 = unit(compU[j,], "native"),
                x1 = unit(right, "native"),
                y1 = unit(compU[j,], "native"),
                gp = gpar(
                    col = "gold",  # fill = grey first time, red or whatever the second ...
                    lty = 2
                )
            )
        }
    )

    medpoint.mat <- matrix(xx, ncol = 4, byrow = TRUE)

    grid.segments(
        x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y0 = unit(s2$compL, "native"),
        x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y1 = unit(s2$compU, "native"),
        gp = gpar(
            col = "green",  # fill = grey first time, red or whatever the second ...
            lwd = opts$inf.lwd.comp
        )
    )

    grid.segments(
        x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y0 = unit(s2$confL, "native"),
        x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
        y1 = unit(s2$confU, "native"),
        gp = gpar(
            col = "black",  # fill = grey first time, red or whatever the second ...
            lwd = opts$inf.lwd.conf
        )
    )
}


#' @describeIn barplotMR method for class \code{b2}
#' @export
barplotMR.b2 <- function(obj, g1.level = NULL, ...) {
    if (!requireNamespace(iNZightPlots))
        stop("Please install the iNZightPlots package to use this function.")

    s2 <- switcher(obj)

    s2$ErrBars <- validateRange2(s2$ErrBars)
    s2$confL <- validateRange2(s2$confL)
    s2$confU <- validateRange2(s2$confU)
    s2$compL <- validateRange2(s2$compL)
    s2$compU <- validateRange2(s2$compU)
    s2 <- validateRange1(s2)
    TYPE1 <- names(s2)[2]
    names(s2)[2] <- "type1"
    TYPE2 <- names(s2)[1]
    names(s2)[1] <- "type2"

    if (!is.null(g1.level)) {
        if (!g1.level %in% levels(s2$type2))
        return("Can't find correct level")

        s2 <- subset(s2, s2$type2 %in% g1.level)
        s2$type2 <- droplevels(s2$type2)
    }

    x <- factor(
        rep(levels(s2$var)[1], nlevels(s2$type1) * nlevels(s2$type2)),
        levels = levels(s2$var)
    )
    y <- factor(
        rep(unique(as.character(s2$type1)), each = nlevels(s2$type2)),
        levels = unique(as.character(s2$type1))
    )
    g1 <- factor(unique(as.character(s2$type2)))

    pl <- iNZightPlots::iNZightPlot(x, y = y, g1 = g1,
        layout.only = TRUE,
        g1.level = g1.level,
        varnames = list(x = attr(obj, "Topic"), y = TYPE1, g1 = TYPE2)
    )
    gen <- pl$gen
    opts <- gen$opts

    #   partial.key <- NULL
    #   if (!is.null(g1.level))
    #     partial.key <- which(g1 %in% g1.level)

    praw <- matrix(s2$est, nrow = nlevels(s2$type2), byrow = TRUE)
    for (i in 1:nrow(praw)) {
        p <- matrix(praw[i, ], nrow = nlevels(s2$type1))
        nx <- ncol(p)
        nameVP <- paste0("VP:locate.these.points1", i)
        seekViewport(nameVP)
        widths <- rep(1 / nlevels(y), nlevels(y))
        edges <- c(0, cumsum(widths))

        edges <- rep(edges * 0.9 + 0.05, each = 4)
        edges <- edges[3:(length(edges) - 2)]
        xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))


        tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
        yy <- c(tops)

        id <- rep(1:prod(dim(p)), each = 4)
        colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill
            else rep(gen$col.args$b.cols, nx)

        compL <- matrix(
            matrix(s2$compL, nrow = nlevels(s2$type2), byrow = TRUE)[i, ],
            nrow = 1
        )
        compU <- matrix(
            matrix(s2$compU, nrow = nlevels(s2$type2), byrow = TRUE)[i, ],
            nrow = 1
        )
        confL <- matrix(
            matrix(s2$confL, nrow = nlevels(s2$type2), byrow = TRUE)[i, ],
            nrow = 1
        )
        confU <- matrix(
            matrix(s2$confU, nrow = nlevels(s2$type2), byrow = TRUE)[i, ],
            nrow = 1
        )

        grid.polygon(unit(xx, "native"), unit(yy, "native"),
            id = id,
            gp = gpar(
                fill = colz,
                col = "#000000",  # fill = grey first time, red or whatever the second ...
                lwd = opts$bar.lwd
            )
        )


        group.range <- apply(matrix(xx, ncol = 4 * nlevels(y), byrow = TRUE), 1, range)
        lower.range <- matrix(compL, nrow = nx, byrow = TRUE)
        upper.range <- matrix(compU, nrow = nx, byrow = TRUE)


        lapply(1:ncol(group.range),
            function(k) {
                left <- group.range[1, k]
                right <- group.range[2, k]
                grid.segments(
                    x0 = unit(left, "native"),
                    y0 = unit(lower.range[k, ], "native"),
                    x1 = unit(right, "native"),
                    y1 = unit(lower.range[k, ], "native"),
                    gp = gpar(col = "gold", lty = 2)
                )
                grid.segments(
                    x0 = unit(left, "native"),
                    y0 = unit(upper.range[k, ], "native"),
                    x1 = unit(right, "native"),
                    y1 = unit(upper.range[k, ], "native"),
                    gp = gpar(col = "gold", lty = 2)
                )
            }
        )


        medpoint.mat <- matrix(xx, ncol = 4, byrow = T)

        grid.segments(
            x0 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
            y0 = unit(compL, "native"),
            x1 = unit(rowMeans(medpoint.mat[, 2:3]), "native"),
            y1 = unit(compU, "native"),
            gp = gpar(col = "green", lwd = opts$inf.lwd.comp)
        )

        grid.segments(
            x0 = unit(rowMeans(medpoint.mat[,2:3]), "native"),
            y0 = unit(confL, "native"),
            x1 = unit(rowMeans(medpoint.mat[,2:3]), "native"),
            y1 = unit(confU, "native"),
            gp = gpar(col = "black", lwd = opts$inf.lwd.conf)
        )

    }

}
