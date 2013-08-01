fastBinaryChange <- function(x, inverse = FALSE, opts = NULL) {
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

mro <- function(frm, data, Labels = NULL,
                inverse = FALSE, combi = NULL, ...) {      
    display <- with(data, {
        # grab variable name from the formual (frm) in the data file (data))
        mro.mat <- model.frame(frm, data, na.action = na.pass,...)
        details <- attributes(mro.mat)
        variables <- attr(details$terms, "variables")
        # mro function treat NA response as absent response in
        # the original data set
        mro.mat[is.na(mro.mat)] <- 0
        mro.mat <- sapply(mro.mat, fastBinaryChange, inverse)

        labelname <- 
            if (is.null(Labels)) {
                attr(details$terms, "term.labels")
            } else if (storage.mode(Labels) == "function") {
                Labels(attr(details$terms, "term.labels")) 
            } else {
                Labels
            }
        
        colnames(mro.mat) <-
            if (is.list(labelname))
                labelname$Varname
            else
                labelname

        if (! is.null(combi)) {
            combination.index <- combn(ncol(mro.mat), combi)
            nc <- ncol(combination.index)
            com.mro.mat <- vector("list", nc)
            com.lablename <- character(nc)
            for (j in seq_len(nc)) {
                com.mro.mat[[i]] <- mro.mat[, combination.index[1, j]] * 
                                    mro.mat[, combination.index[2, j]]
                com.lablename[j] <- paste(labelname[combination.index[1, j]],
                                          labelname[combination.index[2, j]],
                                          sep = ":")
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

substrsplit <- function(obj) {
    str <- names(obj) # if obj is not a vector, str will be NULL
    if (is.vector(obj))
        str <- obj

    n <- max(nchar(str))
    i <- 0
    while(length(unique(substr(str, 1, i))) == 1) {
        i <- i + 1
    }
    commonstr <- unique(substr(str, 1, i - 1))
    varname <- substr(str, i, n)
    
    list(Commonstr = commonstr,
         Varname = varname)
}

fastNumchange <- function(x, style = fivenum(x)) {
    cut(x, style)
} 

by.mro <- function(mro.obj, formula, FUN, ...) {
    Data <- mro.obj$df
    Dframe <- mro.obj[[1]]
    Indice <- model.frame(formula, Data, na.action = na.pass)
    if (storage.mode(as.matrix(Indice)) != "character") {
        Name <- colnames(Indice)
        Indice <- as.data.frame(fastNumchange(as.matrix(Indice)))
        names(Indice) <- Name
    }
    out <-
        if (formula != "~1")
            by(Dframe, Indice, FUN, ...)
        else
            FUN(Dframe, ...)
    if (substitute(FUN) == "mroPara") 
        class(out) <- c("by", "bymrocalc")
    out
}

ses.diff <- function(obj) {
    obj <- as.matrix(obj)
    n <- nrow(obj)
    P <- colMeans(obj)
    Q <- colMeans(1 - obj)
    ses <- sqrt(P * Q / n)
    p12 <- t(obj) %*% (1 - obj) / n
    p21 <- t(1 - obj) %*% obj / n
    ## equation (3.6)
    ses.diffs <- sqrt(((p12 + p21) -(p12 - p21)^2 ) / n)
    ses.moecalc(ses = ses, ses.diffs = ses.diffs)
}

mroPara <- function(obj, conf.levels = 1.96, nonparallel = NULL) {
    cl <- match.call()
    Tb <- deparse(substitute(obj))
    if (! is.null(nonparallel))
        Tb <- nonparallel
    
    # TODO: magic number, get by name instead
    if (inherits(obj, "mro"))
        obj <- obj[[1]]
    
    n <- nrow(obj)
    estP <- colMeans(obj)
    SesDiff <- ses.diff(obj)
    
    # cov = mean((obj[,i]-colMeans(obj[,i]))*(obj[,j]-colMeans(obj[,j])))
    # we should calculate cov/n  the covariance of hat.
    covs <- cov(obj) / n
    variance <- diag(covs) #*(n-1)/n
    mromoecalc2 <- moecalc(x = SesDiff, est = estP)            
    mromoecalc2$fit$df <- n
    multi <- multicomp(mromoecalc2)
    out <- list(Topic = Tb, Variance = variance, Cov = covs,
                Mromoecalc = mromoecalc2, Multicom = multi)
    class(out) <- "mrocalc"
    out
}

print.mrocalc2 <- function(x, ...) {
    cat("For grab things try format(object)\n")
    cat("For summary analysis try summary(object)\n")
}

format.mrocalc <- function(x, ...) {
    OBJ.n <- deparse(substitute(x))
    Root <- paste0(OBJ.n, "$")
    Title <- paste0(Root, names(x))
    out <- data.frame(Function = c("estimated variance",
                                   "covariance matrix",
                                   "moecalc", "multicomp"))
    rownames(out) <- Title
    print(out)
}

chisq.mro <- function(mropa) {
    VCm <- mropa$Cov
    x <- mropa$Mromoecalc$est
    mu <- mean(mropa$Mromoecalc$est)
    t(x - mu) %*% solve(VCm) %*% (x - mu)
}

chisq.mro.by <- function(bymro) {
    n <- length(bymro)
    chiv <- numeric(n)
    p <- numeric(n)
    for (i in seq_len(n)) {
        cm <- bymro[[i]]$Cov
        add <- bymro[[i]]$Mromoecalc$est
        p[i] <- ncol(cm)
        chiv[i] <- t(add - mean(add)) %*% solve(cm) %*% (add - mean(add))
    }
    list(xvalue = sum(chiv),
         df = p * (n - 1),
         pv = pchisq(sum(chiv), df = p * (n - 1), lower.tail = FALSE))
}

barplot.mrocalc <- function(height, Order = NULL, horiz = FALSE,
                            title = NULL, bbar.col = "Alice Blue",
                            fbar.col = "red", dl.col = "Dark Khaki",
                            comi.col = "green", coni.col = "black",
                            Par = FALSE, label.las = NULL, ...) {
    mc <- match.call()
    objName <- height$Topic
    
    obj <- height$Mromoecalc

    if (! Par)
        par(col = "black", font = 1, cex = 1)
    
    if (is.null(label.las))
        label.las <- if (horiz) 3 else 1
    
    n <- length(height$est)
    Od <-
        if (! is.null(Order))
            order(height$est, decreasing = Order == "decreasing")
        else
            seq_len(n)

    width <- 1
    gray <- rep(1, n)
    dev.hold()
    if (! horiz) {
        Label <- names(height$est)[Od]
        xmedian <- barplot(gray, col = bbar.col, horiz = horiz)
        xmedian <- as.vector(xmedian)
        dis <- width / 2
        x1 <- xmedian - dis
        x2 <- xmedian + dis
        height <- height$est[Od]
        rect(x1, 0, x2, height, col = fbar.col)
        compL <- height$compL[Od]
        compU <- height$compU[Od]
        confL <- height$confL[Od]
        confU <- height$confU[Od]
        segments(xmedian, pmax(confL,0),
                 xmedian, pmin(confU,1), 
                 lty = 1,col = coni.col)
        segments(xmedian, pmax(compL,0),
                 xmedian, pmin(compU,1),
                 col = comi.col, lwd = 4,lty=1)
        abline(h = pmax(compL, 0), col = dl.col, lty = 3)
        abline(h = pmin(compU, 1), col = dl.col, lty = 3)
        axis(1, xmedian, Label, las = label.las)
        if (! is.null(title))
            do.call("title", title)
        else
            graphics::title(main = paste("Proportions in categories of",
                                         objName),
                            ylab = "Proportion", xlab = objName)
    } else {
        par(lty = 1)
        Od <- rev(Od)
        Label <- names(height$est)[Od]
        xmedian <- barplot(gray, width = width, col = "Alice Blue",
                           horiz = horiz, ...)
        xmedian <- as.vector(xmedian)
        dis <- width / 2
        x1 <- xmedian - dis
        x2 <- xmedian + dis
        height <- height$est[Od]
        rect(0, x1, height, x2, col = "red")
        compL <- height$compL[Od]
        compU <- height$compU[Od]
        confL <- height$confL[Od]
        confU <- height$confU[Od]
        segments(confL, xmedian, confU, xmedian)
        segments(compL, xmedian, compU, xmedian, col = "green", lwd = 4)
        abline(v = compL, col = "Dark Khaki", lty = 3)
        abline(v = compU, col = "Dark Khaki", lty = 3)
        axis(2, xmedian, Label, las = label.las)
        if (! is.null(title))
            do.call("title",title)
        else
            graphics::title(main = paste("Proportions in categories of",
                                         objName),
                            xlab = "Proportion", ylab = objName)
    }
    box()
    dev.flush()
    invisible(list(xmedian = xmedian,
                   mc = mc))
}

plot.mrobarchart <- function(x = list(), ...) {
    if (length(x) > 5)
        stop("too much to read")
    if (length(x) == 2) {
        opar <- par(mfrow = c(2, 1))
        on.exit(par(opar))
    } else {
        layout(matrix(c(1, 1, 2:length(x)),
                      ceiling(length(x) / 2),
                      2, byrow = TRUE))
    }
    for (i in seq_along(x))
       eval(x[[i]]$mc)
}

plot.bymrocalc <- function(x, show = NULL, ...) {
    pl <- match.call()
    out <- list()
    if (is.null(show)) {
        n <- length(x)
        for (i in seq_along(x)) {
            opar <- par(mfrow = c(1, n))
            dev.hold()
            barplot(x[[i]], ...)
            mtext(names(x)[i])
            dev.flush()
        }
    } else {
        par(mfrow = c(1, length(show)))
        for (j in show) {
            dev.hold()
            barplot(x[[j]], ...)
            mtext(names(x)[j])
            dev.flush()
        }
    }
    par(mfrow = c(1, 1))
    out$mc <- pl
    invisible(out)
}

crossTab <- function(bymro) {
    rn <- dimnames(bymro)[[1]]
    m <- do.call("rbind",
                 lapply(seq_along(bymro),
                        function(x) bymro[[x]]$Mromoecalc$est))
    rownames(m) <- rn
    m
}
    
between <- function(bymro) {
    rn <- dimnames(bymro)[[1]]
    M <- do.call("rbind",
                 lapply(seq_along(bymro),
                        function(x) {
                            # TODO: work out why these indices are special.
                            #       Names would be much better.
                            tmpdf <- as.data.frame(bymro[[x]]$Mromoecalc[
                                                   c(2, 12, 4, 14:17)])
                            as.matrix(tmpdf)
                        }))
    # number of multiple-response variables
    l <- length(unique(rownames(M)))
    L <- vector("list", 2 * l)
    for (j in seq_len(l)) {
        L [[2 * j - 1]] <- M[seq(j, l * length(bymro), by = l), ]
        rownames(L[[2 * j - 1]]) <- rn
        index <- combn(length(rn), 2)
        Groups <- M[seq(j, nrow(M), by = l), ]
        # set name!
        groupNames <- matrix(rn[index],
                             nrow = ncol(index), ncol = 2, byrow = TRUE)
        groupNames <- paste0(groupNames[, 1], " - ", groupNames[, 2])
        est <- Groups[index[1, ], 1]- Groups[index[2,], 1]
        ses <- sqrt(Groups[index[1, ], 2]^2 + Groups[index[2, ], 2]^2) 
        confL <- est - 1.96 * ses
        confU <- est + 1.96 * ses
        L[[2 * j]] <- cbind(est, ses, confL, confU)
        rownames(L[[2 * j]]) <- groupNames
    }
    names(L)[seq(1, 2 * l, by = 2)] <- unique(rownames(M))
    names(L)[seq(2, 2 * l, by = 2)] <-
        paste(unique(rownames(M)), "diff", sep = ".")
    class(L) <- "between"
    # should include a warning that the t multiplier is not 1.96 if
    # the sample size is small
    L
}

# TODO: Consider using hcl() for colouring,
#       ensuring more distinct colourings
barplot.between <- function(height, FUN = heat.colors, ...) {
    k <- length(height)
    Dframe <- t(as.data.frame(unclass(height[seq(1, k, by = 2)])))
    jump.num <- 7
    var.num <- k/2
    Mat <- t(Dframe[1 + jump.num *0:(var.num-1),])
    colnames(Mat) <- names(height)[seq(1, k, by = 2)]
    dev.hold()
    p <- barplot(Mat, beside = TRUE, col = FUN(ncol(Dframe)),
                 ylim = 0:1, ylab = "Proportion",
                 legend = colnames(Dframe), ...)
    box()
    ConfL <- Dframe[4 + jump.num * 0:(var.num - 1), ]
    ConfU <- Dframe[5 + jump.num * 0:(var.num - 1), ]
    CompL <- Dframe[6 + jump.num * 0:(var.num - 1), ]
    CompU <- Dframe[7 + jump.num * 0:(var.num - 1), ]
    for (i in seq_len(k / 2)) {
        segments(p[,i], CompL[i,], p[,i ], CompU[i,],
                 col = "green", lwd = 4)
        segments(p[,i ], ConfL[i,], p[,i ], ConfU[i,])
    }
    dev.flush()
    invisible(p)
}

summary.bymrocalc <- function(object, comp = "basic", ...) {
    cat("Proportions:\n")
    tabprops <- crossTab(object)
    cat("\n")
    print(tabprops, digits = 3)
    cat("--------------------------------------------------------\n")
    if (comp == "within") {
        class(object) <- "within"
        summary(object)
    }
    if (comp == "between"){
        src <- between(object)
        summary(src, object)
    } 
}

summary.within <- function(object, ...) {
    Title <- paste0(names(dimnames(object)),
                    ":",
                    dimnames(object)[[1]])
    Multicom.Group <- vector("list", length(object))
    for (i in seq_along(object)) {
        cat(Title[i] , "\n")  
        # TODO: Magic numbers, use names instead
        WithinVars <- as.data.frame(object[[i]]$Mromoecalc[c(2, 12, 14, 15)])
        print(round(WithinVars, 3))
        cat("\n")
        cat("Chisq test for uniformity: chisq =",
            chisq.mro(object[[i]]),
            " , df =",
            length(object[[i]]$Mromoecalc$est),
            " p-value =",
            pchisq(q = chisq.mro(object[[i]]),
                   df = length(object[[i]]$Mromoecalc$est),
                   lower.tail = FALSE),
            "\n")
        cat("\n")
        cat(paste0("95% CIs for diffs in propns within ", 
                   Title[i], " distribution"),"\n")
        cat("(rowname - colname)","\n")
        cat("\n")
        print(round(object[[i]]$Multicom, 3))
        Multicom.Group[[i]] <- object[[i]]$Multicom
        cat("\n")
        cat("----------------------------------------------------------------------")
        cat("\n")
    }
    invisible(list(Title = Title,
                   WithinVars = WithinVars,
                   Multicom.Group = Multicom.Group))
}

summary.between <- function(Source, bymro) {
    k <- length(Source)
    Title <- names(Source)[seq(1, k, by = 2)] 
    G <- chisq.mro.by(bymro)
    cat("        Chi-squared test\n")
    cat("     equality of response profile\n")
    cat("     across group\n")
    cat("\n")
    cat("X-squared = ", G$xvalue, " df = ", G$df, " p-value = ", G$pv, "\n")
    cat("\n")
    WithinVariables <- vector("list", k / 2)
    BetweenVariables <- vector("list", k / 2)
    for (i in seq_len(k / 2)) {
        cat(Title[i] , "\n") 
        cat("\n")
        # TODO: fix magic number, use names instead
        print(round(Source[[2 * i - 1]][, c(1, 2, 4, 5)], 3))
        WithinVariables[[i]] <- Source[[2 * i - 1]]
        cat("\n")
        cat(paste("95% CIs for difference ",
                  Title[i],
                  " proportions between ",
                  names(dimnames(bymro)), sep = ""), "\n")
        cat("(rowname - colname)\n")
        cat("\n")
        print(round(Source[[2 * i]], 3))
        BetweenVariables[[i]] <- Source[[2 * i]]
        cat("\n")
        cat("----------------------------------------------------------------------")
        cat("\n")
    }
    invisible(list(Title = Title,
                   WithinVariables = WithinVariables,
                   BetweenVariables = BetweenVariables))
}

summary.mrocalc <- function(object, ...) {
    list(df = object$Mromoecalc$fit$df,
         Mromoecalc = round(summary(object$Mromoecalc)$coef, 3),
         Multicom = round(object$Multicom, 3))
}

