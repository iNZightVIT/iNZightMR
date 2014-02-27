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
    mro.mat <- model.frame(frm, data, na.action = na.pass, ...)
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
  if (length(unique(x)) < 10)
    factor(x)
  else
    cut(x, style + c(-1, 0, 0, 0, 0))
} 

by.mro <- function(mro.obj, formula, FUN, ...) {
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
    nm <- colnames(Indice)
    Indna <- is.na(Indice)
    Indice <- Indice[! Indna]
    # must be return the data frame so that the by can detect name
    Indice <- fastNumchange(Indice) 
    Indice <- as.data.frame(Indice)
    colnames(Indice) <- nm
  }
  
  Dframe <- Dframe[!Indna, ]
  Data <- Data[!Indna, ]
  
  if (substitute(FUN) == "mroPara") {
    out <- by(Dframe, Indice, FUN,
              nonparallel = names(mro.obj)[1])
    class(out) <- c("by", "bymrocalc")
  } else {
    out <- by(Dframe, Indice, FUN, ...)
  }
  
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
  ses.diffs <- sqrt(((p12 + p21) -(p12 - p21)^2) / n)
  ses.moecalc(ses = ses, ses.diffs = ses.diffs)
}

mroPara <- function(obj, conf.levels = 1.96, nonparallel = NULL) {
  cl <- match.call()
  Tb <- deparse(substitute(obj))
  if (! is.null(nonparallel))
    Tb <- nonparallel
  
  
  if (inherits(obj, "mro"))
    obj <- obj[[1]]
  
  n <- nrow(obj)
  estP <- colMeans(obj)
  SesDiff <- ses.diff(obj)
  
  #covs <- cov(obj) / n
  covs <- cov(obj)
  variance <- diag(covs) #*(n-1)/n
  mromoecalc2 <- moecalc(x = SesDiff, est = estP)            
  mromoecalc2$fit$df <- n
  multi <- multicomp(mromoecalc2)
  out <- list(Topic = Tb, Variance = variance, Cov = covs,
              Mromoecalc = mromoecalc2, Multicom = multi)
  class(out) <- "mrocalc"
  out
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
       df = (p * (n - 1))[1],
       pv = pchisq(sum(chiv), df = p * (n - 1), lower.tail = FALSE)[1])
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
  
  n <- length(obj$est)
  Od <-
    if (! is.null(Order))
      order(obj$est, decreasing = Order == "decreasing")
  else
    seq_len(n)
  
  width <- 1
  gray <- rep(1, n)
  dev.hold()
  if (! horiz) {
    Label <- names(obj$est)[Od]
    xmedian <- barplot(gray, col = bbar.col, horiz = horiz)
    xmedian <- as.vector(xmedian)
    dis <- width / 2
    x1 <- xmedian - dis
    x2 <- xmedian + dis
    height <- obj$est[Od]
    rect(x1, 0, x2, height, col = fbar.col)
    compL <- obj$compL[Od]
    compU <- obj$compU[Od]
    confL <- obj$confL[Od]
    confU <- obj$confU[Od]
    segments(xmedian, pmax(confL, 0),
             xmedian, pmin(confU, 1), 
             lty = 1,col = coni.col)
    segments(xmedian, pmax(compL, 0),
             xmedian, pmin(compU, 1),
             col = comi.col, lwd = 4,lty = 1)
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
    Label <- names(obj$est)[Od]
    xmedian <- barplot(gray, width = width, col = "Alice Blue",
                       horiz = horiz, ...)
    xmedian <- as.vector(xmedian)
    dis <- width / 2
    x1 <- xmedian - dis
    x2 <- xmedian + dis
    height <- obj$est[Od]
    rect(0, x1, height, x2, col = "red")
    compL <- obj$compL[Od]
    compU <- obj$compU[Od]
    confL <- obj$confL[Od]
    confU <- obj$confU[Od]
    segments(confL, xmedian, confU, xmedian)
    segments(compL, xmedian, compU, xmedian, col = "green", lwd = 4)
    abline(v = compL, col = "Dark Khaki", lty = 3)
    abline(v = compU, col = "Dark Khaki", lty = 3)
    axis(2, xmedian, Label, las = label.las)
    if (! is.null(title))
      do.call("title", title)
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





crossTab <- function(bymro) {
  k <- length(bymro)
  rn <- 
    if (length(dim(bymro)) < 2) {
      dimnames(bymro)[[1]]
    } else {
      outer(dimnames(bymro)[[1]], dimnames(bymro)[[2]],
            paste, sep = " & ")
    }
  
  m <- do.call("rbind",
               lapply(seq_along(bymro),
                      function(x) bymro[[x]]$Mromoecalc$est))
  rownames(m) <- rn
  m
}

between <- function(bymro) {
  dn <- dimnames(bymro)
  if (length(dn) < 2) {
    rn <- names(bymro)
    k <- length(bymro) # number of single response
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
      est <- Groups[index[1, ], 1] - Groups[index[2, ], 1]
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
  } else {
    dimension <- dim(bymro)
    mat <- matrix(seq_along(bymro),
                  ncol = dimension[1], nrow = dimension[2], byrow = TRUE)
    dimname <- dimnames(bymro)
    combnname <- merge(dimname[1], dimname[2])
    out <- vector("list", nrow(mat))
    for (i in seq_len(nrow(mat))) {
      subname <- combnname[mat[i, ], 1]
      bigname <- unique(combnname[, 2])
      In <- bymro[mat[i, ]]
      names(In) <- subname
      out[[i]] <- between(In)
    }
    names(out) <- bigname
    class(out) <- c("b2", "between")
    out
  }
}

barplot.b2 <- function(x, which = NULL, ...) {
  obj <- x
  if (! is.null(which) && length(which) == 1)
    return({barplot.between(obj[[which]]);title(xlab = names(obj[which]))})
  if (! is.null(which))
    obj <- obj[which]
  Name <- names(obj)
  k <- length(obj)
  fake <- k %% 2 
  Lead <- fake + k
  layout(cbind(matrix(1:Lead, nrow = Lead / 2, ncol = 2, byrow = TRUE),
               Lead + 1), widths = c(1, 1, 0.5), heights = rep(1, Lead / 2))
  for(i in 1:Lead) {
    barplot.between(obj[[i]], main = Name[i], LEG = FALSE)  
  }
  plot.new()
  legend(0, 0.5, rownames(obj[[1]][[1]]),
         fill = heat.colors(length(rownames(obj[[1]][[1]]))))
  title(xlab = "online", outer = TRUE)
  #legend("right",fill=c("yes","no"),col=heat.colors(1:4))
  par(mfrow=c(1,1))
}




# TODO: Consider using hcl() for colouring,
barplot.between <- function(height, LEG = TRUE, FUN = heat.colors, ...) {
  #if (inherits(height, "b2")) 
  #    return(plot.b2(height, LEG = LEG))
  k <- length(height)
  Dframe <- t(as.data.frame(unclass(height[seq(1, k, by = 2)])))
  jump.num <- 7
  var.num <- k / 2
  Mat <- t(Dframe[1 + jump.num * 0:(var.num - 1), ])
  colnames(Mat) <- names(height)[seq(1, k, by = 2)]
  dev.hold()
  p <-
    if (LEG) {na
      layout(matrix(c(2,2,2,2,1), 1, 5, byrow = TRUE))
      plot.new()
      plot.window(0:1, 0:1)
      par(oma = )
      legend("center", rownames(Mat), title = "Group", fill = FUN(nrow(Mat)),bty="n")
      tmp <- barplot(Mat, beside = TRUE, col = FUN(ncol(Dframe)),
                     ylim = 0:1, ylab = "Proportion", ...)
      #legend(par("usr")[2] + 2, 0.6, rownames(Mat),
      #       fill = FUN(nrow(Mat)), xpd = NA)
      title(main = "Proportion comparison between case")
      tmp
    } else {
      barplot(Mat, beside = TRUE, col = FUN(ncol(Dframe)),
              ylim = 0:1, ylab = "Proportion",
              #legend = colnames(Dframe), 
              ...)
    }
  box()
  ConfL <- Dframe[4 + jump.num * 0:(var.num - 1), ]
  ConfU <- Dframe[5 + jump.num * 0:(var.num - 1), ]
  CompL <- Dframe[6 + jump.num * 0:(var.num - 1), ]
  CompU <- Dframe[7 + jump.num * 0:(var.num - 1), ]
  for (i in seq_len(k / 2)) {
    segments(p[, i], CompL[i, ], p[, i], CompU[i, ],
             col = "green", lwd = 4)
    segments(p[, i], ConfL[i, ], p[, i], ConfU[i, ])
  }
  dev.flush()
  par(mfrow = c(1,1))
  invisible(p)
}
#       ensuring more distinct colourings





## use but problem
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
  Title <-
    if (length(dim(object)) < 2) {
      paste0(names(dimnames(object)), ":", dimnames(object)[[1]])
    } else {
      commonTitle <- 
        paste0(names(dimnames(object)[1]), ":", names(dimnames(object)[2]))
      groupTitle <- outer(dimnames(object)[[1]], dimnames(object)[[2]],
                          paste, sep = " & ")
      paste(commonTitle, groupTitle, sep = " = ")
    }
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
  G <- chisq.mro.by(bymro)
  cat("        Chi-squared test\n")
  cat("     equality of response profile\n")
  cat("     across group\n")
  cat("\n")
  cat("X-squared = ", G$xvalue, " df = ", G$df, " p-value = ", G$pv, "\n")
  cat("\n")
  
  DN <- dimnames(bymro)
  if (length(DN) < 2) {
    k <- length(Source)
    Title <- names(Source)[seq(1, k, by = 2)] 
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
  } else {
    s <- length(Source)
    partcontent <- vector("list", length(Source))
    for(j in seq_along(Source)){
      partsource <- Source[[j]]
      cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
      cat(paste(names(DN)[2], names(Source)[j], sep=" : "),"\n")
      cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
      cat("\n")
      k <- length(partsource)
      Title <- names(partsource)[seq(1, k, by = 2)] 
      WithinVariables <- c()
      BetweenVariables <- c()
      for (i in 1:(k / 2))  {
        cat(Title[i] , "\n")
        cat("\n")
        print(round(partsource[[2 * i - 1]][, c(1, 2, 4, 5)], 3))
        WithinVariables <- rbind(WithinVariables, partsource[[2 * i - 1]])
        cat("\n")
        cat(paste("95% CIs for difference ",Title[i]," proportions between ",names(dimnames(bymro)[2]),sep=""),"\n")
        cat("(rowname - colname)","\n")
        cat("\n")
        print(round(partsource[[2 * i]], 3))
        BetweenVariables <- rbind(BetweenVariables, partsource[[2 * i]])
        cat("\n")
        cat("----------------------------------------------------------------------")
        cat("\n")
      }
      partcontent[[j]] <- list(Title = Title,
                               WithinVariables = WithinVariables,
                               BetweenVariables = BetweenVariables)  
      names(partcontent)[j] <- paste(names(DN)[2], names(Source)[j], sep=" : ")
    }
    invisible(partcontent)
  }
}

# summary(mroPara(dm)) should upgraded
summary.mrocalc <- function(object, ...) {
  list(df = object$Mromoecalc$fit$df,
       Mromoecalc = round(summary(object$Mromoecalc)$coef, 3),
       Multicom = round(object$Multicom, 3))
}



mro2 <- function(frm, data, Labels = NULL, inverse = FALSE, combi = NULL, ...) {
  # y ~ v1 + v2 + v3 length is 3, ~v1 + v2 + v3 is length of two.
  if (length(frm[[2]])) 
    classnames <- as.character(frm[[2]])
  
  display <- with(data, {
    # grab variable name from the formual (frm) in the data file (data))
    mro.mat <- model.frame(frm[-2], data, na.action = na.pass, ...)
    Ind <- as.logical(rowSums(is.na(mro.mat)))
    mro.mat <- mro.mat[! Ind, ]
    data <- data[! Ind, ]
    rownames(data) <- NULL
    details <- attributes(mro.mat)
    variables <- attr(details$terms, "variables")
    
    # test binary level
    if (all(unique(sapply(mro.mat, nlevels)) == 2)) {
      mro.mat <- sapply(mro.mat, fastBinaryChange, inverse)
      ### mro function treat NA response as absent response in the original data set
    } else if (sum(which(sapply(mro.mat, nlevels) == 2)) == 0) {
      stop("Hard to detect binary pattern")
    } else {
      # use the levels of the first variables that have 2 levels
      index <- which(sapply(mro.mat, nlevels) == 2)[[1]]
      Commonlevels <- levels(mro.mat[, index])
      mro.mat <- sapply(mro.mat, fastBinaryChange, opts = Commonlevels)
      
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
    
    if (!is.null(combi)) {
      combination.index <- combn(ncol(mro.mat), combi)
      com.mro.mat <- c()
      com.lablename <- c()
      for (j in 1:ncol(combination.index)) {
        com.mro.mat <- cbind(com.mro.mat, 
                             mro.mat[, combination.index[1, j]] * mro.mat[, combination.index[2, j]])
        com.lablename <- append(com.lablename, 
                                paste(labelname[combination.index[1, j]], 
                                      labelname[combination.index[2, j]], sep = ":"))
      }
      colnames(com.mro.mat) <- com.lablename
      mro.mat <- com.mro.mat
      labelname <- com.lablename
    }
    
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

# debt to exist
subset.mro <- function(mro, subset) {
  r <- eval(substitute(subset), mro$df, parent.frame())
  #remove NA value
  rmna <- which(is.na(r))
  r <- r[-rmna]
  mro$df <- mro$df[-rmna, ]
  mro[[1]] <- mro[[1]][-rmna, ]
  mro$df <- mro$df[r, ]
  mro[[1]] <- mro[[1]][r, ]
  class(mro) <- c(class(mro), "Sub", deparse(substitute(subset)))
  mro
}



mrbarchart <- function(obj,Order = NULL, horiz = FALSE, title=NULL , 
                       bbar.col="Alice Blue",fbar.col="red", dl.col="Dark Khaki",
                       comi.col="green" , coni.col="black", tick=FALSE,
                       Par=FALSE , label.las = NULL, step=5, type=1, srt=NULL, ...) {
  #browser()                
  mc <- match.call()
  objName=obj$Topic
  
  obj = obj$Mromoecalc
  
  if(!Par) par(col="black",font=1,cex=1)
  
  if( is.null(label.las)) label.las=ifelse(horiz,3,1)
  
  
  if (!is.null(Order)) {
    if (Order == "decreasing")
      Od = order(obj$est, decreasing = T)
    if (Order == "increasing")
      Od = order(obj$est, decreasing = F)
  }
  n = length(obj$est)
  if (is.null(Order))
    Od = 1:n
  
  width = 1
  
  gray = rep(1, n) 
  if (!horiz) {
    if(step>0){
      #par(lty=0) 
      Od = Od
      Label = names(obj$est)[Od]
      xmedian = barplot(gray,col = bbar.col, horiz = horiz , axes=FALSE,ylim=c(-0.02,1.02))
      box(lwd=2)
      #rect(par("usr")[1],par("usr")[4],par("usr")[2],par("usr")[4]+0.05,xpd=TRUE,col="Dark Salmon",lwd=2)
    }
    if(step>1){
      xmedian = as.vector(xmedian)
      dis = width/2
      x1 = xmedian - dis
      x2 = xmedian + dis
      height = obj$est[Od]
      rect(x1, 0, x2, height, col = fbar.col)
    }
    if(step>2){
      compL = obj$compL[Od]
      compU = obj$compU[Od]
      confL = obj$confL[Od]
      confU = obj$confU[Od]
      segments(xmedian, pmax(confL,0), xmedian, pmin(confU,1),lty=1,col=coni.col)
      segments(xmedian, pmax(compL,0), xmedian, pmin(compU,1), col = comi.col, lwd = 4,lty=1)
    }
    if(step>3) {
      abline(h = pmax(compL,0), col = dl.col, lty = 3)
      abline(h = pmin(compU,1), col = dl.col, lty = 3)
    }
    if(step>4){
      if(type==1) axis(2,las=2)
      if(type==3) axis(4,labels=FALSE,las=2)
      if(type==4) axis(2,labels=FALSE,las=2)
      if(type==6) axis(4,las=2)
    }
    if(step>5) {
      if(is.null(srt)) axis(1, xmedian, Label, las = label.las,tick=tick)
      else text(xmedian,par("usr")[3]-0.025,srt=srt,adj=1,labels=Label,xpd=NA)
    }
    
    if(step>6){
      ##  title 
      if (!is.null(title))   do.call("title",title)         # title=  list(...)      or NULL
      else do.call("title",list(main=paste("Proportions in categories of",objName,sep=" "),ylab="Proportion",xlab=objName))
    }
    
  } else {
    par(lty=1)
    Od = Od[length(Od):1]
    Label = names(obj$est)[Od]
    xmedian = barplot(gray, width = width, col = "Alice Blue", horiz = horiz, 
                      ...)
    xmedian = as.vector(xmedian)
    dis = width/2
    x1 = xmedian - dis
    x2 = xmedian + dis
    height = obj$est[Od]
    rect(0, x1, height, x2, col = "red")
    compL = obj$compL[Od]
    compU = obj$compU[Od]
    confL = obj$confL[Od]
    confU = obj$confU[Od]
    segments(confL, xmedian, confU, xmedian)
    segments(compL, xmedian, compU, xmedian, col = "green", lwd = 4)
    abline(v = compL, col = "Dark Khaki", lty = 3)
    abline(v = compU, col = "Dark Khaki", lty = 3)
    axis(2, xmedian, Label, las = label.las)
    
    if (!is.null(title))   do.call("title",title)         # title=  list(...)      or NULL
    else do.call("title",list(main=paste("Proportions in categories of",objName,sep=" "),xlab="Proportion",ylab=objName))
  }
  box()
  
  invisible(list(xmedian=xmedian,mc=mc))
}


barchart3=function(bymrocalc,which=1:length(bymrocalc)) {
  # weired thing!!!!!!!!!!!
  s = attr(bymrocalc,"dim")
  
  
  # take out name 
  namespace = attr(bymrocalc,"dimnames")
  if(length(namespace)<2) {nameVec = namespace[[1]];h=1}
  else {
    #nameVec = outer(namespace[[1]],namespace[[2]],paste,sep=" & ")
    #nameVec = as.vector(t(nameVec)) 
    nameVec=apply(merge(namespace[1],namespace[2]),1,function(x) paste(x[1],x[2],sep=" & "))
    h = 2
  }  
  
  bymrocalc=unclass(bymrocalc)
  names(bymrocalc)=nameVec
  # reorder the bymrocalc
  k = length(bymrocalc)
  
  reorder = 1:k
  if(k>2 & h >1) reorder = c(seq(1,k,by=2),seq(2,k,by=2))
  if(length(s)>1 && s[1]>s[2]) reorder=1:k
  
  # use which to pick out and cover the original set
  bymrocalc=bymrocalc[reorder[which]]
  k = length(which)
  #nameVec=nameVec[which]
  if(k>5 & h<2) h=2
  mfrow=numeric(2)
  mfrow[1]=h
  mfrow[2]=ifelse(h>1,(k%/%2),k)
  
  
  #set graphy parameter 1
  U = c(4,5,6)
  L = c(1,2,3)
  G = k%/%2
  
  
  
  if(h==1) {
    par(mar=c(2,0,1,0),oma=rep(4,4),mfrow=mfrow)
    
    l=0
    for(i in 1: k) {
      if(i==1) j=L[1]
      else {
        j=ifelse(i==k,L[3],L[2])
      }
      mrbarchart(bymrocalc[[i]],type=j,srt=25,step=6-l)
      l=ifelse(l,0,1)
      mtext(names(bymrocalc[i]))
      #mtext(nameVec)
    }
  }
  else{
    par(mfrow=mfrow)
    
    par(oma=rep(4,4),mar=c(0.5,0,2,0))
    j=c()
    for (i in 1:G) {
      if(i==1) {j=U[1] }
      else{
        j=ifelse(i%%G,U[2],U[3])
      }
      mrbarchart(bymrocalc[[i]],type=j)
      mtext(names(bymrocalc[i]))
      #mtext(nameVec[i])
    }
    par(mar=c(1,0,1,0)) 
    l=0
    for(i in (G+1) : k) {
      if(i==(G+1) ) j=L[1]
      else {
        j=ifelse(i%%k,L[2],L[3])
      }
      mrbarchart(bymrocalc[[i]],type=j,srt=25,step=6-l,xpd=NA)
      l=ifelse(l,0,1)
      mtext(names(bymrocalc[i]))
      #mtext(nameVec[i])
    }
  }
  title(main="Proportion comparison",ylab="proportion",xlab="online",outer=TRUE)
}



barchart4=function(bymrocalc,XI=NULL,YI=NULL) {
  # weired thing!!!!!!!!!!!
  s = attr(bymrocalc,"dim")
  
  
  # take out name 
  namespace = attr(bymrocalc,"dimnames")
  if(length(namespace)<2) {nameVec = namespace[[1]];h=1}
  else {
    #nameVec = outer(namespace[[1]],namespace[[2]],paste,sep=" & ")
    #nameVec = as.vector(t(nameVec)) 
    nameVec=apply(merge(namespace[1],namespace[2]),1,function(x) paste(x[1],x[2],sep=" & "))
    h = 2
  }  
  
  bymrocalc=unclass(bymrocalc)
  names(bymrocalc)=nameVec
  # reorder the bymrocalc
  k = length(bymrocalc)
  
  if(!is.null(XI) | !is.null(YI)){
    if(length(s)==1) {
      #can not give YI value
      if(!is.null(YI)) stop("can't use YI option here")
      bymrocalc=bymrocalc[XI]
      k=length(bymrocalc)
    }
    else{
      mat = matrix(1:prod(s),ncol=s[1],nrow=s[2],byrow=TRUE)
      if(is.null(XI)) XI = 1:s[2]
      if(is.null(YI)) YI = 1:s[1]
      which = as.vector(t(mat[XI,YI]))
      s=length(which)
      h=1
      bymrocalc=bymrocalc[which]
      k=s
      
    }
  }
  
  if(k>5 & h<2) h=2
  
  # control default layout 
  if(length(s)>1) {mfrow=s[2:1] ; G=prod(s) - s[1] }
  else{
    mfrow=numeric(2)
    mfrow[1]=h
    mfrow[2]=s/h  # is s = 6 ,h=1, get 6, h=2,get 3. A vector have too much level always go two rows.
    G = s/h
  }
  
  #set graphy parameter 1
  U = c(4,5,6)
  L = c(1,2,3)
  
  
  
  if(k>1){
    if(h==1) {
      par(mar=c(2,0,1,0),oma=rep(4,4),mfrow=mfrow)
      l=0
      for(i in 1:k) {
        if(i==1) j=L[1]
        else {
          j=ifelse(i==k,L[3],L[2])
        }
        mrbarchart(bymrocalc[[i]],type=j,srt=25,step=6-l)
        l=ifelse(l,0,1)
        mtext(names(bymrocalc[i]))
        #mtext(nameVec)
      }
    }
    else{
      par(mfrow=mfrow)
      par(oma=rep(4,4),mar=c(0,0,2,0))
      j=c()
      g=(mfrow[1]-1)%%2
      r = seq(mfrow[2],G,by=mfrow[2])
      l = seq(1,G,by=mfrow[2])
      for (i in 1:G) {
        if(any(i==l)) {j=L[1] }
        else{
          j=ifelse(i%%(s[1]),L[2],L[3])
        }
        mrbarchart(bymrocalc[[i]],type=j+3*g,step=5)
        if(any(i==r)) g=g+1
        if(g>1) g=0
        
        mtext(names(bymrocalc[i]))
        #mtext(nameVec[i])
      }
      par(mar=c(1,0,1,0)) 
      l=0    ##  l  for control labels showing in space like piano
      for(i in (G+1) : k) {
        if(i==(G+1) ) j=L[1]
        else {
          j=ifelse(i%%k,L[2],L[3])
        }
        mrbarchart(bymrocalc[[i]],type=j,srt=25,step=6-l,xpd=NA)
        
        l=ifelse(l,0,1)
        mtext(names(bymrocalc[i]))
        #mtext(nameVec[i])
      }
    }
  }
  else {
    par(mfrow=c(1,1),oma=rep(4,4),mar=c(1,1,1,1))
    mrbarchart(bymrocalc[[1]],type=1,srt=25,step=6)
    mtext(names(bymrocalc))
  }    
  
  title(main="Proportion comparison",ylab="proportion",xlab="mro.mat", outer=TRUE)
  par(mfrow=c(1,1),oma=rep(0,4),mar=c(5.1, 4.1, 4.1, 2.1))
  
}
