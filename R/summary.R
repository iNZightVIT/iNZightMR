
# single mro summary

#' @export
summary.mrocalc <- function(object, ...) {
  list(df = object$Mromoecalc$fit$df,
       Mromoecalc = round(summary(object$Mromoecalc)$coef, 3),
       Multicom = round(object$Multicom, 3))
}

#' @export
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
    if(is.null(object[[i]])){
      cat("Missing Observations","\n")
      cat("\n")
      cat("----------------------------------------------------------------------")
      cat("\n")
      next
    }
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

#' @export
summary.between <- function(object, bymro,...) {
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


## wrapper function
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
