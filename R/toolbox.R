#' Help mro variables extract common name out
#'
#' @title Extract Common Name from variables
#' @param obj It can be a vector or data frame, however, \code{substrsplit} is usually
#' used in the \code{iNZightMR} function.
#' @return A list with common character and unique variable name respectively
#' @export
#' @examples
#' substrsplit(c("varx", "vary", "varz"))
substrsplit <- function(obj) {
    str <- names(obj) # if obj is not a vector, str will be NULL
    if (is.vector(obj))
        str <- obj

    n <- max(nchar(str))
    i <- 0
    while (length(unique(substr(str, 1, i))) == 1) {
        i <- i + 1
    }
    commonstr <- unique(substr(str, 1, i - 1))
    varname <- substr(str, i, n)

    list(
        Commonstr = commonstr,
        Varname   = varname
    )
}

crossTab <- function(bymro) {
    k <- length(bymro)
    rn <-
        if (length(dim(bymro)) < 2) {
            dimnames(bymro)[[1]]
        } else {
            outer(
                dimnames(bymro)[[1]],
                dimnames(bymro)[[2]],
                paste,
                sep = " & "
            )
        }

    m <- do.call("rbind",
        lapply(seq_along(bymro),
            function(x) {
                if (is.null(bymro[[x]])) {
                    return(0)
                }
                bymro[[x]]$Mromoecalc$est
            }
        )
    )
    #m <- matrix(m, nrow=length(rn))
    rownames(m) <- rn
    m
}


sampleSize <- function (bymro) {

    # sampleSize is a function extract sample size from the list

    if (inherits(bymro, "bymrocalc")) {
        k <- length(bymro)
        rn <- dimnames(bymro)[[1]]
        cn <- if (length(dim(bymro)) < 2) {
            NULL
        } else {
            dimnames(bymro)[[2]]
        }
        m <- do.call("rbind",
            lapply(seq_along(bymro),
                function(x) {
                    if (is.null(bymro[[x]]))
                        return(0)
                    bymro[[x]]$Mromoecalc$fit$df
                }
            )
        )
        m <- matrix(m, nrow = length(rn))
        rownames(m) <- rn
        if (!is.null(cn))
            colnames(m) <- cn
        return(m)
    } else {
        # a subset of bymro not having class "bymrocalc" where it is a list
        # so a list method is providing here for subset of bymro
        k <- length(bymro)
        rn <- names(bymro)
        # The subset for bymro implies one dimension
        m <- do.call("rbind",
            lapply(seq_along(bymro),
                function(x) {
                    if (is.null(bymro[[x]]))
                        return(0)
                    bymro[[x]]$Mromoecalc$fit$df
                }
            )
        )
        rownames(m) <- rn
        return(m)
    }
}


#' Between SEs
#'
#' @title Compute Between se's
#' @param bymro a bymro object
#' @return something about between.
#' @author Junjie Zheng
#' @export
#' @examples
#' mr <- iNZightMR(online ~ onlinegame + onlinevideo + onlinemusic,
#'     data = census.at.school.5000)
#' (bt <- between(byMRO(mr, ~gender, mroPara)))
#'
#' if (requireNamespace("iNZightPlots"))
#'     barplotMR(bt)
between <- function (bymro) {
    dn <- dimnames(bymro)
    if (length(dn) < 2) {
        tab <- sampleSize(bymro)
        # The print below is to test whether we are using the correct sample size (ns) in seBinprops().
        # print(tab[,1])
        # print(tab)
        #

        rn <- names(bymro)
        k <- length(bymro)
        temp <- crossTab(bymro)
        mro.names <- colnames(temp)
        M <- matrix(NA, nrow = ncol(temp) * k, ncol = 8)
        M[, 8] <- rep(tab, times = length(mro.names))
        rownames(M) <- rep(names(bymro), times = ncol(temp))

        for (i in seq_along(bymro)) {
            if (is.null(bymro[[i]])) {

            } else {
                tmpdf <- as.data.frame(
                    bymro[[i]]$Mromoecalc[c(2, 12, 4, 14:17)],
                    stringsAsFactors = TRUE
                )
                # we need to recalculate the comparison interval here.
                # we only need to inherit {est, ses, confL, confU} from within case
                # then we calculate ErrBars and compL,compU here
                M[seq(i, ncol(temp) * k, by = k), 1:7] <- as.matrix(tmpdf)
            }
        }
        colnames(M) <-
            c("est", "ses", "ErrBars", "confL", "confU", "compL", "compU", "count")
        mr.col <- length(unique(rownames(M)))
        isNA <- apply(is.na(M[, 1:7]), 1, all)
        for (x in 1:ncol(temp)) {
            j <- nrow(temp) * (x - 1) + 1
            index <- seq(j, j + mr.col - 1)
            notNA <- (!isNA)[index]
            phat <- M[index, "est"]
            names(phat) <- rn
            raw <- moecalc(
                seBinprops(tab[which(notNA), 1], phat[notNA]),
                est = phat[notNA]
            )
            M[index[notNA], "ErrBars"] <- raw$ErrBars
            M[index[notNA], "compL"] <- raw$compL
            M[index[notNA], "compU"] <- raw$compU
        }
        l <- length(mro.names)
        L <- vector("list", 2 * l)
        for (j in seq_len(l)) {
            k <- nrow(temp) * (j - 1) + 1
            index <- seq(k, k + mr.col - 1)
            notNA <- (!isNA)[index]
            L[[2 * j - 1]] <- M[index, ]
            rownames(L[[2 * j - 1]]) <- rn
            id <- combn(length(rn[notNA]), 2)
            Groups <- M[index[notNA], ]
            groupNames <- matrix(rn[id],
                nrow = ncol(id),
                ncol = 2,
                byrow = TRUE
            )
            groupNames <- paste0(groupNames[, 1], " - ", groupNames[, 2])
            est <- Groups[id[1, ], 1] - Groups[id[2, ], 1]
            ses <- sqrt(Groups[id[1, ], 2]^2 + Groups[id[2, ], 2]^2)
            confL <- est - 1.96 * ses
            confU <- est + 1.96 * ses
            L[[2 * j]] <- cbind(est, ses, confL, confU)
            rownames(L[[2 * j]]) <- groupNames
        }
        names(L)[seq(1, 2 * l, by = 2)] <- mro.names
        names(L)[seq(2, 2 * l, by = 2)] <- paste(mro.names, "diff", sep = ".")
        class(L) <- "between"

        attr(L, "type1") <- names(dimnames(bymro))
        notNULL <- which(!sapply(bymro, is.null))
        attr(L, "Topic") <- bymro[[notNULL[1]]]$Topic
        L
    } else {
        dimension <- dim(bymro)
        mat <- matrix(seq_along(bymro),
            ncol = dimension[1],
            nrow = dimension[2],
            byrow = TRUE
        )
        dimname <- dimnames(bymro)
        combnname <- merge(dimname[1], dimname[2])
        out <- vector("list", nrow(mat))
        for (i in seq_len(nrow(mat))) {
            subname <- combnname[mat[i, ], 1]
            bigname <- unique(combnname[, 2])
            #In <- bymro[mat[i, ]]
            In <- bymro[][,i]
            #names(In) <- subname
            out[[i]] <- between(In)
            attr(out[[i]], "type1") <- names(dimnames(bymro))[1]
        }
        names(out) <- bigname
        class(out) <- c("b2", "between")
        attr(out, "type2") <- names(dimnames(bymro))[2]
        notNULL <- which(!sapply(bymro,is.null))
        attr(out, "Topic") <- bymro[[notNULL[1]]]$Topic
        out
    }

}


validateRange1 <- function(table) {
    # validateRange1 is for checking count <=5, num>=1 or num<=0
    id <- table$count<=5
    table[id, c("ErrBars","confL","confU","compL","compU")] <- 0
    table$confL <- pmax(table$confL,0)
    table$compL <- pmax(table$compL,0)
    table$confU <- pmin(table$confU,1)
    table$compU <- pmin(table$compU,1)
    table
}

validateRange2 <- function(x) {
    id <- is.na(x)
    x[id] <- 0
    x
}
