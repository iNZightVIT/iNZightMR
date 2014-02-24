\name{between}
\alias{between}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
between(bymro)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bymro}{
%%     ~~Describe \code{bymro} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (bymro) 
{
    dn <- dimnames(bymro)
    if (length(dn) < 2) {
        rn <- names(bymro)
        k <- length(bymro)
        M <- do.call("rbind", lapply(seq_along(bymro), function(x) {
            tmpdf <- as.data.frame(bymro[[x]]$Mromoecalc[c(2, 
                12, 4, 14:17)])
            as.matrix(tmpdf)
        }))
        l <- length(unique(rownames(M)))
        L <- vector("list", 2 * l)
        for (j in seq_len(l)) {
            L[[2 * j - 1]] <- M[seq(j, l * length(bymro), by = l), 
                ]
            rownames(L[[2 * j - 1]]) <- rn
            index <- combn(length(rn), 2)
            Groups <- M[seq(j, nrow(M), by = l), ]
            groupNames <- matrix(rn[index], nrow = ncol(index), 
                ncol = 2, byrow = TRUE)
            groupNames <- paste0(groupNames[, 1], " - ", groupNames[, 
                2])
            est <- Groups[index[1, ], 1] - Groups[index[2, ], 
                1]
            ses <- sqrt(Groups[index[1, ], 2]^2 + Groups[index[2, 
                ], 2]^2)
            confL <- est - 1.96 * ses
            confU <- est + 1.96 * ses
            L[[2 * j]] <- cbind(est, ses, confL, confU)
            rownames(L[[2 * j]]) <- groupNames
        }
        names(L)[seq(1, 2 * l, by = 2)] <- unique(rownames(M))
        names(L)[seq(2, 2 * l, by = 2)] <- paste(unique(rownames(M)), 
            "diff", sep = ".")
        class(L) <- "between"
        L
    }
    else {
        dimension <- dim(bymro)
        mat <- matrix(seq_along(bymro), ncol = dimension[1], 
            nrow = dimension[2], byrow = TRUE)
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
        class(out) <- c("between", "b2")
        out
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
