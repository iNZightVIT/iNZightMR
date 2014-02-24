\name{plotcombn}
\alias{plotcombn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotcombn(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function (obj) 
{
    Subtitle <- NULL
    if (inherits(obj, "Sub")) {
        Subtitle <- class(obj)[3]
        class(obj) <- "mro"
    }
    if (!inherits(obj, "data.frame") && !inherits(obj, "mro")) 
        stop("Invalid input. Must be 'data.frame' or 'mro'")
    dev.hold()
    layout(rbind(c(0, 0, 0, 0, 0), c(0, 4, 4, 4, 0), c(0, 0, 
        0, 0, 0), c(0, 2, 0, 5, 0), c(0, 0, 0, 0, 0), c(0, 1, 
        0, 3, 0), c(0, 0, 0, 0, 0)), heights = c(lcm(1), lcm(1), 
        lcm(1), lcm(5), lcm(1), 1, lcm(1)), widths = c(lcm(0.5), 
        1, lcm(0.1), lcm(3), lcm(0.5)))
    finaltable <- calcmissing(obj, "row")
    x <- finaltable
    row.x <- nrow(x)
    col.x <- ncol(x)
    row.num <- row.x - 1
    col.num <- col.x - 1
    total <- x[row.x, col.x]
    p2 <- 1 - x[row.x, 1:col.num]/total
    p3 <- x[1:row.num, col.x]/total
    x.fit <- finaltable[seq_len(row.num), seq_len(col.num)]
    opa <- par(mar = rep(0, 4))
    plot(2 * col.num, min(row.num, 30), type = "n", xlim = c(0, 
        2 * col.num), ylim = c(0, min(row.num, 30)), axes = FALSE, 
        xlab = "", ylab = "")
    x1 <- seq(0, 2 * col.num - 2, by = 2)
    x2 <- seq(2, 2 * col.num, by = 2)
    y1 <- min(row.num, 30) - 1
    y2 <- min(row.num, 30)
    cols <- c("gray", "red")
    for (i in 0:(min(row.num, 30) - 1)) {
        index <- as.numeric(x.fit[i + 1, ]) + 1
        rect(x1, y1 - i, x2, y2 - i, col = cols[index], border = "Alice Blue")
    }
    plot(2 * col.num, 1, type = "n", xlim = c(0, 2 * col.num), 
        ylim = c(0, 1.3), axes = FALSE, xlab = "", ylab = "")
    xmed <- seq(1, 2 * col.num, by = 2)
    text(col.num^(1/10), 1.2, "\% Present", cex = 2)
    rect(xmed - 0.3, 0, xmed + 0.3, 1, col = "gray")
    rect(xmed - 0.3, 0, xmed + 0.3, p2, col = "red")
    if (col.num <= 10) 
        axis(1, xmed, colnames(x.fit), tick = FALSE)
    else text(xmed, -0.025, srt = 65, adj = 1, labels = colnames(x.fit), 
        xpd = NA)
    cons <- min(row.num, 30)
    p3 <- p3[1:cons]
    bb <- p3[length(p3):1]
    bb[which(bb == max(bb))] <- max(bb[-which(bb == max(bb))]) * 
        1.1
    plot(max(bb), cons, type = "n", xlim = c(0, max(bb)), ylim = c(0, 
        cons), xlab = "", ylab = "", axes = FALSE)
    ymed <- seq(0.5, cons - 0.5, by = 1)
    border <- rep(NA, length(bb))
    border[length(bb)] <- "red"
    rect(0, ymed - 0.2, bb, ymed + 0.2, col = "blue", border = border)
    mtext("Pattern", line = 1)
    mtext("Frequency", line = -0.5)
    plot.new()
    if (inherits(obj, "data.frame")) 
        text(0.5, 0.5, "Missing Value Plot", cex = 1.5, font = 2)
    if (is.null(Subtitle) && inherits(obj, "mro")) 
        text(0.5, 0.5, "Combination Plot", cex = 1.5, font = 2)
    if (!is.null(Subtitle) && inherits(obj, "mro")) 
        text(0.5, 0.3, paste("Combination Plot", Subtitle, sep = "---"), 
            cex = 1.5, font = 1)
    plot.new()
    legend.name <- c("Missing", "Present", "NotSelected", "Selected")
    legend.index <- ifelse(class(obj)[1] == "mro", 3, 1)
    legend(0, 0.5, legend.name[c(legend.index, legend.index + 
        1)], fill = c("gray", "red"), col = c("red", "gray"), 
        xpd = NA)
    par(opa)
    dev.flush()
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
