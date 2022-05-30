chisq.mro <- function(mropa) {
    VCm <- mropa$Cov
    x <- mropa$Mromoecalc$est
    mu <- mean(mropa$Mromoecalc$est)
    vm <- try(solve(VCm), silent = TRUE)
    if (inherits(vm, "try-error"))
      return(NA)
    t(x - mu) %*% vm %*% (x - mu)
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
    list(
        xvalue = sum(chiv),
        df = (p * (n - 1))[1],
        pv = pchisq(sum(chiv), df = p * (n - 1), lower.tail = FALSE)[1]
    )
}
