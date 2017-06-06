##' Computes the MRO information for a para object ...
##'
##' @title Compute Para MRO for an object
##' @param obj An object to compute things for 
##' @param conf.levels Confidence level to use
##' @param nonparallel Should these things be parallel?
##' @return An object of class \code{mrocalc}
##' @author Junjie Zeng
##' @export
mroPara <- function(obj, conf.levels = 1.96, nonparallel=NULL) {
  cl <- match.call()
  Tb <- names(obj)[1]
  
  ## nonparallel is playing the role as changing the topic names
  if (! is.null(nonparallel))
     Tb <- nonparallel
  
  
  if (inherits(obj, "mro"))
    obj <- obj[[1]]
  
  n <- nrow(obj)
  estP <- colMeans(obj)
  SesDiff <- seMRprops(obj)
  
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


