## standard error class
## has two components
## ses : standard error, simply a numeirc vector
## ses.diffs : standard error difference matrix
##
## can add any other funcitons for any other kind of situation

## constructor
ses.moecalc = function(ses, ses.diffs){
  x = list(ses=ses, ses.diffs=ses.diffs)
  class(x) = "ses.moecalc"
  x
}


## find ses.diffs of model
## base : has baseline or not
seModel = function(model, idx, base = TRUE){
  covs = vcov(model)[idx, idx, drop = FALSE]
  vars = diag(covs)
  ses = sqrt(vars)
  ses.diffs = outer(vars, vars, '+') - 2 * covs
  diag(ses.diffs) = 0
  ses.diffs = sqrt(ses.diffs)
  if (base) {
    ses.diffs = rbind(c(0, ses), cbind(ses, ses.diffs))
    ses = c(0, ses)
  }
  ses.moecalc(ses, ses.diffs)
}


##' Compute teh standard error information for a given covariance matrix.
##' @title Compute standard error for covariance matrix
##' @param covs covariance matrix
##' @param addbase logical, is there a baseline?
##' @return an ses.moecalc object
##' @author Junjie Zeng
##' @export
seCovs = function(covs, addbase=FALSE){
  # for use when covariance matrix is given (e.g. cal from bootstrap)
  vars = diag(covs)
  ses = sqrt(vars)
  ses.diffs = outer(vars, vars, '+') - 2 * covs
  diag(ses.diffs) = 0
  ses.diffs = sqrt(ses.diffs)
  if (addbase) { #if we have a missing baseline catecory, put it in at front
    ses.diffs = rbind(c(0, ses), cbind(ses, ses.diffs))
    ses = c(0, ses)
  }
  ses.moecalc(ses, ses.diffs)
}


##' SEs for Multinomial Proportions
##' @title Compuate SE for Multinomial proportions
##' @param n the number of observations in each group
##' @param phat the estimates proportions for each group
##' @return an \code{ses.moecalc} object
##' @author Junjie Zeng
##' @export
seMNprops = function(n, phat){
  if (length(n)!=1) stop("Requires length(n)=1")
  if (abs(sum(phat)-1)>.001) stop("proportions must sum to 1")
  ses.diffs = sqrt((outer(phat,phat,'+') - (outer(phat,phat,'-'))^2)/n)
  diag(ses.diffs)=0
  ses = sqrt(phat*(1-phat)/n)
  
  ses.moecalc(ses, ses.diffs)
}

##' Compute SEs for Independent Binomial Proportions
##' @title Independent Binomial Proportions
##' @param ns the number of observations in the independent groups
##' @param phats the proportions of TRUE/1's etc.
##' @return an \code{ses.moecalc} object
##' @author Junjie Zeng
##' @export
seBinprops = function(ns, phats){
  if ((any(phats>1)) | (any(phats<0)))
    stop("proportions must lie between 0 and 1")
  if (length(ns)!=length(phats))
    stop("sample sizes and proportions must have same length")
  temp = phats*(1-phats)/ns
  ses = sqrt(temp)
  ses.diffs = sqrt(outer(temp,temp,'+'))
  diag(ses.diffs)=0
  
  ses.moecalc(ses, ses.diffs)
}

##' Returns ses.moecalc for given SEs
##' @title Independent Standard errors given
##' @param ses the standard errors
##' @return an \code{ses.moecalc} object
##' @author Junjie Zeng
##' @export
seIndepSes = function(ses){
  temp = ses^2
  ses.diffs = sqrt(outer(temp,temp,'+'))
  ses.moecalc(ses, ses.diffs)
}

##' SE's for multiple binary response
##' @title Multiple binary response
##' @param obj something that can be turned into a matrix
##' @return an \code{ses.moecalc} object
##' @author Junjie Zeng
##' @export
seMRprops <- function(obj) {
  obj <- as.matrix(obj)
  n <- nrow(obj)
  P <- colMeans(obj)
  Q <- colMeans(1 - obj)
  ses <- sqrt(P * Q / n)
  p12 <- t(obj) %*% (1 - obj) / n
  p21 <- t(1 - obj) %*% obj / n
  ses.diffs <- sqrt(((p12 + p21) -(p12 - p21)^2) / n)
  ses.moecalc(ses = ses, ses.diffs = ses.diffs)
}
