% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bymro.R
\name{byMRO}
\alias{byMRO}
\title{Calculate MRO inference for subsets}
\usage{
byMRO(mro.obj, formula, FUN, ...)
}
\arguments{
\item{mro.obj}{an mro object (created by \code{iNZightMR})}

\item{formula}{variable for subsetting, as a formula (e.g., \code{~x})}

\item{FUN}{the function to apply to subsets}

\item{...}{additional arguments passed to \code{FUN}}
}
\value{
an object with classes of \code{by} and \code{bymrocalc}
}
\description{
Constructs a multiple response object (MRO) subset by another explanatory variable.
}
\examples{
mr <- iNZightMR(online ~ onlinegame + onlinevideo + onlinemusic,
    data = census.at.school.5000
)

byMRO(mr, ~gender, mroPara)
byMRO(mr, ~ gender + handed, mroPara)
}
\seealso{
\link{mroPara}
}
