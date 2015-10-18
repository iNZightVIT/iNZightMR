.onLoad <- function(libname = find.package("iNZightMR"), pkgname = "iNZightMR") {
    if (!"iNZightPlots" %in% row.names(installed.packages())) {
        message("iNZightMR cannot be run without iNZightPlots... installing for you now.")
        install.packages("iNZightPlots", repos = "http://docker.stat.auckland.ac.nz/R")
    }
}
