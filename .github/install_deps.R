# R script
github_deps <- c(
    "gdemin/expss",
    "iNZightVIT/iNZightPlots@dev",
    "iNZightVIT/iNZightTools@1.9",
    "iNZightVIT/iNZightMR@2.2.5"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM <- Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS == "Linux" &&
    !requireNamespace("units", quietly = TRUE) &&
    getRversion() >= "4.3") {
    system("sudo apt-get install libudunits2-dev")
}

if (OS != "Linux" && !requireNamespace("XML", quietly = TRUE)) {
    install.packages("XML", type = "binary")
}

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
