require(survey)

cas <- read.csv('census.at.school.survey.csv')
cas$region <- factor(cas$region, levels = unique(cas$region))

tm <- as.matrix(cas[-(1:7)])
mode(tm) <- "factor"
tm <- tm - 1
cas[-(1:7)] <- as.data.frame(tm)
cas.svy <- svydesign(~1, strata = ~region, fpc = ~fpc, data = cas)

test_that("Survey designs can be handled", {
    techMR <- iNZightMR(
        tech ~ own_cell_phone + facebook + instagram + twitter + snapchat,
        data = cas.svy
    )
    expect_is(techMR, "mro")
    expect_warning(barplotMR(mroPara(techMR)), "Max error betw.")
    expect_warning(barplotMR(byMRO(techMR, ~region, mroPara)), "Max error betw.")
    expect_warning(barplotMR(between(byMRO(techMR, ~region, mroPara))), "Max error betw.")
})

test_that("Survey GLMs", {
    fit <- svyglm(height ~ age + gender, design = cas.svy)
    expect_warning(mc <- moecalc(fit, "gender"), "confidence interval of baseline")
    expect_is(mc, "moecalc")
    expect_warning(plot(mc), "Max error betw. approx.")
})
