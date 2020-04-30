context("VMRO")

cas5k <- census.at.school.5000[1:1000, 1:20]
test_that("Row names are suppressed", {
    o <- capture.output(calcmissing(cas5k))
    expect_length(grep("^[0-9]", o), 0)
})
