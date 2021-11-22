## A new way of handling multiple response
x <- list("A", c("A", "B"), c("B", "D", "E"), "C", c("A", "E"))
all_values <- LETTERS[1:5]
x_mr <- sapply(x, function(y) {
    paste(as.integer(all_values %in% y), collapse = "")
})
attr(x_mr, "levels") <- all_values

test_that("Columns of a dataframe can be coerced to an mfactor", {
    tech_df <- census.at.school.5000[grep("^tech", names(census.at.school.5000))]
    tech_v <- mfactor(tech_df, gsub("^tech", "", names(tech_df)), indicator = "yes")

    cas5k <- data.frame(census.at.school.5000, tech = tech_v)

    plot(~tech, data = cas5k)
})
