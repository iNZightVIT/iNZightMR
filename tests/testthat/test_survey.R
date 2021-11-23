# require(survey)
# data(api)

# d <- apiclus2
# d$X1 <- sample(0:1, nrow(d), TRUE)
# d$X2 <- sample(0:1, nrow(d), TRUE)
# d$X3 <- sample(0:1, nrow(d), TRUE)
# d$X4 <- sample(0:1, nrow(d), TRUE)
# dclus2 <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = d)

# devtools::load_all()

# obj <- iNZightMR(X ~ X1+X2+X3+X4, dclus2)

# barplotMR(mroPara(iNZightMR(X~X1+X2+X3+X4, d)))
