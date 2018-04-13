context("test_loglikelihood")

dset <- 1:3
# Not really a fdp, just an ++ function.
inc <- function(x, par) return(x+1)
incll <- BuildLogLikelihood(inc, dset)

test_that("BuildLogLikelihood returns a closure", {
  expect_type(BuildLogLikelihood(dunif, dset), "closure")
  expect_type(BuildLogLikelihood(dpois, dset), "closure")
  expect_type(incll, "closure")
})

test_that("LogLikelihood returns the correct value", {
  expect_equal(incll(1:3), sum(log(inc(1:3))))
  # Same as above
  expect_equal(incll(1:3), 3.178054, tolerance=10^(-6))
})