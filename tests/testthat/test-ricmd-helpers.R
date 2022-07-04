context("ricmd helpers")



test_that("isPkgLoaded", {
              unloadNamespace("git2r")
              expect_false(isPkgLoaded("git2r"))
              library(git2r)
              expect_true(isPkgLoaded("git2r"))

})
