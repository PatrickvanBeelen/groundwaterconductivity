context("ri_workflowInit")

oldwd <- getwd()
setwd(testRepodir)

test_that("package loaded", {
              expect_error(ri_workflowInit(testScript))
              library(ricmd)
              library(git2r)
              library(datafile)
              ricmd::ri_session()
              ri_workflowInit(testScript)
              ricmd:::destroySession() 
              unloadNamespace("git2r")
              unloadNamespace("ricmd")
              unloadNamespace("datafile")

})


test_that("valid arguments", {
              expect_error(ri_workflowInit(1))
              expect_error(ri_workflowInit(testScript,1))
              expect_error(ri_workflowInit("nonexistingscript"))
})

test_that("session test", {
              library(ricmd)
              library(git2r)
              library(datafile)
              expect_error(ri_workflowInit(testScript))
              ricmd::ri_session()
              ri_workflowInit(testScript)
              ricmd:::destroySession() 
              unloadNamespace("git2r")
              unloadNamespace("ricmd")
              unloadNamespace("datafile")

})


test_that("correct functioning",{

              library(ricmd)
              library(git2r)
              library(datafile)
              ricmd::ri_session()

              ri_workflowInit(testScript)
              x <- get("workflowavu",env=.ricmdWorkflow)
              expect_equal(length(x),2)


              ricmd:::destroySession() 
              unloadNamespace("git2r")
              unloadNamespace("ricmd")
              unloadNamespace("datafile")

})

setwd(oldwd)
