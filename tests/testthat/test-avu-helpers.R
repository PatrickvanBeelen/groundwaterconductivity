context("avu-helpers")

oldwd <- getwd()
setwd(testRepodir)
# 
# test_that("setGitAvu", {
#               
#               setGitAvu()
#               x <- get("gitavu",env=.ricmdWorkflow)
#               expect_equal(x$avu[[x$key$branch]]$value,"master")
#                            
# 
# })
# 

test_that("setProjectAvu", {
              setProjectAvu("thisscript","thisworkdir","thisfile")
              x <- get("projectavu",env=.ricmdWorkflow)
              expect_equal(x$avu[[x$key$script]]$value,"thisscript")


})

test_that("setWorkflowAvu", {
    
              thisfile <- file.path(testRepodir,testScript)
              setProjectAvu(testScript,testRepodir,thisfile)
              setWorkflowAvu()
              x <- get("workflowavu",env=.ricmdWorkflow)
              expect_equal(x$avu[[x$key$script]]$value,testScript)
              expect_equal(x$avu[[x$key$thisfile]]$value,thisfile)


})

setwd(oldwd)


