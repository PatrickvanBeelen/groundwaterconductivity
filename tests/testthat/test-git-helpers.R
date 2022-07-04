context("git helpers")


oldwd <- getwd()
setwd(testRepodir)

test_that("isValidRepo", {
              # data directory is not a repository
              expect_false(isValidRepo(testDatadir))

              #repodir is valid repository
              expect_true(isValidRepo(testRepodir))

              # git repository without remote is not valid
              testRepodir2 <- file.path(tempfile())
              dir.create(testRepodir2)
              if(!dir.exists(testRepodir2)) {
                  stop("creating of testRepodir2 failed")
              }
              cat("testRepodir2:",testRepodir2,"\n")

              repo <- git2r::init(testRepodir2)
              git2r::config(repo,user.name="devel",user.email="devel@local")
              expect_false(isValidRepo(testRepodir2))


              # repo is valid if remote is added
              git2r::remote_add(repo, "ricmdtest", "https://example.org/testrepo/")
              expect_true(isValidRepo(testRepodir2))

              # remove repo dir
              unlink(testRepodir2,recursive=TRUE)


})

test_that("isDataIgnored", {

})


test_that("isGitIgnored", {

})

test_that("getGitMeta", {
              g <- getGitMeta()
              expect_equal(g$branch,"master")
              expect_equal(g$remote,"https://example.org/testrepo/")

})

setwd(oldwd)
