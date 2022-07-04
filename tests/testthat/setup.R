cat("running setup ...")

env <- path.expand('~/.irods/irods_environment.json')
irods <- import("irods.session")
session <- irods$iRODSSession(irods_env_file=env)
testColl <- file.path("",session$zone,'home',session$username,"tests")
session$collections$create(testColl)
rm(session)

# create test data dir

testDatadir <- file.path(tempdir(),'data')
if(!dir.exists(testDatadir)) {
    dir.create(testDatadir)
}

# create test git repository with one script
testRepodir <- file.path(tempdir(),'repo')
if(!dir.exists(testRepodir)) {
    dir.create(testRepodir)
}

testScript <- "testscript.R"
cat("test script",testScript,"\n")

f <- file(file.path(testRepodir,testScript), open="w")
cat("print('hello world')",file=f)
close(f)

repo <- git2r::init(testRepodir)
git2r::config(repo,user.name="devel",user.email="devel@local")
git2r::add(repo,path=file.path(testRepodir,testScript))
git2r::commit(repo,message="test script")
git2r::remote_add(repo, "ricmdtest", "https://example.org/testrepo/")

# # create Test list
# 
# default.lst <- list(avu=list(list(attribute="key1",value="val1",units="unit1"),
#                    list(attribute="key2",value="val2",units=NA)),
#           key=list(key1=1,key2=2))
# 

cat("Done!\n")

