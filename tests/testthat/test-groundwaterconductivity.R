test_that("my first try", {
  Stuyfzandtest <- calculate_conductivity(inputfilename = "CopyOfStuyfzandTable31.csv", inputstyle = "Stuyfzand", outputstyle = "Stuyfzandstyle", celcius = 25)
  
  expect_equal(max(Stuyfzandtest$percentage_xecv_ec25), 1.35)
})
