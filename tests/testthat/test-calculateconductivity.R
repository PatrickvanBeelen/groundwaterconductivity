test_that("calculate_conductivity", {
  Stuyfzandtest <- calculate_conductivity(inputfilename = "Table.csv", inputstyle = "Stuyfzand", outputstyle = "Stuyfzandstyle", celcius = 25)
  
  expect_equal(max(Stuyfzandtest$percentage_xecv_ec25), 1.35)
})