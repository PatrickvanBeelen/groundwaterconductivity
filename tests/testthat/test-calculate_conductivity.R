test_that("calculate_conductivity", {
  inputfilename=paste0(getwd(),"/data/Table.csv")
  inputfilename
  # setwd("~")
  Stuyfzandtest <- calculate_conductivity(inputfilename = "data/Table.csv", inputstyle = "Stuyfzand", outputstyle = "minimal", celcius = 25)
  good_ec25 <- c(
    7.62, 166.53, 51.38, 142.6, 57.17, 182.97, 65.26, 140.65, 82.02,
    115.11, 190.82, 36.61, 737.73, 293.23, 826.06, 296.6, 551.47,
    272.57, 663.96, 1110, 1670.92, 938.41, 6.15, 1308.44, 1256.22,
    4073, 7625.13, 24358.73, 23.96, 8.26, 38.78, 10.41, 33.16, 56.51
  )

  expect_equal(Stuyfzandtest$ec25, good_ec25)
})
