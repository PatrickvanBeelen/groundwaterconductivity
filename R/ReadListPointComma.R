#' Reads a list from a ; separated data file
#' made by WriteListPointComma or Microsoft Excel
#' @param output The output dataframe from the file "data/Table.csv"
#' @importFrom utils read.csv
#' @export
#' @examples
#' Table <- ReadListPointComma(filename = "data/Table.csv")
ReadListPointComma <- function(filename = "WriteListPointComma.csv") {
  output <- try(read.csv(
    file = filename,
    header = TRUE, sep = ";", quote = "\"",
    na.strings = c("NA", "NaN"),
    stringsAsFactors = FALSE, strip.white = TRUE,
    check.names = TRUE
  ))
  if (class(output) == "try-error") {
    cat("\n", filename, "not found in ", getwd(), "\n\n")
  }
  return(output)
}
