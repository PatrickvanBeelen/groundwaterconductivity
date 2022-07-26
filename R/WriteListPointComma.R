#' Writes a list or a dataframe to a ; separated data file
#' easy to read by WriteListPointComma or Microsoft Excel
#' WriteListPointComma(Table,filename="data/myTable.csv")
#' @param Table The input dataframe or list Table
#' @param filename The name of the data/Table.csv
#' @importFrom  utils write.table
WriteListPointComma <- function(Table, filename = "data/WriteListPointComma.csv") {
  Table <- as.data.frame(Table)
  write.table(Table, filename,
    sep = ";",
    fileEncoding = "UTF-8",
    quote = TRUE,
    qmethod = "double",
    row.names = FALSE,
    dec = ".",
    eol = "\n"
  )
}
