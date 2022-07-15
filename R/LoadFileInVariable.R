#' Loads an .rda data file into a variable
#' myTable<-LoadFileInVariable("data/Table.rda")
#' @param filename Input datafile like data/Table.rda
#' @export
LoadFileInVariable <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}
