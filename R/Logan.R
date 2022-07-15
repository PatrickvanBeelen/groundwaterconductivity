Logan <- function(z = dataframeuitMaakKolomMeth) {
  #' logan routine  voor ec25 berekening
  b <- z[z$meth == "logan", ]
  b$klogan <- (222.28 * b$sgem)^0.9058
  b$rk20 <- b$klogan - 30
  b[b$sgem > 100 & !is.na(b$sgem), "rk20"] <- 1.002 * b[b$sgem > 100 & !is.na(b$sgem), "klogan"] - 83
  z[z$meth == "logan", "rk20"] <- b$rk20
  rk20uitLoganIndataframeuitMaakKolomMeth <- z
  return(rk20uitLoganIndataframeuitMaakKolomMeth)
}
