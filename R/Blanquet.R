Blanquet <- function(z = dataframeuitMaakKolomMeth) {
  #' Blanquet routine  voor ec25 berekening
  b <- z[z$meth == "blanquet", ]
  b$sqgem <- sqrt(b$sgem)
  b$rlngem <- log(b$sgem)
  b$kblan <- 1.046 * (107.73 * b$cl + 77.55 * b$hco3 + 109.02 * b$so4 + 20.97 * b$k - b$sqgem * (1.452 * b$cl + 1.228 * b$hco3 + 2.844 * b$so4 + 0.112 * b$k) + ((6.1 - 0.9 * b$sqgem) * b$cl + (6 - 2.067 * b$sqgem) * b$hco3 + (-3.1 - 7.274 * b$rlngem) * b$so4) * b$ca / b$sgem + ((-0.23 - 1.746 * b$rlngem) * b$cl + (6.43 - 4.047 * b$rlngem) * b$hco3 + (-7.8 - 4.831 * b$rlngem) * b$so4) * b$mg / b$sgem)
  b$rk20 <- b$kblan
  b[b$rhco3 >= 0.15 & !is.na(b$rhco3), "rk20"] <- 0.911 * b[b$rhco3 >= 0.15 & !is.na(b$rhco3), "kblan"] + 196
  b[b$rhco3 >= 0.5 & !is.na(b$rhco3), "rk20"] <- 0.98 * b[b$rhco3 >= 0.5 & !is.na(b$rhco3), "kblan"] + 33
  b[b$rso4 >= 0.33 & !is.na(b$rso4), "rk20"] <- 0.8 * b[b$rso4 >= 0.33 & !is.na(b$rso4), "kblan"] + 309
  b[b$rso4 >= 0.33 & b$sgem >= 100 & !is.na(b$rso4) & !is.na(b$sgem), "rk20"] <- 1.02 * b[b$rso4 >= 0.33 & b$sgem >= 100 & !is.na(b$rso4) & !is.na(b$sgem), "kblan"] - 528
  z[z$meth == "blanquet", "rk20"] <- b$rk20
  rk20uitBlanquetIndataframeuitMaakKolomMeth <- z
  return(rk20uitBlanquetIndataframeuitMaakKolomMeth)
}
