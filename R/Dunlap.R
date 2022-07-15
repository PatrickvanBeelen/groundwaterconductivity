Dunlap <- function(z = dataframeuitMaakKolomMeth) {
  #' Dunlap routine  voor ec25 berekening
  b <- z[z$meth == "dunlap", ]
  b$A <- 35.35 * b$cl + 16.48 * b$hco3 + 24.02 * b$so4 + 75.63 * b$co3 + (b$na + b$k) * 22.99 + 19.04 * b$ca + 24.3 * b$mg
  b$B <- 4.3 * 10^-4 * (log(b$A))^7.888
  b$F <- 0.948 + 1.503 * 10^-6 * b$B
  b[b$B < 10 - 4, "F"] <- 1.101 - 3.252 * 10^-5 * b[b$B < 10 - 4, "B"]
  b$kdun <- b$F * b$B
  b$KDUN <- 7.456 * b$kdun^0.8198
  z[z$meth == "dunlap", "rk20"] <- b$KDUN
  rk20uitDunlapIndataframeuitMaakKolomMeth <- z
  return(rk20uitDunlapIndataframeuitMaakKolomMeth)
}
