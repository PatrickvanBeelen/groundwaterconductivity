McNeal <- function(z = dataframeuitMaakKolomMeth) {
  #' McNeal routine voor ec25 berekening
  b <- z[z$meth == "mcneal", ]
  b$caT <- b$ca / 2000
  b$mgT <- b$mg / 2000
  b$so4T <- b$so4 / 2000
  b$alfa <- (b$gam2^2 * 204.174)^-1
  b$beta <- (b$gam2^2 * 229.087)^-1
  b$caso4 <- 500 * (b$caT + b$so4T + b$alfa) - 500 * sqrt((b$caT + b$so4T + b$alfa)^2 - 4 * b$caT * b$so4T)
  b$so4L <- b$so4T - b$caso4 / 1000
  b$mgso4 <- 500 * (b$mgT + b$so4L + b$beta) - 500 * sqrt((b$mgT + b$so4L + b$beta)^2 - 4 * b$mgT * b$so4L)
  b$caf <- b$ca - 2 * b$caso4
  b$mgf <- b$mg - 2 * b$mgso4
  b$so4f <- b$so4 - 2 * b$caso4 - 2 * b$mgso4
  # methode 1 algemeen
  b$kmcneal <- 885 * (0.0660 * (b$cl + b$k) + 0.0414 * b$caf + 0.0356 * b$mgf + 0.0452 * b$na + 0.0507 * b$so4f + 0.0470 * b$co3 + 0.0348 * b$hco3 + 0.0603 * b$no3 + 0.0629 * (b$caso4 + b$mgso4) + (1 / b$san) * (0.03 * b$cl + 0.029 * b$hco3 + 0.077 * b$so4f + 0.034 * b$no3 + 0.07 * b$co3) + (1 / b$skat) * (0.055 * b$caf + 0.06 * b$mgf + 0.023 * b$na + 0.03 * b$k + 0.183 * (b$caso4 + b$mgso4)))
  b$KMCNEAL <- 0.964 * b$kmcneal + 8
  #  methode 1 speciaal geval
  b[b$rso4 >= 0.33 & !is.na(b$rso4), "KMCNEAL"] <- 1.181 * b[b$rso4 >= 0.33 & !is.na(b$rso4), "kmcneal"] - 275
  b[b$rso4 >= 0.33 & b$kmcneal < 1100 & !is.na(b$kmcneal) & !is.na(b$rso4), "KMCNEAL"] <- 1.052 * b[b$rso4 >= 0.33 & b$kmcneal < 1100 & !is.na(b$kmcneal) & !is.na(b$rso4), "kmcneal"] - 45
  # methode 2 overschrijft methode 1
  b$kmcneal <- 885 * (0.0620 * (b$cl + b$k) + 0.0355 * b$caf + 0.0269 * b$mgf + 0.0402 * b$na + 0.0407 * b$so4f + 0.0382 * b$co3 + 0.0291 * b$hco3 + 0.0528 * b$no3 + 0.0492 * (b$caso4 + b$mgso4) + (1 / b$san) * (0.23 * b$cl + 0.320 * b$hco3 + 0.590 * b$so4f + 0.400 * b$no3 + 0.51 * b$co3) + (1 / b$skat) * (0.260 * b$caf + 0.44 * b$mgf + 0.270 * b$na + 0.23 * b$k + 0.870 * (b$caso4 + b$mgso4)))
  b[b$sgem > 50 & !is.na(b$sgem), "KMCNEAL"] <- 0.953 * b[b$sgem > 50 & !is.na(b$sgem), "kmcneal"] + 58
  z[z$meth == "mcneal", "rk20"] <- b$KMCNEAL
  rk20uitMcNealIndataframeuitMaakKolomMeth <- z
  return(rk20uitMcNealIndataframeuitMaakKolomMeth)
}
