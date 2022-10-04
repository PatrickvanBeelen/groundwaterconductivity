globalVariables(c("LMM_broad_input_groundwaterconductivity","input_groundwaterconductivity","celcius","add_bicarbonate","add_phosphate","dataframeuitMaakKolomMeth",
                  "inputfilename","inputstyle","outputstyle","celcius","rk20uitBlanquetIndataframeuitMaakKolomMeth",
                  "detectieteken","parameter","waarde"))

#' loads an RData file use variablename<-LoadFileInVariable(filename)
#' @param fileName the name of the input.rda file
LoadFileInVariable <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"])
}


#' Prepare for the conductivity calculations by selecting the proper method
#' @param LMM_broad_input_groundwaterconductivity your input file was converted to the standard inputfile used here
#' @param celcius temperature numerical
#' @param add_bicarbonate is TRUE when bicarbonate can be estimated from the ion balance
#' @param add_phosphate ist TRUE when total phosphorus is used to estimate phosphate
#'
MaakKolomMeth <- function(LMM_broad_input_groundwaterconductivity = LMM_broad_input_groundwaterconductivity, celcius = celcius, add_bicarbonate = add_bicarbonate, add_phosphate = add_phosphate) {
  #   matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3')
  # LMM_broad_input_groundwaterconductivity <-celcius <-add_bicarbonate<-add_phosphate<-NULL
  zm <- as.data.frame(LMM_broad_input_groundwaterconductivity)
  if (!"xhv" %in% colnames(zm)) {
    zm$xhv <- NA
  }

  if (!"xhco3" %in% colnames(zm)) {
    zm$xhco3 <- NA
  }
  if (!"xco3" %in% colnames(zm)) {
    zm$xco3 <- NA
  }
  if (!"xhco3v" %in% colnames(zm)) {
    zm$xhco3v <- NA
  }

  if (!"xpo4" %in% colnames(zm)) {
    zm$xpo4 <- NA
  }
  # in xhco3 stoppen we het gemiddelde van xhco3 en xhco3v
  # met de NAs niet meegenomen dus meer kans op een meetwaarde
  # alle NAs op nul zetten behalve pH en xhco3v

  zm[is.na(zm$xhco3) & !is.na(zm$xhco3v), "xhco3e"] <- zm[is.na(zm$xhco3) & !is.na(zm$xhco3v), "xhco3v"]
  zm[!is.na(zm$xhco3) & is.na(zm$xhco3v), "xhco3e"] <- zm[!is.na(zm$xhco3) & is.na(zm$xhco3v), "xhco3"]
  zm[is.na(zm$xhco3) & is.na(zm$xhco3v), "xhco3e"] <- 0
  myrows <- !is.na(zm$xhco3) & !is.na(zm$xhco3v)
  zm[myrows, "xhco3e"] <- (zm[myrows, "xhco3"] + zm[myrows, "xhco3"]) / 2
  zm[is.na(zm$xpo4), "xpo4"] <- 0
  zm[is.na(zm$xcl), "xcl"] <- 0
  zm[is.na(zm$xso4), "xso4"] <- 0
  zm[is.na(zm$xno3), "xno3"] <- 0
  zm[is.na(zm$xco3), "xco3"] <- 0
  zm[is.na(zm$xna), "xna"] <- 0
  zm[is.na(zm$xk), "xk"] <- 0
  zm[is.na(zm$xca), "xca"] <- 0
  zm[is.na(zm$xmg), "xmg"] <- 0
  zm[is.na(zm$xnh4), "xnh4"] <- 0
  zm[is.na(zm$xfe), "xfe"] <- 0
  zm[is.na(zm$xmn), "xmn"] <- 0
  zm[is.na(zm$xal), "xal"] <- 0
  zm[is.na(zm$xzn), "xzn"] <- 0
  #  wanneer je niks weet is de pH 7
  zm[is.na(zm$xhv), "xhv"] <- 7
  # een invuldataframe maken voor de berekeningen
  z <- data.frame(matrix(ncol = 1, nrow = length(zm[, 1])))
  row.names(z) <- row.names(zm)
  names(z) <- "myrownames"
  z$myrownames <- zm$myrownames
  # de xhco3e is het beste gemiddelde van de hco3 metingen en heeft NA=0
  if ("xhco3e" %in% names(zm)) {
    z$hco3 <- zm$xhco3e / 61
  }
  z$cl <- zm$xcl / 35.453
  z$so4 <- 2 * zm$xso4 / 96.062
  # bij Herman Prins was de invoer in mg N-nitraat in de platte matrix in mg nitraat
  z$no3 <- zm$xno3 / 62
  z$co3 <- 2 * zm$xco3 / 60.02
  #  pH omrekenen naar mili-equivalent dus maal 1000
  z$h3o <- 1000 * 10^-zm$xhv
  z$na <- zm$xna / 22.9898
  z$k <- zm$xk / 39.102
  z$ca <- 2 * zm$xca / 40.08
  z$mg <- 2 * zm$xmg / 24.31
  # nh4 hier ook als mg nh4 en niet als mg N
  z$nh4 <- zm$xnh4 / 18
  z$fe <- 2 * zm$xfe / 55.85
  z$mn <- 2 * zm$xmn / 54.94
  # al is in microgram
  z$al <- 0.003 * zm$xal / 26.98
  z$zn <- 0.002 * zm$xzn / 65.39

  z$po4 <- 3 * zm$xpo4 / 30.97
  # z bevat geen NAs in plaats daarvan nullen

  # nu staan er nog nullen in z$po4
  if (add_phosphate) {
    # als z$po4=0 dan gebruiken we zm$xptot
    z[z$po4 == 0, "po4"] <- 3 * zm[zm$xpo4 == 0, "xptot"] / 30.97
  }

  # alles omgezet van zm naar z behalve xecv

  #  ionbalans
  z$pos <- z$al + z$ca + 0.6 * z$fe + z$k + z$mg + z$mn + z$nh4 + z$na + z$zn + z$h3o
  z$neg <- z$cl + z$hco3 + z$no3 + z$so4 + z$co3 + z$po4
  # wanneer overal nullen staan dan wordt de pH 7 en z$h3o = 0.0001
  z$ib <- 100 * (z$pos - z$neg) / (z$pos + z$neg)
  # de gemiddelde temperatuur tijdens veldmetingen is 12 graden celcius
  #  maar xecv wordt omgerekend naar 25 graden celcius
  TK <- 273.15 + celcius
  z$oh <- (1000 * 10^(6.0875 - 0.01706 * TK - 4470.99 / TK)) / z$h3o

  # The bicarbonate anion is in equilibrium with CO2 gas from the air. It therefore can escape from sampling through the air.
  # At the LMM monitoring network we have chosen not to sample bicarbonate.
  # In that case the bicarbonate concentration can only be roughly estimated from the ion balance when one assumes that there are equal charge equivalents from cations and anions.
  # In that case the amount of milliequivalents of bicarbonate is equal to the sum of the cations minus the sum of the anions.
  # This can only be performed when there is an excess of cations.
  if (add_bicarbonate) {
    z$san <- z$cl + z$hco3 + z$so4 + z$no3 + z$co3 + z$oh
    z$skat <- z$h3o + z$na + z$k + z$ca + z$mg + z$nh4 + z$fe + z$mn
    myrows <- row.names(z[z$skat > z$san & z$hco3 == 0 & !is.na(z$skat) & !is.na(z$san) & !is.na(z$hco3), ])
    z[myrows, "hco3"] <- z[myrows, "skat"] - z[myrows, "san"]
  }
  z$mu <- 0.0005 * (z$cl + z$hco3 + z$no3 + z$oh + z$h3o + z$na + z$k + z$nh4 + 2 * z$so4 + 2 * z$co3 + 2 * z$ca + 2 * z$mg + 2 * z$fe + 2 * z$mn + 2.55 * z$al)
  z$sqmu <- sqrt(z$mu)
  z$gam2 <- 10^(-2 * (z$sqmu / (z$sqmu + 1) - 0.3 * z$mu))
  z$san <- z$cl + z$hco3 + z$so4 + z$no3 + z$co3 + z$oh
  z$skat <- z$h3o + z$na + z$k + z$ca + z$mg + z$nh4 + z$fe + z$mn
  z$alM <- z$al / 3000
  z$so4M <- z$so4 / 2000
  z$DO <- ((z$gam2^-3) * (10^-3.02) + z$alM + z$so4M) / 2
  z$tmp <- z$DO^2 - z$alM * z$so4M
  z$also4 <- z$DO - sqrt(z$tmp)
  z$alF <- z$alM - z$also4
  z$aloh <- z$gam2^1.25 * z$alF * z$oh * 10^9.03
  z$aloh2 <- (z$gam2^2) * z$alF * (z$oh^2) * (10^18.7)
  z$alZ <- (3 * z$alF - z$aloh - 2 * z$aloh2 + z$also4) * 1.000
  z$san2 <- z$cl + z$hco3 + z$so4 + z$no3 + z$co3 - z$also4
  z$skat2 <- z$h3o + z$na + z$k + z$ca + z$mg + z$nh4 + z$fe + z$mn + z$alZ
  #   voor pH<5 geldt z$h3o>0.01 omdat het in mili-equivalent is
  z[na.omit(z$h3o > 0.01), "san"] <- z[na.omit(z$h3o > 0.01), "san2"]
  z[na.omit(z$h3o > 0.01), "skat"] <- z[na.omit(z$h3o > 0.01), "skat2"]
  z$sgem <- (z$san + z$skat) / 2
  z$rcl <- z$cl / z$san
  z$rhco3 <- (z$hco3 + z$co3) / z$san
  z$rso4 <- z$so4 / z$san
  z$rno3 <- z$no3 / z$san
  # keuze schattingsmethode
  # lukt alleen wanneer je sgem kan berekenen
  z$meth <- "dunlap"
  z$meth_ <- "dunlap"
  myrows <- z$sgem < 600
  # myrows is een logical met TRUE FALSE en NA
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "logan"
  myrows <- z$sgem < 600 & (z$rso4 >= 0.33 | z$rhco3 >= 0.15)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "blanquet"
  myrows <- z$sgem < 100
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "blanquet"
  myrows <- z$sgem < 100 & z$rno3 >= 0.15
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "mcneal"
  myrows <- z$sgem < 100 & z$rcl >= 0.67
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "logan"
  myrows <- z$sgem < 100 & z$rhco3 >= 0.5
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "blanquet"
  myrows <- z$sgem < 100 & (z$rso4 >= 0.33)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "mcneal"
  myrows <- z$sgem < 100 & (z$rso4 >= 0.33 & z$sgem < 50)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "blanquet"
  myrows <- z$sgem < 20
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "rossum"
  myrows <- z$sgem < 20 & (z$rso4 >= 0.33 | z$rno3 >= 0.15)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "mcneal"
  myrows <- z$sgem < 4
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth"] <- "rossum"
  myrows <- z$sgem < 600
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "logan_2"
  myrows <- z$sgem < 600 & (z$rso4 >= 0.33 | z$rhco3 >= 0.15)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "blanquet_4"
  myrows <- z$sgem < 100
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "blanquet_3"
  myrows <- z$sgem < 100 & z$rno3 >= 0.15
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "mcneal_3"
  myrows <- z$sgem < 100 & z$rcl >= 0.67
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "logan_1"
  myrows <- z$sgem < 100 & z$rhco3 >= 0.5
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "blanquet_2"
  myrows <- z$sgem < 100 & (z$rso4 >= 0.33)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "mcneal_2"
  myrows <- z$sgem < 100 & (z$rso4 >= 0.33 & z$sgem < 50)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "blanquet_1"
  myrows <- z$sgem < 20
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "rossum_2"
  myrows <- z$sgem < 20 & (z$rso4 >= 0.33 | z$rno3 >= 0.15)
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "mcneal_1"
  myrows <- z$sgem < 4
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "rossum_1"
  myrows <- z$san == 0 | z$skat == 0
  myrows[is.na(myrows)] <- FALSE
  z[myrows, "meth_"] <- "leeg"
  z[myrows, "meth"] <- "leeg"

  z$rk20 <- as.numeric(0)
  dataframeuitMaakKolomMeth <- z
  return(dataframeuitMaakKolomMeth)
}






#' Blanquet routine for ec25 calculation
#' it fills in some specific rows marked for the Blanquet method
#' @param dataframeuitMaakKolomMeth the first ec25 calculations start with a dataframe from MaakKolomMeth
#'
Blanquet <- function(dataframeuitMaakKolomMeth = dataframeuitMaakKolomMeth) {
  b <- dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "blanquet", ]
  b$sqgem <- sqrt(b$sgem)
  b$rlngem <- log(b$sgem)
  b$kblan <- 1.046 * (107.73 * b$cl + 77.55 * b$hco3 + 109.02 * b$so4 + 20.97 * b$k - b$sqgem * (1.452 * b$cl + 1.228 * b$hco3 + 2.844 * b$so4 + 0.112 * b$k) + ((6.1 - 0.9 * b$sqgem) * b$cl + (6 - 2.067 * b$sqgem) * b$hco3 + (-3.1 - 7.274 * b$rlngem) * b$so4) * b$ca / b$sgem + ((-0.23 - 1.746 * b$rlngem) * b$cl + (6.43 - 4.047 * b$rlngem) * b$hco3 + (-7.8 - 4.831 * b$rlngem) * b$so4) * b$mg / b$sgem)
  b$rk20 <- b$kblan
  b[b$rhco3 >= 0.15 & !is.na(b$rhco3), "rk20"] <- 0.911 * b[b$rhco3 >= 0.15 & !is.na(b$rhco3), "kblan"] + 196
  b[b$rhco3 >= 0.5 & !is.na(b$rhco3), "rk20"] <- 0.98 * b[b$rhco3 >= 0.5 & !is.na(b$rhco3), "kblan"] + 33
  b[b$rso4 >= 0.33 & !is.na(b$rso4), "rk20"] <- 0.8 * b[b$rso4 >= 0.33 & !is.na(b$rso4), "kblan"] + 309
  b[b$rso4 >= 0.33 & b$sgem >= 100 & !is.na(b$rso4) & !is.na(b$sgem), "rk20"] <- 1.02 * b[b$rso4 >= 0.33 & b$sgem >= 100 & !is.na(b$rso4) & !is.na(b$sgem), "kblan"] - 528
  dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "blanquet", "rk20"] <- b$rk20
  rk20uitBlanquetIndataframeuitMaakKolomMeth <- dataframeuitMaakKolomMeth
  return(rk20uitBlanquetIndataframeuitMaakKolomMeth)
}


#' Dunlap routine for ec25 calculation
#' it fills in some specific rows marked for the Dunlap method
#' @param dataframeuitMaakKolomMeth the second ec25 calculations start with a dataframe from Blanquet
#'
Dunlap <- function(dataframeuitMaakKolomMeth = dataframeuitMaakKolomMeth) {
  b <- dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "dunlap", ]
  b$A <- 35.35 * b$cl + 16.48 * b$hco3 + 24.02 * b$so4 + 75.63 * b$co3 + (b$na + b$k) * 22.99 + 19.04 * b$ca + 24.3 * b$mg
  b$B <- 4.3 * 10^-4 * (log(b$A))^7.888
  b$F <- 0.948 + 1.503 * 10^-6 * b$B
  b[b$B < 10 - 4, "F"] <- 1.101 - 3.252 * 10^-5 * b[b$B < 10 - 4, "B"]
  b$kdun <- b$F * b$B
  b$KDUN <- 7.456 * b$kdun^0.8198
  dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "dunlap", "rk20"] <- b$KDUN
  rk20uitDunlapIndataframeuitMaakKolomMeth <- dataframeuitMaakKolomMeth
  return(rk20uitDunlapIndataframeuitMaakKolomMeth)
}



#' Logan routine for ec25 calculation
#' it fills in some specific rows marked for the Logan method
#' @param dataframeuitMaakKolomMeth the third ec25 calculations start with a dataframe from Dunlap
#'
Logan <- function(dataframeuitMaakKolomMeth = dataframeuitMaakKolomMeth) {
  b <- dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "logan", ]
  b$klogan <- (222.28 * b$sgem)^0.9058
  b$rk20 <- b$klogan - 30
  b[b$sgem > 100 & !is.na(b$sgem), "rk20"] <- 1.002 * b[b$sgem > 100 & !is.na(b$sgem), "klogan"] - 83
  dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "logan", "rk20"] <- b$rk20
  rk20uitLoganIndataframeuitMaakKolomMeth <- dataframeuitMaakKolomMeth
  return(rk20uitLoganIndataframeuitMaakKolomMeth)
}


#' McNeal routine for ec25 calculation
#' it fills in some specific rows marked for the McNeal method
#' @param dataframeuitMaakKolomMeth the forth ec25 calculations start with a dataframe from Logan
#'
McNeal <- function(dataframeuitMaakKolomMeth = dataframeuitMaakKolomMeth) {
  #' McNeal routine voor ec25 berekening
  b <- dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "mcneal", ]
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
  dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "mcneal", "rk20"] <- b$KMCNEAL
  rk20uitMcNealIndataframeuitMaakKolomMeth <- dataframeuitMaakKolomMeth
  return(rk20uitMcNealIndataframeuitMaakKolomMeth)
}



#' Rossum routine for ec25 calculation
#' it fills in some specific rows marked for the Rossum method
#' @param dataframeuitMaakKolomMeth the fifth ec25 calculations start with a dataframe from McNeal
#'
Rossum <- function(dataframeuitMaakKolomMeth = dataframeuitMaakKolomMeth) {
  #' Rossum volgens Ec_voor_Patrick.docx  voor ec25 berekening
  b <- dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "rossum", ]
  # van H+ naar milli-equivalent
  b$al <- 0
  b$g0an <- 86 * b$co3 + 44.5 * b$hco3 + 79.8 * b$so4 + 76.3 * b$cl + 71.4 * b$no3
  b$g0kat <- 59.5 * b$ca + 53.1 * b$mg + 50.1 * b$na + 73.5 * b$k + 349 * b$h3o + 73.5 * b$nh4 + 54 * b$fe + 78 * b$al
  b$zan <- (4 * (b$co3 + b$so4) + b$cl + b$hco3 + b$no3) / (2 * (b$co3 + b$so4) + b$cl + b$hco3 + b$no3)
  # Aanname 1.55*al
  b$zkat <- (4 * (b$ca + b$mg + b$fe + 1.55 * b$al) + b$na + b$k + b$h3o + b$nh4) / (2 * (b$ca + b$mg + b$fe + 1.55 * b$al) + b$na + b$k + b$h3o + b$nh4)
  b$gaman <- b$g0an / b$san
  b$gamkat <- b$g0kat / b$skat
  b$q <- b$zan * b$zkat * (b$gaman + b$gamkat) / ((b$zan + b$zkat) * (b$zkat * b$gaman + b$zan * b$gamkat))
  b$kross <- 0.885 * (b$g0kat + b$g0an - ((b$gaman + b$gamkat) * b$zan * b$zkat * 2 * b$q / (115.2 * (b$zan + b$zkat) * (1 + sqrt(b$q))) + 0.668) * ((b$zan + b$zkat) * b$sgem)^1.5)
  b$KROSS <- b$kross
  b[b$rcl >= 0.67 & !is.na(b$rcl), "KROSS"] <- 1.0003 * b[b$rcl >= 0.67 & !is.na(b$rcl), "kross"] - 2
  b[b$rso4 >= 0.33 & !is.na(b$rso4), "KROSS"] <- 0.989 * b[b$rso4 >= 0.33 & !is.na(b$rso4), "kross"]
  b[b$rhco3 >= 0.67 & !is.na(b$rhco3), "KROSS"] <- 1.025 * b[b$rhco3 >= 0.67 & !is.na(b$rhco3), "kross"] - 8

  dataframeuitMaakKolomMeth[dataframeuitMaakKolomMeth$meth == "rossum", "rk20"] <- b$KROSS
  rk20uitRossumIndataframeuitMaakKolomMeth <- dataframeuitMaakKolomMeth
  return(rk20uitRossumIndataframeuitMaakKolomMeth)
}




#' Calculate the conductivity of groundwater from its ionic composition
#' @description
#' you will need a data frame with the concentrations of a number of ions
#' as shown in data/input_groundwaterconductivity
#' The calculations can use calcium, chloride, Iran, potassium, magnesium, manganese, sodium, ammonium, nitrate, phosphate, zinc, #' bicarbonate, carbonate in milligrams/liter.
#' Aluminum is used in microgram/liter and the pH is used as such.
#' The conductivity is calculated at 25°C so the measured conductivity might need some temperature adjustment.
#' Missing data are assumed to be zero except a missing pH which will be assumed to be 7.
#' myoutputdataframe<-calculate_conductivity(inputfilename="input_groundwaterconductivity.rda",inputstyle = "Stuyfzand",outputstyle = "minimal", celcius = 25)
#' @param inputfilename The name of the input file in data
#' @param inputstyle The layout of the input file like column names etc
#' @param outputstyle The layout of the output file
#' @param celcius The temperature of the measured conductivity
#' @return A dataframe with the calculated conductivities
#' @importFrom stats lm na.omit rstandard
#' @import dplyr
#' @import tidyr
#' @export
calculate_conductivity <- function(inputfilename="data/input_groundwaterconductivity.rda",
                                  inputstyle = "Stuyfzand",
                                   outputstyle = "Stuyfzand",
                                   celcius = 25) {
  writefiles=FALSE
  if (!exists("input_groundwaterconductivity")){
    # read file if possible
    if (file.exists(inputfilename)){
      input_groundwaterconductivity<-LoadFileInVariable(inputfilename)
      writefiles=TRUE
    }
  }

  # if the input_groundwaterconductivity does not exist and
  # inputstyle is Stuyfzand we will use the standard Table3.1
  if (inputstyle=="Stuyfzand"){
  if (!exists("input_groundwaterconductivity")) {
    # read the original inputfile and save with extra myrownames column
    # in dataframe input_groundwaterconductivity which can have different
    # shapes according to the inputstyle
      input_groundwaterconductivity<-structure(list(no = 1:34,
                     cl = c(5, 32, 4.4, 17.7, 14.9, 83,
                                       15.6, 16.3, 33, 196, 39, 11, 113, 476, 42, 34, 128, 206, 190,
                                       349, 326, 2084, 92, 850, 714, 1790, 3300, 5285, 386, 3120, 4230,
                                       15500, 25350, 189000),
                hco3 = c(38L, 171L, 0L, 55L, 0L, 0L, 0L, 113L, 307L, 716L, 101L, 415L, 32L, 226L, 185L, 352L, 201L, 212L,
                             917L, 4750L, 762L, 1910L, 732L, 72L, 255L, 621L, 1710L, 2635L,
                             134L, 140L, 480L, 261L, 256L, 40L),
                so4 = c(0.9, 0, 5.5, 38.4,
                              2.5, 19.5, 8.9, 21, 1, 6, 116, 437, 62, 17, 63, 140, 48, 100,
                                5, 190, 372, 2, 879, 2130, 19, 230, 27, 4, 5005, 2426, 274, 2050,
                                11170, 11700),
                no3 = c(1.5, 0.1, 3.4, 0, 1.5, 13.1, 8, 24.8,
                            0.1, 0, 0.6, 0, 0, 0, 69.5, 372, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9,
                           0, 0, 0, 0, 0, 0, 0, 63),
                co3 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L,
                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 30L,
                          0L, 0L, 0L, 0L, 0L, 0L, 3L, 0L, 0L, 0L, 0L), h = c(7.6, 7.91,
                          4.07, 7, 4.19, 3.96, 4.63, 7.57, 7.34, 7.77, 7, 7, 6.2, 8, 7.57,
                          7.04, 7, 7.53, 7.26, 7, 7, 7, 7, 7.3, 7.73, 7.53, 6.94, 7, 7,
                           7, 7.12, 7.18, 7, 7),
                na = c(5.9, 23, 1.5, 16.1, 7.9, 45.3, 8.1,
                          6, 20, 350, 23, 297, 62, 165, 26, 19, 161, 103, 170, 1901, 650,
                          1540, 738, 462, 316, 1290, 1900, 3300, 1957, 2415, 1437, 8300,
                          20050, 121000),
                k = c(0.8, 10.9, 0.2, 3.9, 0.37, 2.17, 0.75,
                          0.66, 6.3, 7.4, 2.8, 3.9, 3.7, 22.8, 1.9, 0.3, 0, 2.4, 13.1,
                          96, 0, 44, 3.9, 9.1, 30.7, 32.7, 76, 95, 19.6, 58.7, 21.5, 300,
                          670, 3700),
                ca = c(6.5, 14, 1, 20, 1, 6.2, 3.3, 53.3, 79, 29,
                     58, 20, 29, 112, 101, 285, 18, 121, 200, 106, 10, 102, 16, 696,
                     80, 60, 236, 160, 425, 357, 1038, 430, 1065, 722),
                mg = c(2.6,
                     21, 0.4, 4.9, 0.95, 5.6, 1.3, 3, 7.9, 10, 13, 26.7, 9.1, 52,
                     5, 4.1, 0, 4.7, 12.5, 11.5, 7, 154, 10.9, 204, 75, 69, 290, 390,
                     128, 202, 205, 1110, 115, 2490),
                nh4 = c(0, 0.4, 0.7, 0, 0.3,
                     1.2, 1.9, 0.1, 2.3, 3, 0, 0, 0.4, 1.4, 0.1, 0.1, 0, 0.3, 26.4,
                     0, 0, 37, 0, 0, 0.9, 0.3, 0, 70, 0, 0, 10.1, 8.6, 0, 0),
                k20meas = c(67L,
                     326L, 48L, 212L, 65L, 334L, 87L, 282L, 486L, 1445L, 458L, 1204L,
                     505L, 1660L, 585L, 1248L, 735L, 1021L, 1680L, 6455L, 2580L, 7200L,
                     2645L, 4965L, 2440L, 5810L, 9600L, 14950L, 8340L, 11600L, 11250L,
                     36300L, 65800L, 199100L),
                k20 = c(67L, 322L, 52L, 212L, 70L,
                     340L, 85L, 293L, 492L, 1463L, 456L, 1261L, 505L, 1620L, 578L,
                     1246L, 726L, 1018L, 1663L, 6530L, 2630L, 7320L, 2625L, 4905L,
                     2412L, 5880L, 9824L, 14800L, 8310L, 11600L, 11250L, 36100L, 67500L,
                     215900L)),
                class = "data.frame", row.names = c(NA, -34L))


    }
    myrownames <- as.numeric(row.names(input_groundwaterconductivity))
    s <- cbind(input_groundwaterconductivity, myrownames)
    matrixnamen <- c("xal", "xca", "xcl", "xfe", "xhv", "xk", "xmg", "xmn", "xna", "xnh4", "xno3", "xpo4", "xso4", "xecv", "xzn", "xhco3", "xco3", "myrownames")
    l <- data.frame(matrix(nrow = length(s[, 1]), ncol = length(matrixnamen), 0))
    names(l) <- matrixnamen
    l$xcl <- s$cl
    l$xso4 <- s$so4
    l$xno3 <- s$no3
    l$xhv <- s$h
    l$xna <- s$na
    l$xk <- s$k
    l$xca <- s$ca
    l$xmg <- s$mg
    l$xnh4 <- s$nh4
    l$xhco3 <- s$hco3
    l$xco3 <- s$co3
    l$xecv <- 0.10 * s$k20 / (1 - 0.023 * 5)
    l$myrownames <- s$myrownames
    # # omrekenen naar 25 celcius en mS/m ipv uS/cm
    # # temperatuur formule uit SWE 87-006
    # h$ec25=0.10*h$rk20/(1-0.023*5)

    LMM_broad_input_groundwaterconductivity <- l
    add_phosphate <- FALSE
    add_bicarbonate <- FALSE
  }

  if (inputstyle == "broadLMM") {

    LMM_broad_input_groundwaterconductivity <- input_groundwaterconductivity
    add_phosphate <- FALSE
    add_bicarbonate <- TRUE
  }

  if (inputstyle == "broadLGW") {

    LMM_broad_input_groundwaterconductivity <- input_groundwaterconductivity
     add_phosphate <- TRUE
    add_bicarbonate <- FALSE
  }

  if (inputstyle == "KRWQC") {
    # this is a long style with parameter names in the column parameter
    # remove not measured
    m=input_groundwaterconductivity %>% drop_na(waarde) %>% mutate(cen=(detectieteken=="<"))
    # measurements below detection limit are set to zero
    m[m$cen,"waarde"]=0
    # make broad layout and replace NA with 0
    z=pivot_wider(m,id_cols=c("monsterid", "jaar", "maand", "dag", "filter", "putcode"),
                  names_from = parameter, values_from = waarde)
    z[is.na(z)] <- 0
    myrownames <- row.names(z)
    s <- cbind(z, myrownames)
    # these names are needed for the calculation
    matrixnamen <- c("xal", "xca", "xcl", "xfe", "xhv", "xk", "xmg", "xmn", "xna", "xnh4", "xno3", "xpo4", "xso4", "xecv", "xzn", "xhco3", "xco3", "myrownames")
    # make an empty matrix to fill in the measurement values
    l <- data.frame(matrix(nrow = length(s[, 1]), ncol = length(matrixnamen), 0))
    names(l) <- matrixnamen
    # both measured in µg/Liter
    l$xal=s$Al

    l$xca=s$Ca
    l$xcl=s$Cl
    l$xfe=s$Fe
    l$xhv=s$pH
    l$xk=s$K
    l$xmg=s$Mg
    l$xmn=s$Mn
    l$xna=s$Na
    l$xnh4=s$NH4
    l$xno3=s$NO3
    # all phoshorus is phosphate
    l$xpo4=s$ptot_p

    l$xso4=s$SO4
    # both measured in the field and corrected to 25 Celcius
    l$xecv=s$ec_5__veld

    l$xzn=s$Zn
    l$xhco3=s$hco3_veld
    l$xco3=0
    l$myrownames=s$monsterid

    LMM_broad_input_groundwaterconductivity <- l
    add_phosphate <- FALSE
    add_bicarbonate <- FALSE
  }

  if (inputstyle == "general") {
    LMM_broad_input_groundwaterconductivity <- input_groundwaterconductivity
    add_phosphate <- FALSE
    add_bicarbonate <- FALSE
  }
  # add myrownames if not present
  if (!"myrownames"%in%names(LMM_broad_input_groundwaterconductivity)){
    myrownames <- as.numeric(row.names(LMM_broad_input_groundwaterconductivity))
    LMM_broad_input_groundwaterconductivity <- cbind(LMM_broad_input_groundwaterconductivity, myrownames)
  }

  # replace NA with 0
  LMM_broad_input_groundwaterconductivity<-LMM_broad_input_groundwaterconductivity %>% mutate_if(is.numeric,~replace_na(.,0))
  # save standardized inputfile
  if (writefiles){
  save(LMM_broad_input_groundwaterconductivity, file = "LMM_broad_input_groundwaterconductivity.rda")
  }
  # select proper methods per row
  dataframeuitMaakKolomMeth <- MaakKolomMeth(LMM_broad_input_groundwaterconductivity = LMM_broad_input_groundwaterconductivity, celcius = celcius, add_bicarbonate = add_bicarbonate, add_phosphate = add_phosphate)
  rk20uitBlanquetIndataframeuitMaakKolomMeth <- Blanquet(dataframeuitMaakKolomMeth)
  rk20uitLoganIndataframeuitMaakKolomMeth <- Logan(rk20uitBlanquetIndataframeuitMaakKolomMeth)
  rk20uitDunlapIndataframeuitMaakKolomMeth <- Dunlap(rk20uitLoganIndataframeuitMaakKolomMeth)
  rk20uitMcNealIndataframeuitMaakKolomMeth <- McNeal(rk20uitDunlapIndataframeuitMaakKolomMeth)
  z <- Rossum(rk20uitMcNealIndataframeuitMaakKolomMeth)

  h <- merge(x = LMM_broad_input_groundwaterconductivity, y = z, by = "myrownames", all.x = TRUE, suffixes = c(" mg/l", " meq/l"))
  # omrekenen naar 25 celcius en mS/m ipv uS/cm
  # temperatuur formule uit SWE 87-006
  h$ec25 <- 0.10 * h$rk20 / (1 - 0.023 * 5)
  if (!"xecv" %in% names(h)) {
    h$xecv <- 0.10 * h$k20 / (1 - 0.023 * 5)
  }
  # remove rows without proper data
  myrows <- h$xecv > 0 & h$ec25 > 0
  myrows[is.na(myrows)] <- FALSE
  mlm <- lm(log10(h[myrows, "ec25"]) ~ log10(h[myrows, "xecv"]))
  h[myrows, "ec25_xecv_sr"] <- rstandard(mlm)
  h[myrows, "pxecv"] <- 2^-log10(h[myrows, "xecv"])
  h[myrows, "pec25"] <- 2^-log10(h[myrows, "ec25"])
  # deze formule  komt van Herman Prins en klopt vrij nauwkeurig  drie standaardresiduen
  # uit een logaritmische correlatie van xecv en ec25
  h$prinslabel <- (h$xecv * (1 + h$pxecv) < h$ec25 * (1 - h$pec25)) | (h$xecv * (1 - h$pxecv) > h$ec25 * (1 + h$pec25))
  h$percentage_xecv_ec25 <- 100 * (h$xecv - h$ec25) / h$ec25

  ionbalancelm <- lm(log10(h[myrows, "skat"]) ~ log10(h[myrows, "san"]))
  h[myrows, "skat_san_sr"] <- rstandard(ionbalancelm)

  hanions <- c("cl", "hco3", "so4", "no3", "co3", "oh")
  hkations <- c("h3o", "na", "k", "ca", "mg", "nh4", "fe", "mn")

  h$max_anion <- apply(h[, hanions], 1, max)
  h$max_anion_name <- NA
  for (rownumber in 1:length(h$myrownames)) {
    h[rownumber, "max_anion_name"] <- hanions[(h[rownumber, hanions] == h[rownumber, "max_anion"])]
  }

  h$max_kation <- apply(h[, hkations], 1, max)
  h$max_kation_name <- NA
  for (rownumber in 1:length(h$myrownames)) {
    h[rownumber, "max_kation_name"] <- hkations[(h[rownumber, hkations] == h[rownumber, "max_kation"])]
  }
  h$suspect <- "none"
  # When the calculated conductivity is much higher than the measured one and the ion balance has an excess of kations,
  # then the kation with the maximum concentration of milliequivalents is suspect.
  h[myrows&h$ec25_xecv_sr > 2 & h$skat_san_sr > 2, "suspect"] <- "max_kation"

  # When the calculated conductivity is much higher than the measured one and the ion balance has an excess of anions,
  # then the anion with the maximum concentration of milliequivalents is suspect.
  h[myrows&h$ec25_xecv_sr > 2 & h$skat_san_sr < (-2), "suspect"] <- "max_anion"
  with_all_calculated_conductivity <- h
  # save standard output file
  if (writefiles){
  save(with_all_calculated_conductivity, file = "with_all_calculated_conductivity.rda")
  }

  names_with_all_calculated_conductivity<-c("myrownames", "nr", "jb", "no", "nm", "ob", "wt", "cron", "rhg",
    "nle", "ptc", "labor", "jdecl", "xal", "xas", "xba", "xca", "xcd",
    "xcl", "xcr", "xcu", "xdoc", "xfe", "xk", "xmg", "xmn", "xna",
    "xnh4", "xni", "xno3", "xntot", "xpb", "xpo4", "xptot", "xso4",
    "xsr", "xzn", "nieuw", "ditjaardeelnemer", "trendplaatjeswt",
    "wtob", "xecv", "xh3ov", "xno2v", "xno3v", "hco3", "cl", "so4",
    "no3", "co3", "h3o", "na", "k", "ca", "mg", "nh4", "fe", "mn",
    "al", "zn", "po4", "pos", "neg", "ib", "oh", "san", "skat", "mu",
    "sqmu", "gam2", "alM", "so4M", "DO", "tmp", "also4", "alF", "aloh",
    "aloh2", "alZ", "san2", "skat2", "sgem", "rcl", "rhco3", "rso4",
    "rno3", "meth", "meth_", "rk20", "ec25", "ec25_xecv_sr", "pxecv",
    "pec25", "prinslabel", "percentage_xecv_ec25", "skat_san_sr",
    "max_anion", "max_anion_name", "max_kation", "max_kation_name",
    "suspect")

  i=c("myrownames", "nr", "jb", "no", "nm", "ob", "wt", "cron", "rhg",
                                            "nle", "ptc", "labor", "jdecl", "xal", "xas", "xba", "xca", "xcd",
                                            "xcl", "xcr", "xcu", "xdoc", "xfe", "xk", "xmg", "xmn", "xna",
                                            "xnh4", "xni", "xno3", "xntot", "xpb", "xpo4", "xptot", "xso4",
                                            "xsr", "xzn", "nieuw", "ditjaardeelnemer", "trendplaatjeswt",
                                            "wtob", "xecv", "xh3ov", "xno2v", "xno3v", "hco3", "cl", "so4",
                                            "no3", "co3", "h3o", "na", "k", "ca", "mg", "nh4", "fe", "mn",
                                            "al", "zn", "po4", "pos", "neg", "ib", "oh", "san", "skat", "mu",
                                            "sqmu", "gam2", "alM", "so4M", "DO", "tmp", "also4", "alF", "aloh",
                                            "aloh2", "alZ", "san2", "skat2", "sgem", "rcl", "rhco3", "rso4",
                                            "rno3", "meth", "meth_", "rk20", "ec25", "ec25_xecv_sr", "pxecv",
                                            "pec25", "prinslabel", "percentage_xecv_ec25", "skat_san_sr",
                                            "max_anion", "max_anion_name", "max_kation", "max_kation_name",
                                            "suspect")




  if (outputstyle == "general") {
    with_calculated_conductivity=with_all_calculated_conductivity}

  if (outputstyle == "KRWQC") {
    with_calculated_conductivity=with_all_calculated_conductivity}

  if (outputstyle == "broadLGW") {
    with_calculated_conductivity=with_all_calculated_conductivity}


  if (outputstyle == "Stuyfzand") {
    mycols <- c(
      "myrownames", "xal", "xca", "xcl", "xfe", "xhv", "xk", "xmg",
      "xmn", "xna", "xnh4", "xno3", "xpo4", "xso4", "xecv", "xzn",
      "xhco3", "xco3", "hco3", "cl", "so4", "no3", "co3", "h3o", "na",
      "k", "ca", "mg", "nh4", "fe", "mn", "al", "zn", "po4", "pos",
      "neg", "ib", "oh", "mu", "sqmu", "gam2", "san", "skat", "alM",
      "so4M", "DO", "tmp", "also4", "alF", "aloh", "aloh2", "alZ",
      "san2", "skat2", "sgem", "rcl", "rhco3", "rso4", "rno3", "meth",
      "meth_", "rk20", "ec25", "ec25_xecv_sr", "pxecv", "pec25", "prinslabel",
      "percentage_xecv_ec25", "skat_san_sr", "max_anion", "max_anion_name",
      "max_kation", "max_kation_name", "suspect"
    )
    mysuperfluouscols <- c("pxecv", "pec25", "prinslabel")
    mycharactercols <- c("myrownames", "meth", "meth_", "prinslabel", "max_kation_name", "max_anion_name", "suspect")
    selectedcols <- mycols[!mycols %in% mysuperfluouscols]
    mydatacols <- selectedcols[!selectedcols %in% mycharactercols]
    a <- with_all_calculated_conductivity[, selectedcols]
    a$no <- as.numeric(a$myrownames)
    a[, mydatacols] <- round(a[, mydatacols], 2)
    b <- merge(input_groundwaterconductivity, a, by = "no", suffixes = c("_mg/L", "_meq/L"))
    b$percentage <- round(100 * (b$rk20 - b$k20) / b$k20, 0)
    minimaloutput <- b[, c("no", "k20", "rk20", "percentage", "meth_")]
    with_calculated_conductivity <- b
  }

  if (outputstyle == "minimal") {

    wish_cols <- c(
      "myrownames", "xal", "xca", "xcl", "xfe", "xhv", "xk", "xmg",
      "xmn", "xna", "xnh4", "xno3", "xpo4", "xso4", "xecv", "xzn",
      "xhco3", "xco3", "hco3", "cl", "so4", "no3", "co3", "h3o", "na",
      "k", "ca", "mg", "nh4", "fe", "mn", "al", "zn", "po4", "pos",
      "neg", "ib", "oh", "mu", "sqmu", "gam2", "san", "skat", "alM",
      "so4M", "DO", "tmp", "also4", "alF", "aloh", "aloh2", "alZ",
      "san2", "skat2", "sgem", "rcl", "rhco3", "rso4", "rno3", "meth",
      "meth_", "rk20", "ec25", "ec25_xecv_sr", "pxecv", "pec25", "prinslabel",
      "percentage_xecv_ec25", "skat_san_sr", "max_anion", "max_anion_name",
      "max_kation", "max_kation_name", "suspect"
    )
mycols<-wish_cols[wish_cols%in%names_with_all_calculated_conductivity]
    wishsuperfluouscols <- c(
      "pos",
      "neg", "ib", "oh", "mu", "sqmu", "gam2", "san", "skat", "alM",
      "so4M", "DO", "tmp", "also4", "alF", "aloh", "aloh2", "alZ",
      "san2", "skat2", "sgem", "rcl", "rhco3", "rso4", "rno3", "meth", "rk20", "pxecv", "pec25", "prinslabel", "percentage_xecv_ec25"
    )
    mysuperfluouscols <- wishsuperfluouscols[wishsuperfluouscols%in%names_with_all_calculated_conductivity]
    mycharactercols <- c("myrownames", "meth_", "prinslabel", "max_kation_name", "max_anion_name", "suspect")
    selectedcols <- mycols[!mycols %in% mysuperfluouscols]
    mydatacols <- selectedcols[!selectedcols %in% mycharactercols]
    a <- with_all_calculated_conductivity[, selectedcols]
    a[, mydatacols] <- round(a[, mydatacols], 2)
    with_calculated_conductivity <- a

  }


  if (outputstyle == "broadLMM") {
    inputname <- unlist(strsplit(inputfilename, split = ".", fixed = T))
    newname <- paste0(inputname[1], "_", outputstyle, ".", inputname[2])
    rdsname <- paste0(inputname[1], "_", outputstyle, "_full.rds")

    wish_cols <- c(names(LMM_broad_input_groundwaterconductivity), "cl", "so4", "no3", "na", "k", "ca", "mg", "po4", "hco3", "percentage_xecv_ec25", "ec25", "prinslabel", "ec25_xecv_sr")
    mycols<-wish_cols[wish_cols%in%names_with_all_calculated_conductivity]
    with_calculated_conductivity <- h[, mycols]
  }
  if (writefiles){
  save(with_calculated_conductivity,file="with_calculated_conductivity.rda")
  }
  return(with_calculated_conductivity)
}

# library(tidyverse)
# metveldgemiddelden=readRDS('data/metveldgemiddelden.rds')
# # nieuwe geleidbaarheidsberekening
# input_groundwaterconductivity=metveldgemiddelden
# metgeleidbaarheid=calculate_conductivity(inputfilename = 'zot.rda',inputstyle = "broadLMM",outputstyle = "minimal",celcius=25)
# Oude geleidbaarheidsberekening: metgeleidbaarheid=BerekenGeleidbaarheid(metveldgemiddelden=metveldgemiddelden,celcius=25,add_bicarbonate = TRUE,add_phosphate=FALSE)
# maakt
