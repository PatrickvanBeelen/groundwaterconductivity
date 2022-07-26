#' Prepare for the conductivity calculations by selecting the proper method
MaakKolomMeth <- function(LMM_broad_input_dataframe = LMM_broad_input_dataframe, celcius = celcius, add_bicarbonate = add_bicarbonate, add_phosphate = add_phosphate) {
  #   matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3')

  zm <- as.data.frame(LMM_broad_input_dataframe)
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
