Rossum <- function(z = dataframeuitMaakKolomMeth) {
  #' Rossum volgens Ec_voor_Patrick.docx  voor ec25 berekening
  b <- z[z$meth == "rossum", ]
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
  
  z[z$meth == "rossum", "rk20"] <- b$KROSS
  rk20uitRossumIndataframeuitMaakKolomMeth <- z
  return(rk20uitRossumIndataframeuitMaakKolomMeth)
}
