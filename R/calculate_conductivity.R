#' Calculate the conductivity of groundwater from its ionic composition
#' @description
#' you will need a data frame with the concentrations of a number of ions
#' as shown in data/Table.csv
#' The calculations can use calcium, chloride, Iran, potassium, magnesium, manganese, sodium, ammonium, nitrate, phosphate, zinc, #' bicarbonate, carbonate in milligrams/liter.
#' Aluminum is used in microgram/liter and the pH is used as such.
#' The conductivity is calculated at 25°C so the measured conductivity might need some temperature adjustment.
#' Missing data are assumed to be zero except a missing pH which will be assumed to be 7.
#' myoutputdataframe<-calculate_conductivity(inputfilename="data/Table.csv",inputstyle = "Stuyfzand",outputstyle = "Stuyfzandstyle", celcius = 25)
#' @param inputfilename The name of the input file in data 
#' @param inputstyle The layout of the input file like column names etc 
#' @param outputstyle The layout of the output file
#' @param celcius The temperature of the measured conductivity
#' @return A dataframe with the calculated conductivities
#' @export
#' @importFrom stats lm na.omit rstandard
#' @importFrom utils write.table read.csv
calculate_conductivity <- function(inputfilename="data/Table.csv",
                                   inputstyle = "Stuyfzand",
                                   outputstyle = "Stuyfzandstyle", 
                                   celcius = 25) {
  
  
  if (inputstyle == "Stuyfzand") {
    # read the original inputfile and save with extra myrownames column
    input_dataframe <- ReadListPointComma(inputfilename)
    myrownames <- row.names(input_dataframe)
    s <- cbind(input_dataframe, myrownames)
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
    
    LMM_broad_input_dataframe <- l
    add_phosphate <- FALSE
    add_bicarbonate <- FALSE
  }
  
  if (inputstyle == "broadLMM") {
    # read the original inputfile and save with extra myrownames column
    LMM_broad_input_dataframe <- LoadFileInVariable(inputfilename)
    add_phosphate <- FALSE
    add_bicarbonate <- TRUE
  }
  
  if (inputstyle == "broadLGW") {
    # read the original inputfile and save with extra myrownames column
    LMM_broad_input_dataframe <- LoadFileInVariable(inputfilename)
    add_phosphate <- TRUE
    add_bicarbonate <- TRUE
  }
  
  inputname <- unlist(strsplit(inputfilename, split = ".", fixed = T))
  rdsname <- paste0(inputname[1], "_", inputstyle, "_LMM_broad_input_dataframe.rds")
  
  saveRDS(LMM_broad_input_dataframe, file = rdsname)
  
  z <- MaakKolomMeth(LMM_broad_input_dataframe = LMM_broad_input_dataframe, celcius = celcius, add_bicarbonate = add_bicarbonate, add_phosphate = add_phosphate)
  z <- Blanquet(z)
  z <- Logan(z)
  z <- Dunlap(z)
  z <- McNeal(z)
  z <- Rossum(z)
  
  h<-merge(x=LMM_broad_input_dataframe,y=z,by="myrownames",all.x=TRUE,suffixes=c(" mg/l", " meq/l"))
  # hx <- dplyr::left_join(s, z, by = "myrownames", suffix = c(" mg/l", " meq/l"))
  # omrekenen naar 25 celcius en mS/m ipv uS/cm
  # temperatuur formule uit SWE 87-006
  h$ec25 <- 0.10 * h$rk20 / (1 - 0.023 * 5)
  if (!"xecv" %in% names(h)) {
    h$xecv <- 0.10 * h$k20 / (1 - 0.023 * 5)
  }
  # h is net zo lang als with_calculated_conductivity
  # alle afwezige en negatieve xecv eruit halen
  myrows <- h$xecv > 0 & !h$meth == "leeg" & h$ec25 > 0 & !h$ec25 == Inf
  myrows[is.na(myrows)] <- FALSE
  mlm <- lm(log10(h[myrows, "ec25"]) ~ log10(h[myrows, "xecv"]))
  h[myrows, "ec25_xecv_sr"] <- rstandard(mlm)
  h[myrows, "pxecv"] <- 2^-log10(h[myrows, "xecv"])
  h[myrows, "pec25"] <- 2^-log10(h[myrows, "ec25"])
  # deze formule  komt van Herman Prins en klopt vrij nauwkeurig  drie standaardresiduen
  # uit een logaritmische correlatie van xecv en ec25
  h$prinslabel <- (h$xecv * (1 + h$pxecv) < h$ec25 * (1 - h$pec25)) | (h$xecv * (1 - h$pxecv) > h$ec25 * (1 + h$pec25))
  h$percentage_xecv_ec25 <- 100 * (h$xecv - h$ec25) / h$ec25
  
  #
  ionbalancelm <- lm(log10(h[myrows, "skat"]) ~ log10(h[myrows, "san"]))
  h[myrows, "skat_san_sr"] <- rstandard(ionbalancelm)
  
  # plot(log10(h[myrows, "skat"]),log10(h[myrows, "san"]))
  # abline(coef = c(0,1))
  
  # hanions <- c("cl meq/l", "hco3 meq/l", "so4 meq/l", "no3 meq/l", "co3 meq/l", "oh")
  # hkations <- c("h3o", "na meq/l", "k meq/l", "ca meq/l", "mg meq/l", "nh4 meq/l", "fe", "mn")
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
    h[h$ec25_xecv_sr > 2 & h$skat_san_sr > 2, "suspect"] <- "max_kation"

  # When the calculated conductivity is much higher than the measured one and the ion balance has an excess of anions, 
  # then the anion with the maximum concentration of milliequivalents is suspect.
    h[h$ec25_xecv_sr > 2 & h$skat_san_sr < (-2), "suspect"] <- "max_anion"
  # z$san <- z$cl + z$hco3 + z$so4 + z$no3 + z$co3 + z$oh
  # z$skat <- z$h3o + z$na + z$k + z$ca + z$mg + z$nh4 + z$fe + z$mn
  #
  
  
  with_all_calculated_conductivity <- h
  rdsname <- paste0(inputname[1], "_", inputstyle, "_LMM_broad_output_dataframe.rds")
  saveRDS(with_all_calculated_conductivity, file = rdsname)
  # h<- readRDS("/rivm/r/M350001_ondersteuning_mestbeleid_data/Patrick/groundwaterconductivity/data/Table_Stuyfzand_LMM_broad_output_dataframe.rds")
  
  
  if (outputstyle == "Stuyfzandstyle") {
    inputname <- unlist(strsplit(inputfilename, split = ".", fixed = T))
    newname <- paste0(inputname[1], "_", outputstyle, ".", inputname[2])
    mycols <- c(
      "cl mg/l", "hco3 mg/l", "so4 mg/l", "no3 mg/l", "co3 mg/l",
      "h", "na mg/l", "k mg/l", "ca mg/l", "mg mg/l", "nh4 mg/l",
      "myrownames", "meth_", "ib", "k20", "rk20",
      "ec25", "xecv",
      "percentage_xecv_ec25", "ec25_xecv_sr", "skat_san_sr", "skat", "san",
      "max_anion", "max_anion_name", "max_kation", "max_kation_name", "suspect"
    )
    
    with_calculated_conductivity <- with_all_calculated_conductivity[, mycols]
    with_calculated_conductivity[, c("percentage_xecv_ec25", "rk20", "ec25", "xecv", "ib", "ec25_xecv_sr", "skat_san_sr", "skat", "san", "max_anion", "max_kation")] <-
      round(with_calculated_conductivity[, c("percentage_xecv_ec25", "rk20", "ec25", "xecv", "ib", "ec25_xecv_sr", "skat_san_sr", "skat", "san", "max_anion", "max_kation")], 2)
    
    names(with_calculated_conductivity) <- c(
      "Cl mg/l", "HCO3 mg/l", "SO4 mg/l", "NO3 mg/l", "CO3 mg/l",
      "pH", "Na mg/l", "K mg/l", "Ca mg/l", "Mg mg/l", "NH4 mg/l",
      "myrownames", "method", "ionbalance", "k20_calc", "rk20_calc",
      "ec25_calc", "xecv_measured",
      "percentage_xecv_ec25", "ec25_xecv_sr", "skat_san_sr", "kations meq/l", "anions meq/l",
      "max_anion meq/l", "max_anion_name", "max_kation meq/l", "max_kation_name", "suspect"
    )
    
    
    WriteListPointComma(with_calculated_conductivity, filename = newname)
  }
  
  if (outputstyle == "minimal") {
    mycols=c("myrownames", "no", "cl mg/l", "hco3 mg/l", "so4 mg/l", "no3 mg/l", 
      "co3 mg/l", "h", "na mg/l", "k mg/l", "ca mg/l", "mg mg/l", "nh4 mg/l", 
      "k20meas", "k20", "orderK20", "hco3 meq/l", "cl meq/l", "so4 meq/l", 
      "no3 meq/l", "co3 meq/l", "h3o", "na meq/l", "k meq/l", "ca meq/l", 
      "mg meq/l", "nh4 meq/l", "fe", "mn", "al", "zn", "po4", "pos", 
      "neg", "ib", "oh", "mu", "sqmu", "gam2", "san", "skat", "alM", 
      "so4M", "DO", "tmp", "also4", "alF", "aloh", "aloh2", "alZ", 
      "san2", "skat2", "sgem", "rcl", "rhco3", "rso4", "rno3", "meth", 
      "meth_", "rk20", "ec25", "xecv", "ec25_xecv_sr", "pxecv", "pec25", 
      "prinslabel", "percentage_xecv_ec25", "skat_san_sr", "max_anion", 
      "max_anion_name", "max_kation", "max_kation_name", "suspect")
    mycols=c("myrownames", "no", "hco3 meq/l", "cl meq/l", "so4 meq/l", 
             "no3 meq/l", "co3 meq/l", "h3o", "na meq/l", "k meq/l", "ca meq/l", 
             "mg meq/l", "nh4 meq/l", "fe", "mn", "al", "zn", "po4", "pos", 
             "neg", "ib", "oh","san2", "skat2", 
             "meth_", "ec25", "xecv", "ec25_xecv_sr","skat_san_sr", "max_anion", 
             "max_anion_name", "max_kation", "max_kation_name", "suspect")
    mydatacols=c("hco3 meq/l", "cl meq/l", "so4 meq/l", 
                 "no3 meq/l", "co3 meq/l", "h3o", "na meq/l", "k meq/l", "ca meq/l", 
                 "mg meq/l", "nh4 meq/l", "fe", "mn", "al", "zn", "po4", "pos", 
                 "neg", "ib", "oh","san2", "skat2", 
                 "ec25", "xecv", "ec25_xecv_sr", "skat_san_sr", "max_anion", 
                 "max_kation")
    a=h[,mycols]
    a[,mydatacols]=round(a[,mydatacols],2)
  }
  
  
  if (outputstyle == "BroadLMMstyle") {
    inputname <- unlist(strsplit(inputfilename, split = ".", fixed = T))
    newname <- paste0(inputname[1], "_", outputstyle, ".", inputname[2])
    rdsname <- paste0(inputname[1], "_", outputstyle, "_full.rds")
    
    save(with_all_calculated_conductivity, file = rdsname)
    mycols <- c(names(LMM_broad_input_dataframe), "cl", "so4", "no3", "na", "k", "ca", "mg", "po4", "hco3", "xhco3e", "percentage_xecv_ec25", "ec25", "prinslabel", "ec25_xecv_sr")
    with_calculated_conductivity <- h[, mycols]
    WriteListPointComma(with_calculated_conductivity, filename = newname)
  }
  
  
  return(with_calculated_conductivity)
}