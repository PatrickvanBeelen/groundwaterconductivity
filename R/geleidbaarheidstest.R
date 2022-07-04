# Patrick van Beelen patrick.van.beelen@gmail.com 2021-12-13
# Het script geleidbaarheidstest.R berekent de geleidbaarheid met behulp van de methode van Stuyfzand in het rapport 
# r:\Projecten\M350001_ondersteuning_mestbeleid_data\Patrick\calcgeleidbaarheid\SWE 87.006 Een zeer nauwkeurige berekening van het elektrisch geleidingsvermogen, ter controle en aanvulling van wateranalys.pdf
# Uit dit rapport is tabel31 nagerekend. Deze tabel staat op 
# r:\Projecten\M350001_ondersteuning_mestbeleid_data\Patrick\PatricksFuncties\StuyfzandTable31.csv
# De gemeten geleidbaarheid in deze tabel was in µS/cm bij 20 °C en wordt omgerekend naar mS/m bij 25 °C 
# omdat binnen het LMM deze geleidbaarheidparameter gebruikt wordt. De parameters staan beschreven in 
# s:\Model&Applicatie\qBase\plattematrix_info_29102021.docx.
# De functie BerekenGeleidbaarheid heeft de mogelijkheid om bicarbonaat te schatten uit de ionbalans 
# zoals bij LMM gebruikelijk is omdat daar geen bicarbonaat gemeten wordt. 
# De functie heeft ook de mogelijkheid om het fosfaatgehalte te schatten uit totaal fosfor zoals binnen het LMG gebruikelijk is. 
# De geleidbaarheid wordt berekend bij 25 °C maar je kunt ook een andere temperatuur invoeren.

rm(list=ls())
graphics.off();
close.screen(all.screens=TRUE)# Inladen pakketten
patricksfunctiesnaam="/rivm/r/M350001_ondersteuning_mestbeleid_data/Patrick/PatricksFuncties/AlgemeneFunctiesPatrick.R"
StuyfzandTable31naam="/rivm/r/M350001_ondersteuning_mestbeleid_data/Patrick/PatricksFuncties/StuyfzandTable31.csv"
# StuyfzandTable31naam="testtabel31.csv"
# Patricks functies laden
source(patricksfunctiesnaam)
# source(patricksLMMfunctiesnaam)

LeesStuyfzandTable31<-function(StuyfzandTable31naam="StuyfzandTable31.csv"){
  #' maakt een invoerbestand in plattematrix layout om de ec25 berekening van Stuyfzand te testen 
  #' Stuyfzand, P. (1987). 
  #' Een zeer nauwkeurige berekening van het elektrischgeleidingsvermogen, ter controle en aanvulling van wateranalyses: 
  #' 2e versie (SWE 87.006). Retrieved from Rijswijk: 
  
  s=ReadListPointComma(StuyfzandTable31naam)
  matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3')
  l=data.frame(matrix(nrow=length(s[,1]),ncol=length(matrixnamen),0)) 
  names(l)=matrixnamen
  l$xcl=s$cl
  l$xso4=s$so4
  l$xno3=s$no3
  l$xhv=s$h
  l$xna=s$na
  l$xk=s$k
  l$xca=s$ca
  l$xmg=s$mg
  l$xnh4=s$nh4
  l$xhco3=s$hco3
  l$xco3=s$co3
  l$xecv=0.10*s$k20/(1-0.023*5)
  
  # # omrekenen naar 25 celcius en mS/m ipv uS/cm
  # # temperatuur formule uit SWE 87-006
  # h$ec25=0.10*h$rk20/(1-0.023*5)

  stuyfzandtable=l
  saveRDS(stuyfzandtable,file='stuyfzandtable.rds')
  return(stuyfzandtable)  
}


# test
# Maak invoerbestand met de juiste LMM kolom namen
invoer=LeesStuyfzandTable31(StuyfzandTable31naam)
# Berekende geleidbaarheid bij 25 °C en pas geen trucjes toe 
# zoals het verzinnen van het bicarbonaat gehalte uit de ionenbalans of het schatten van het fosfaatgehalte uit totaal fosfor.
uitvoer=BerekenGeleidbaarheid(metveldgemiddelden=invoer,celcius=25,add_bicarbonate = FALSE,add_phosphate=FALSE)


# voor LMM
source("/rivm/r/M350001_ondersteuning_mestbeleid_data/Patrick/PatricksFuncties/AlgemeneFunctiesPatrick.R")
uitvoer=BerekenGeleidbaarheid(metveldgemiddelden=invoer,celcius=25,add_bicarbonate = TRUE,add_phosphate=FALSE)
