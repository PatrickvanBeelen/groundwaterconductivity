# ================================================================================== #
#                                                                                    #
#                                                                                    #
#                                                                                    #
# Auteurs: Patrick van Beelen                                                        #
# Laatste aanpassing: 2022-01-13-                                                    #
# Status : op 20210913 veel oude functies eruit gedaan                 #
# ================================================================================== #
# 
#

MakeWorksheet<-function(dataframenaam=afgerondeoverzichtstabel,sheetnaam='hoeft_niet_ingevuld',filenaam=paste0(sheetnaam,'.xlsx'),bevriesrij=1,bevrieskolom=4){  #' gebruikt openxlsx aanroepen met  makeWorksheet(rangordekleur) of
  #' makeWorksheet(dataframenaam = rangordekleur,sheetnaam = 'zot3',filenaam = 'zot5.xlsx',bevriesrij = 2,bevrieskolom = 1)
  
  if (sheetnaam=='hoeft_niet_ingevuld'){
    sheetnaam=deparse(substitute(dataframenaam))
  }
  if (nchar(sheetnaam)>31){
    sheetnaam=substr(sheetnaam,start=1,stop=31)
  }
  wb<-createWorkbook()
  addWorksheet(wb,sheetnaam)
  setColWidths(wb,sheetnaam, cols = 1:ncol(dataframenaam), widths = "auto")
  writeData(wb,1,dataframenaam)
  addFilter(wb,1,row=1,cols=1:ncol(dataframenaam))
  freezePane(wb,sheetnaam,firstActiveRow = bevriesrij+1,firstActiveCol = bevrieskolom+1)
  ## modify base font to size 11 Arial Narrow in red
  modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Lucida Sans Unicode")
  # firstrowStyle <- createStyle(textDecoration = "Bold",border="Right",borderColour = "black", fontColour = "black")
  # addStyle(wb,sheetnaam,firstrowStyle,cols=1:ncol(dataframenaam),rows = bevriesrij)
  saveWorkbook(wb,file=filenaam,overwrite = TRUE)
}

DecimalYearToDate<-function(numericvector){
  #' maakt van een vector met numerieke decimale jaren zoals 1956.202 een datum  15-03-1956
  #' heeft lubridate nodig ik tel er een minuut bij op om na 00:00 uur uit te komen
  stringvector=format(date_decimal(as.numeric(numericvector)+1/(365*24*60),tz="UTC"), "%d-%m-%Y")
  return(stringvector)
}

DateToDecimalYear<-function(daymonthyearstringvector){
  #' maakt van een vector met data zoals 15-03-1956
  #' heeft lubridate nodig 
  decimalvector=decimal_date(parse_date_time(daymonthyearstringvector,orders="d-m-Y"))
  return(decimalvector)
}

RondMooiAf <- function(df) {
  # rond de numerieke kolommen (nums) van een dataframe af zodat het minimum te zien blijft
  nums <- vapply(df, is.double, FUN.VALUE = logical(1))
  # mini is een dataframe met 1 rij absolute waarde van de minima
  mini <- abs(apply(df[,nums],2,min,na.rm=TRUE))
  # af is een dataframe met alleen de getallen
  af=df[,nums]
for (digits in 6:0){
  digitgrens=trunc((10^digits)*mini)
  grens=digitgrens>=1&digitgrens<10
  af[,grens] <- round(af[,grens], digits+1)

  if (digits==0){
    digitgrens=trunc((10^digits)*mini)
    grens=digitgrens>1
    af[,grens] <- round(af[,grens], digits+1)
  }
}
  df[,nums]=af
  return(df)
}

RondDataframeAf <- function(df, digits) {
  # rond de numerieke kolommen (nums) van een dataframe af tot digits cijfers
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  mini <- apply(df[,nums],2,min)
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

WriteBroadExcelCorrelation<-function(invoer=bredecorrelatietabel,sheetnaam='correlatie',afronding=2,
                                     rodegrens='>0.9',oranjegrens='>0.7',groenegrens='>0.5',grensrode='<-0.9',grensoranje='<-0.7',grensgroene='<-0.5'){
  # library(openxlsx) is nodig maakt een correlatietabel met kleur
  cm=cor(invoer,use = "pairwise.complete.obs")
  data=round(cm,afronding)
  # sheetnaam='cor'
  # rodegrens='>0.9'
  # oranjegrens='>0.7'
  # groenegrens='>0.5'
  # grensrode='<-0.9'
  # grensoranje='<-0.7'
  # grensgroene='<-0.5'
  
  data=as.data.frame(data)
  data$name=row.names(data)
  data=data[,c('name',names(data)[!names(data)=='name'])]
  wb <- createWorkbook()
  sht=addWorksheet(wb, sheetnaam)
  ## modify base font to size 10 Arial Narrow in red
  modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Lucida Sans Unicode")
  # percentStyle=createStyle(numFmt='0%')
  redStyle=createStyle(fontColour = "black",bgFill = 'red')
  orangeStyle=createStyle(fontColour = "black",bgFill = 'orange')
  greenStyle=createStyle(fontColour = "black",bgFill = 'green')
  ## headerStyles
  headerStyle <- createStyle(textDecoration = "Bold",border = "Bottom", fontColour = "black")
  firstrowStyle <- createStyle(textDecoration = "Bold",border="Right",borderColour = "black", fontColour = "black")
  
  conditionalFormatting(wb,sheetnaam,rule=groenegrens,cols=(1:ncol(data))+1,rows=(1:nrow(data))+1,style=greenStyle)
  conditionalFormatting(wb,sheetnaam,rule=oranjegrens,cols=(1:ncol(data))+1,rows=(1:nrow(data))+1,style=orangeStyle)
  conditionalFormatting(wb,sheetnaam,rule=rodegrens,cols=(1:ncol(data))+1,rows=(1:nrow(data))+1,style=redStyle)
  conditionalFormatting(wb,sheetnaam,rule=grensgroene,cols=(1:ncol(data))+1,rows=(1:nrow(data))+1,style=greenStyle)
  conditionalFormatting(wb,sheetnaam,rule=grensoranje,cols=(1:ncol(data))+1,rows=(1:nrow(data))+1,style=orangeStyle)
  conditionalFormatting(wb,sheetnaam,rule=grensrode,cols=(1:ncol(data))+1,rows=(1:nrow(data))+1,style=redStyle)
  
  addStyle(wb,sht,firstrowStyle,cols=1,rows = 1:ncol(data))
  
  writeData(wb,sht,data,headerStyle = headerStyle)
  setColWidths (wb,sht, cols=1:ncol(data),widths = "auto")
  setColWidths (wb,sht, cols=1,widths = "7")
  filenaam=paste0(sheetnaam,'.xlsx')
  saveWorkbook(wb, filenaam, overwrite = TRUE)
  save(data,file=paste0(sheetnaam,'.rda'))
}

ZeeWater<-function(langeplattematrix=metgemid,rijnamen='rijnamen',getallenkolom='obs'){
  # Deze functie heeft als invoer een LMM databestand met onder variable de stofnamen
  # Gewichtsratio chloride met element uit Millero,2008
  # Millero, F. J., Feistel, R., Wright, D. G., & McDougall, T. J. (2008).
  # The composition of Standard Seawater and the definition of the Reference-Composition Salinity Scale.
  # Deep Sea Research Part I: Oceanographic Research Papers, 55(1), 50-72. doi:https://doi.org/10.1016/j.dsr.2007.10.001
  
  # Voor de belangrijkste parameters uit Millero,2008
  # heb ik diegenen gekozen die we bij het LMM ook meten
  # wanneer een stof de perfecte zeewater verhoudingen heeft met chloride
  # dan is z$Millero gelijk aan 1
  # alle parameters zijn in g/m3 behalve xsr dat in mg/m3 gaat
  # werkt ook voor metperfilter van LMG kiest dan rijnamen in plaats van mengsleutels msl
  
  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(tidyr)
  # 
  z=langeplattematrix
  LMMname=as.character(c('xcl',"xna", "xmg","xca", "xk","xsr", "xso4"))
  MilleroRatio=c(1,0.55649, 0.06626, 0.02127, 0.0206, 0.41,0.14)
  
  # De kleine selectie correleert goed met xcl en zou dus geschikt
  # kunnen zijn om verdund zeewater te detecteren met behulp van de MilleroRatio's
  # wanneer deze alle 4 gelijk aan 1 zijn dan is het verdund zeewater
  m=tibble(LMMname,MilleroRatio)

  namen=c("nrjbnowtobnm", "variable",getallenkolom, rijnamen)
  # 
  # Ieder mengmonster heeft 1 unieke rijnaam nu zetten we de rijen breed op rij
  q=pivot_wider(z[,namen],names_from ='variable',values_from = getallenkolom)
  q=q[,c('nrjbnowtobnm',rijnamen,LMMname)]
  # Millerochloride geldt voor het hele monster
  q$Millerochloride=q$xcl
  
  # nu weer even een lange tabel maken voor vaste ratio, Millero en MK
  r=pivot_longer(q,all_of(LMMname),names_to = 'variable')
  r$vasteratio=NA_real_
  for (rij in 1:length(m$LMMname)){
    r[r$variable==as.character(m[rij,'LMMname']),'vasteratio']=m[rij,'MilleroRatio']
  }
  r$Millero=r$value/(r$Millerochloride*r$vasteratio)
  r$MK=log10(r$Millero)^2
  # Millero en MK zijn per parameter verschillend

  # MK sommeren per rij om SSSea te krijgen 
s=r[,c(rijnamen,'MK')] %>% group_by(rijnamen) %>% summarize_all(sum)
names(s)=c(rijnamen,'SSSea')
# max MK zoeken per rij 
g=r[,c(rijnamen,'MK')] %>% group_by(rijnamen) %>% summarize_all(max)
names(g)=c(rijnamen,'maxMK')
# SSSea geldt voor de hele rij en kan dus aan de brede matrix worden toegevoegd
gs=merge(g,s)
gs$bg=gs$maxMK/(gs$SSSea-gs$maxMK)

# SSSea maxMK en bg gelden voor de hele rij en kan dus aan de brede matrix worden toegevoegd
t=merge(q,gs)


# zeewater bevat 19.3 gram chloride per kg
# 1% is dus 193 mg chloride
t$percentagezeewater=t$xcl/193.5271
breedzeewater=t
saveRDS(breedzeewater,'breedzeewater.csv')

# namen=c("rijnamen", "nrjbnowtobnm", "xcl", "xna", "xmg", "xca", "xk", 
#   "xsr", "xso4", "Millerochloride", "SSSea", "percentagezeewater"
# )
# v komt uit het hele monster
v=pivot_longer(t,all_of(LMMname),names_to = 'variable')
# v1=v[,c("rijnamen", "nrjbnowtobnm", "Millerochloride","variable")]
v=v[,c(rijnamen, "nrjbnowtobnm","Millerochloride",'maxMK','bg', "SSSea", "percentagezeewater")]
v=unique(v)
# v3=merge(v1,v2)
# de algemene parameters per monster toevoegen
u=merge(z,v,all.x = T)
namesr=c("nrjbnowtobnm", "rijnamen", "variable", 
  "vasteratio", "Millero", "MK")

u2=merge(u,r[,namesr],all.x = T)
# de specifieke parameters per stof toevoegen
  # Millerochloride, SSSea en percentagezeewater gelden voor het hele monster
  metzeewater=u2
  saveRDS(metzeewater,file='metzeewater.rds')
  # save(metzeewater,file='metzeewater.rda')
  return(metzeewater)
}

setpaperwidth<-function(orientation="landscape") {
  # returns A4 paper sizes depending on orientation
  #
  # options:
  # orientation: paper orientation, either landscape or portrait
  
  # calculate wdth in inches
  widtha4<-297/10/2.54; 
  heighta4<-210/10/2.54;
  
  # get orientation
  if(orientation!="portrait"&&orientation!="landscape") {
    stop("wrong orientation");
  }
  
  # create list with sizes
  a4<-list();
  if(orientation=="landscape") {
    a4$width<-widtha4;
    a4$height<-heighta4;
  } else {
    a4$width<-heighta4;
    a4$height<-widtha4;
  }
  a4$orientation<-orientation
  
  # return list with width, height, and orientation
  return (a4)
}

LoadFileInVariable <- function(fileName){
  #' loads an RData file use variablename<-LoadFileInVariable(filename) 
  load(fileName)
  get(ls()[ls() != "fileName"])
}

RemoveEmptyRows<-function(mydataframe){
  #' verwijdert lege regels
  cleandataframe=mydataframe[rowSums(is.na(mydataframe))!=ncol(mydataframe),]
  return(cleandataframe)
}

RemoveEmptyColumns<-function(mydataframe){
  #' verwijdert lege kolommen
  cleandataframe=mydataframe[,colSums(is.na(mydataframe))!=nrow(mydataframe)]
  return(cleandataframe)
}

ConvertFactorsToStrings<-function(mydataframe){
  #' factors naar strings veranderen
  i <- sapply(mydataframe, is.factor)
  mydataframe[i] <- lapply(mydataframe[i], as.character)
  return(mydataframe)
}

ConvertFactorsToNumbers<-function(mydataframe){
  #' factors naar nummers veranderen
  #' eerst alles naar character en dan naar nummer
  #' kolommen met getallen en letters bijv <1.5
  #' worden omgezet in getallen en NA
  m=mydataframe
  i <- sapply(m, is.factor)
  m[i] <- lapply(m[i], as.character)
  j<-sapply(m, is.character)
  m[j] <- lapply(m[j], as.numeric)
  k<-lapply(m,is.na)
  kk=unlist(lapply(k,all))
  m[,kk]=mydataframe[,kk]
  return(m)
}

is.nan.data.frame<- function(x)
  do.call(cbind, lapply(x, is.nan))
# aanroepen met mydataframe[is.nan(mydataframe)]<-NA

NA2zero <- function (x) {
  x[is.na(x)] <- 0
  x[is.nan(x)] <- 0
  return(x)
}

NA2FALSE <- function (x) {
  x[is.na(x)] <- FALSE
  x[is.nan(x)] <- FALSE
  return(x)
}

Zie<-function(stringlist,kleinerdanteken="<"){
  #' geeft informatie over de kolommen van een lijst gegevens
  # 
  # Args:
  #   stringlist: een aantal kolommen gevuld met getallen of cijfers
  #   getallen als "12" worden herkend als getal 
  #   "<0.4" als kleinerdan
  #   NA als niet aanwezig
  # Returns:
  #    een dataframe met informatie in een rij over iedere kolom
  #    zoals het aantal kleinerdan waarden en het aantal unieke waarden
  z4<-data.frame(stringlist,stringsAsFactors=FALSE)
  z4[]<-lapply(z4,as.character)
  #kleinerdanteken="<"
  # taRifx::destring voorkomt overbodige waarschuwingen
  #cijfers<-sapply(z4, function(x) length(na.omit(destring(na.omit(x)))))
  #letters<-sapply(z4, function(x) sum(is.na(destring(na.omit(x)))))
  # het kan ook zonder destring zie de 2 regels hieronder maar dan krijg je foutmeldingen
  cijfers<-sapply(z4, function(x) length(na.omit(as.numeric(na.omit(x)))))
  nietcijfers<-sapply(z4, function(x) sum(is.na(as.numeric(na.omit(x)))))
  NAwaarden<-sapply(z4, function(x) sum(is.na(x)))
  kleinerdan<-sapply(z4, function(x) sum(substr(x,1,1)==kleinerdanteken,na.rm=TRUE))
  waar<-sapply(z4, function(x) sum(as.numeric(x)==TRUE,na.rm=TRUE))
  nietwaar<-sapply(z4, function(x) sum(as.numeric(x)==FALSE,na.rm=TRUE))
  minimum<-sapply(z4, function(x) min(as.numeric(x),na.rm=TRUE))
  maximum<-sapply(z4, function(x) max(as.numeric(x),na.rm=TRUE))
  uniek<-sapply(z4, function(x) length(unique(x)))
  #een cijfer met een < teken ervoor is een nietcijfer maar geen letter
  letters<-nietcijfers-kleinerdan
  info<-data.frame(kleinerdan,cijfers,letters,NAwaarden,uniek,waar,nietwaar,maximum,minimum,stringsAsFactors=FALSE)
  infolijst<-cbind(rijnamen=rownames(info),info)
  return(infolijst)
}

ReadListPointComma<-function(filename="WriteListPointComma.csv"){
  #' Leest een lijst uit een ; separated value csv bestand
  # gemaakt door WriteListPointComma
  # een oude versie was:
  # output <- read.csv(file = filename, header = TRUE, sep = ";", quote = "", na.strings = "NA", comment.char = "",,stringsAsFactors=FALSE)
  output<-  try(read.csv(file = filename,
                     header = TRUE, sep = ";", quote = "\"",
                     na.strings = c("NA","NaN"),
                     stringsAsFactors=FALSE, strip.white=TRUE,
                     check.names=TRUE))
  if (class(output)=='try-error'){
    cat('\n',filename,'not found in ',getwd(),'\n\n')
  }
  return(output)
}

WriteListPointComma<-function(inlist,filename="WriteListPointComma.csv"){
  #' Schrijft een lijst weg als ; separated value
  # met "om de tekst" door quote=TRUE en 
  # zonder een 1e kolom voor de rijnamen
  inlist=as.data.frame(inlist)
  write.table(inlist,filename, sep=';',
              fileEncoding="UTF-8",
              quote =TRUE,
              qmethod = 'double',
              row.names=FALSE,
              dec='.',
              eol="\n")
  #een oude versie was:
  #write.table(inlist,filename, sep=';',row.names=TRUE, col.names=NA, quote=TRUE, ,eol="\n")
}

LeesMetKomma<-function(filename="LeesMetDecimaleKomma.csv"){
  #' Leest een lijst uit een ; separated value csv bestand 
  #' gemaakt door SchrijfMetKomma met decimale komma voor 
  # voor Nederlands rekenvel
  output<-  try(read.table(filename,header = T,dec=',',sep=';',encoding = "UTF8"))
  if (class(output)=='try-error'){
    mystring='\n try(read.table(filename header is T dec is komma sep is puntkomma encoding is UTF8))'
    cat('\n',filename,'niet gevonden in \n',getwd(),mystring,'\n\n\n')
  }
  return(output)
}

SchrijfMetKomma<-function(inlist,filename="SchrijfMetDecimaleKomma.csv"){
  #' Schrijft een lijst weg als ; separated value met decimale Nederlandse komma
  # met "om de tekst" door quote=TRUE en 
  # zonder een 1e kolom voor de rijnamen
  write.table(inlist,file=filename,dec=',',sep=';',na="",row.names=FALSE)
}

NAros<-function(obs=obs,cen=cen){
  #' de functie robust order statistics van NADA geschikt gemaakt voor NA waarden 
  # de functie geeft de originele volgorde van de metingen
  # terug
  # met daarnaast nog een rijtje statistische gegevens:
  # wanneer er maar 1 waarde boven de detectiegrens is dan geeft
  # rmean die waarde gedeeld door het aantal waarnemingen
  # library(NADA) moet gestart zijn
  # NAros(obs,cen)$mean[1] geeft het gemiddelde
  # NaN not a number wordt vervangen door NA not available
  # voorbeeld
  # library(NADA)
  # Voorbeeld
  # obs=c(3,1,NA,NaN,0.1,0.2)
  # cen=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
  # # NAros(obs=obs,cen=cen)
  # NAros(obs=obs,cen=cen)$meantxt[1]
  
  
  # cbind maakt soms character van cen als obs dat ook is
  # cbind.data.frame is beter
  
  obsdouble=obs
  if (!typeof(obs)=="double"){
    # cat("typeof(obs)= ",typeof(obs))
    if (typeof(obs)=="character"){
      obsdouble=as.numeric(obs)
      if (typeof(obs)=="factor"){
        obsdouble=as.numeric(as.character(obs))
      }
    }
  }
  obsdouble[is.nan(obsdouble)]=NA
  cenlogical=cen
  if (!typeof(cen)=="logical"){
    # cat("typeof(cen)= ",typeof(cen))
    if ("FALSE"%in%cen|"TRUE"%in%cen){
      cenlogical=as.logical(as.character(cen))
    }
    if ("0"%in%cen|"1"%in%cen)
      cenlogical=as.logical(cen)
  }
  cen=cenlogical
  obs=obsdouble

  df=cbind.data.frame(obs,cen)
  df[,'rij']=row.names(df)
  df$aantalbovendetectiegrens=NA
  df$allobs=NA
  df$meantxt=NA
  # Dropped censored values that exceed max of uncensored values.
  if (any(df$cen==TRUE,na.rm=TRUE)&any(df$cen==FALSE,na.rm=TRUE)){
    maxuncensored=max(df[!df$cen,'obs'],na.rm=TRUE)
    if (!is.na(maxuncensored)){
      abovemax=df[,'obs']>maxuncensored
      ml=df$cen&abovemax
      # de NA worden FALSE
      ml[which(is.na(ml))]=FALSE
      # censored and above max uncensored becomes NA
      df[ml,'obs']=NA
    }
  }
  
  dfo=df[order(df$obs),]
  row.names(dfo)=NULL
  dfo[,'rank']=row.names(dfo)
  #  de goodrows gaan in ros
  goodrows=is.finite(dfo$obs)
  allobs=sum(is.finite(dfo$obs))
  bovendetectiegrensrows=goodrows&!dfo$cen
  aantalbovendetectiegrens=length(dfo[bovendetectiegrensrows,'obs'])
  
  if (aantalbovendetectiegrens==0){
    dfo$mean=NA
    dfo$sd=NA
    dfo$rstandard=NA
    dfo$rmodeled=NA
    if (allobs>0){
      dfo$meantxt=paste0('<',max(df[goodrows,'obs']))
      dfo$aantalbovendetectiegrens=aantalbovendetectiegrens
      dfo$allobs=allobs
    }
  }else if (aantalbovendetectiegrens==1|aantalbovendetectiegrens==2){
    dfo$mean=sum(dfo[goodrows&!dfo$cen,'obs'])/allobs
    dfo$sd=NA
    dfo$rstandard=NA
    dfo$rmodeled=NA
    dfo$meantxt=as.character(dfo$mean)
    dfo$aantalbovendetectiegrens=aantalbovendetectiegrens
    dfo$allobs=allobs
    
  }else {
    myobs=as.numeric(dfo[goodrows,'obs'])
    mycen=as.logical(dfo[goodrows,'cen'])
    r=suppressWarnings(ros(myobs,mycen))
    # Dropped censored values that exceed max of uncensored values.
    meanresiduals=mean(log(r$obs))
    sdevr=sd(log(r$obs))
    dfo[goodrows,'rstandard']=(log(r$obs)-meanresiduals)/sdevr
    dfo[goodrows,'rmodeled']=r$modeled
    dfo$mean=mean(r)
    dfo$sd=sd(r)
    dfo$meantxt=as.character(dfo$mean)
    dfo$aantalbovendetectiegrens=aantalbovendetectiegrens
    dfo$allobs=allobs
  } 
  output=dfo[order(as.numeric(dfo$rij)),]
  row.names(output)=NULL
  return(output)
}

MaakKolomMeth<-function(metveldgemiddelden=dataframeuitLeesData,celcius=celcius,add_bicarbonate=add_bicarbonate,add_phosphate=add_phosphate){
  #' voorbereiding van methoden kolom voor ec25 berekening volgens Stuyfzand 
  # add_bicarbonate
  #   matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3')

  zm=as.data.frame(metveldgemiddelden)
# 
  # zm=dr
  # celcius=25
  # add_bicarbonate=TRUE
  # add_phosphate=FALSE
#   
  if (!'xhv' %in% colnames(zm)){
    zm$xhv=NA
  }
  
  if (!'xhco3' %in% colnames(zm)){
    zm$xhco3=NA
  }
  if (!'xco3' %in% colnames(zm)){
    zm$xco3=NA
  }
  if (!'xhco3v' %in% colnames(zm)){
    zm$xhco3v=NA
  }
  
  if (!'xpo4' %in% colnames(zm)){
    zm$xpo4=NA
  }
  # in xhco3 stoppen we het gemiddelde van xhco3 en xhco3v
  # met de NAs niet meegenomen dus meer kans op een meetwaarde
  # zm[,'xhco3zot']=rowMeans(zm[c('xhco3','xhco3v')], na.rm = TRUE)
  
  # alle NAs op nul zetten behalve pH en xhco3v
  
  zm[is.na(zm$xhco3)&!is.na(zm$xhco3v),'xhco3e']=zm[is.na(zm$xhco3)&!is.na(zm$xhco3v),'xhco3v']
  zm[!is.na(zm$xhco3)&is.na(zm$xhco3v),'xhco3e']=zm[!is.na(zm$xhco3)&is.na(zm$xhco3v),'xhco3']
  zm[is.na(zm$xhco3)&is.na(zm$xhco3v),'xhco3e']=0
  myrows=!is.na(zm$xhco3)&!is.na(zm$xhco3v)
  zm[myrows,'xhco3e']=(zm[myrows,'xhco3']+zm[myrows,'xhco3'])/2
  
  
  
  
  zm[is.na(zm$xpo4),'xpo4']=0
  zm[is.na(zm$xcl),'xcl']=0
  zm[is.na(zm$xso4),'xso4']=0
  zm[is.na(zm$xno3),'xno3']=0
  zm[is.na(zm$xco3),'xco3']=0
  zm[is.na(zm$xna),'xna']=0
  zm[is.na(zm$xk),'xk']=0
  zm[is.na(zm$xca),'xca']=0
  zm[is.na(zm$xmg),'xmg']=0
  zm[is.na(zm$xnh4),'xnh4']=0
  zm[is.na(zm$xfe),'xfe']=0
  zm[is.na(zm$xmn),'xmn']=0
  zm[is.na(zm$xal),'xal']=0
  zm[is.na(zm$xzn),'xzn']=0
  #  wanneer je niks weet is de pH 7
  zm[is.na(zm$xhv),'xhv']=7
  NA%in%zm
  
  # een invuldataframe maken voor de berekeningen
  z=data.frame(matrix(ncol=0,nrow=length(zm[,1])))
  row.names(z)=row.names(zm)
  # colnames(z)=colnames(zm)
  # de xhco3e is het beste gemiddelde van de hco3 metingen en heeft NA=0
  z$hco3=zm$xhco3e/61
  z$xhco3e=zm$xhco3e
  z$cl=zm$xcl/35.453
  z$so4=2*zm$xso4/96.062
  # bij Herman Prins was de invoer in mg N-nitraat in de platte matrix in mg nitraat
  z$no3=zm$xno3/62
  z$co3=2*zm$xco3/60.02
  #  pH omrekenen naar mili-equivalent dus maal 1000
  z$h3o=1000*10^-zm$xhv
  z$na=zm$xna/22.9898
  z$k=zm$xk/39.102
  z$ca=2*zm$xca/40.08
  z$mg=2*zm$xmg/24.31
  # nh4 hier ook als mg nh4 en niet als mg N
  z$nh4=zm$xnh4/18
  z$fe=2*zm$xfe/55.85
  z$mn=2*zm$xmn/54.94
  # al is in microgram
  z$al=0.003 * zm$xal/26.98
  z$zn=0.002*zm$xzn/65.39
  
  z$po4=3*zm$xpo4/30.97
  # z bevat geen NAs in plaats daarvan nullen
  
  # nu staan er nog nullen in z$po4
  if (add_phosphate){
    # als z$po4=0 dan gebruiken we zm$xptot
    z[z$po4==0,'po4']=3*zm[zm$xpo4==0,'xptot']/30.97
  }
  
  # alles omgezet van zm naar z behalve xecv
  
  #  ionbalans
  z$pos=z$al+z$ca+0.6*z$fe+z$k+z$mg+z$mn+z$nh4+z$na+z$zn+z$h3o
  z$neg=z$cl+z$hco3+z$no3+z$so4+z$co3+z$po4
  # wanneer overal nullen staan dan wordt de pH 7 en z$h3o = 0.0001
  z$ib=100*(z$pos-z$neg)/(z$pos+z$neg)
  # de gemiddelde temperatuur tijdens veldmetingen is 12 graden celcius
  #  maar xecv wordt omgerekend naar 25 graden celcius
  
  TK=273.15+celcius
  z$oh=(1000*10^(6.0875-0.01706*TK-4470.99/TK))/z$h3o

  # als er geen xhco3 is dan schatten we hco3 uit de ionbalans
  if (add_bicarbonate){
    z$san  = z$cl   + z$hco3 + z$so4 + z$no3 + z$co3 + z$oh
    z$skat = z$h3o  + z$na   + z$k   + z$ca  + z$mg  + z$nh4 + z$fe + z$mn
    myrows=row.names(z[z$skat>z$san&z$hco3==0&!is.na(z$skat)&!is.na(z$san)&!is.na(z$hco3),])
    z[myrows,'hco3']<-z[myrows,'skat']-z[myrows,'san']
  }
  
  NA%in%z
  
  z$mu=0.0005*(z$cl+z$hco3+z$no3+z$oh+z$h3o+z$na+z$k+z$nh4+2*z$so4+2*z$co3+2*z$ca+2*z$mg+2*z$fe+2*z$mn+2.55*z$al)
  # z$mu=0.5*(z$san+z$so4+z$co3+z$skat+z$ca+z$mg+z$fe+z$mn)/1000 uit Taat script 1990 is net even anders
  z$sqmu=sqrt(z$mu)
  z$gam2 = 10^(-2*(z$sqmu/(z$sqmu+1)-0.3*z$mu))
  z$san  = z$cl   + z$hco3 + z$so4 + z$no3 + z$co3 + z$oh
  z$skat = z$h3o  + z$na   + z$k   + z$ca  + z$mg  + z$nh4 + z$fe + z$mn
  z$alM  = z$al /3000
  z$so4M = z$so4/2000
  z$DO   = ((z$gam2^-3)*(10^-3.02)+z$alM+z$so4M)/2
  z$tmp=z$DO^2-z$alM*z$so4M
  z$also4= z$DO-sqrt(z$tmp)
  z$alF=z$alM-z$also4
  z$aloh=z$gam2^1.25*z$alF*z$oh*10^9.03
  z$aloh2=(z$gam2^2)*z$alF*(z$oh^2)*(10^18.7)
  z$alZ=(3*z$alF-z$aloh-2*z$aloh2+z$also4)*1.000
  z$san2  = z$cl   + z$hco3 + z$so4 + z$no3 + z$co3 - z$also4
  z$skat2 = z$h3o  + z$na   + z$k   + z$ca  + z$mg  + z$nh4 + z$fe + z$mn + z$alZ
  #   voor pH<5 geldt z$h3o>0.01 omdat het in mili-equivalent is
  z[na.omit(z$h3o>0.01),'san']=z[na.omit(z$h3o>0.01),'san2']
  z[na.omit(z$h3o>0.01),'skat']=z[na.omit(z$h3o>0.01),'skat2']
  z$sgem=(z$san+z$skat)/2
  z$rcl=z$cl/z$san
  
  z$rhco3=(z$hco3+z$co3)/z$san
  z$rso4=z$so4/z$san
  z$rno3=z$no3/z$san
  
  # q=z
  # z=q
  # NA%in%z$hco3
  # NA%in%z$sgem
  # NA%in%z$rso4
  # NA%in%z$rhco3
  # NaN%in%z$hco3
  # NaN%in%z$sgem
  # NaN%in%z$rso4
  # NaN%in%z$rhco3
  
  # keuze schattingsmethode
  # lukt alleen wanneer je sgem kan berekenen
  z$meth="dunlap"
  z$meth_="dunlap"
  myrows=z$sgem<600
  # myrows is een logical met TRUE FALSE en NA
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="logan"

  myrows=z$sgem<600&(z$rso4>=0.33|z$rhco3>=0.15)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="blanquet"

  myrows=z$sgem<100
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="blanquet"

  myrows=z$sgem<100&z$rno3>=0.15
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="mcneal"

  myrows=z$sgem<100&z$rcl>=0.67
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="logan"
  
  myrows=z$sgem<100&z$rhco3>=0.5
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="blanquet"
  
  myrows=z$sgem<100&(z$rso4>=0.33)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="mcneal"
  
  myrows=z$sgem<100&(z$rso4>=0.33&z$sgem<50)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="blanquet"
  
  myrows=z$sgem<20
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="rossum"
  
  myrows=z$sgem<20&(z$rso4>=0.33|z$rno3>=0.15)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="mcneal"
  
  myrows=z$sgem<4
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth"]="rossum"
  
  myrows=z$sgem<600
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="logan_2"
  
  myrows=z$sgem<600&(z$rso4>=0.33|z$rhco3>=0.15)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="blanquet_4"
  
  myrows=z$sgem<100
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="blanquet_3"
  
  myrows=z$sgem<100&z$rno3>=0.15
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="mcneal_3"
  
  myrows=z$sgem<100&z$rcl>=0.67
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="logan_1"
  
  myrows=z$sgem<100&z$rhco3>=0.5
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="blanquet_2"
  
  myrows=z$sgem<100&(z$rso4>=0.33)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="mcneal_2"
  
  myrows=z$sgem<100&(z$rso4>=0.33&z$sgem<50)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="blanquet_1"
  
  myrows=z$sgem<20
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="rossum_2"
  
  myrows=z$sgem<20&(z$rso4>=0.33|z$rno3>=0.15)
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="mcneal_1"
  
  myrows=z$sgem<4  
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]="rossum_1"
  
  myrows=z$san==0|z$skat==0
  myrows[is.na(myrows)]=FALSE
  z[myrows,"meth_"]='leeg'
  z[myrows,"meth"]='leeg'
  
  z$rk20=as.numeric(0)
  stuyfzandmethode=z
  return(stuyfzandmethode)
}
Blanquet<-function(z=dataframeuitMaakKolomMeth){
  #' Blanquet routine  voor ec25 berekening
  b=z[z$meth=='blanquet',]
  b$sqgem=sqrt(b$sgem)
  b$rlngem=log(b$sgem)
  # b$rlngem=log(b$sgem)/log(10)
  b$kblan=1.046*(107.73*b$cl+77.55*b$hco3+109.02*b$so4+20.97*b$k-b$sqgem*(1.452*b$cl+1.228*b$hco3+2.844*b$so4+0.112*b$k)+((6.1-0.9*b$sqgem)*b$cl+(6-2.067*b$sqgem)*b$hco3+(-3.1-7.274*b$rlngem)*b$so4)*b$ca/b$sgem+((-0.23-1.746*b$rlngem)*b$cl+(6.43-4.047*b$rlngem)*b$hco3+(-7.8-4.831*b$rlngem)*b$so4)*b$mg/b$sgem)
  #$kblan=1.046*(107.73*b$cl+77.55*b$hco3+109.02*b$so4+20.97*b$k-b$sqgem*(1.452*b$cl+1.228*b$hco3+2.844*b$so4+0.112*b$k)+((6.1-0.9*b$sqgem)*b$cl+(6-2.067*b$sqgem)*b$hco3+(-3.1-7.274*b$rlngem)*sb$o4)*b$ca/b$sgem+((-0.23-1.746*b$rlngem)*b$cl+(6.43-4.047*b$rlngem)*b$hco3+(-7.8-4.831*b$rlngem)*b$so4)*b$mg/b$sgem)
  b$rk20=b$kblan
  b[b$rhco3>=0.15&!is.na(b$rhco3),'rk20']=0.911*b[b$rhco3>=0.15&!is.na(b$rhco3),'kblan']+196
  b[b$rhco3>=0.5&!is.na(b$rhco3),'rk20']=0.98*b[b$rhco3>=0.5&!is.na(b$rhco3),'kblan']+33
  b[b$rso4>=0.33&!is.na(b$rso4),'rk20']=0.8*b[b$rso4>=0.33&!is.na(b$rso4),'kblan']+309
  b[b$rso4>=0.33&b$sgem>=100&!is.na(b$rso4)&!is.na(b$sgem),'rk20']=1.02*b[b$rso4>=0.33&b$sgem>=100&!is.na(b$rso4)&!is.na(b$sgem),'kblan']-528
  z[z$meth=='blanquet','rk20']=b$rk20
  rk20uitBlanquetIndataframeuitMaakKolomMeth=z
  return(rk20uitBlanquetIndataframeuitMaakKolomMeth)
}
Dunlap<-function(z=dataframeuitMaakKolomMeth){
  #' Dunlap routine  voor ec25 berekening
  b=z[z$meth=='dunlap',]
  b$A=35.35*b$cl+16.48*b$hco3+24.02*b$so4+75.63*b$co3+(b$na+b$k)*22.99+19.04*b$ca+24.3*b$mg
  b$B=4.3*10^-4*(log(b$A))^7.888
  b$F=0.948+1.503*10^-6*b$B
  b[b$B<10-4,'F']=1.101-3.252*10^-5*b[b$B<10-4,'B']
  b$kdun=b$F*b$B
  b$KDUN=7.456*b$kdun^0.8198
  z[z$meth=='dunlap','rk20']=b$KDUN
  rk20uitDunlapIndataframeuitMaakKolomMeth=z
  return(rk20uitDunlapIndataframeuitMaakKolomMeth)  
}
Logan<-function(z=dataframeuitMaakKolomMeth){
  #' logan routine  voor ec25 berekening
  b=z[z$meth=='logan',]
  b$klogan=(222.28*b$sgem)^0.9058
  b$rk20=b$klogan-30
  b[b$sgem>100&!is.na(b$sgem),"rk20"]=1.002*b[b$sgem>100&!is.na(b$sgem),"klogan"]-83
  z[z$meth=='logan','rk20']=b$rk20
  rk20uitLoganIndataframeuitMaakKolomMeth=z
  return(rk20uitLoganIndataframeuitMaakKolomMeth)  
}
Rossum<-function(z=dataframeuitMaakKolomMeth){
  #' Rossum volgens Ec_voor_Patrick.docx  voor ec25 berekening
  b=z[z$meth=="rossum",]
  # van H+ naar milliequivalent
  # b$h3o=b$h
  b$al=0
  b$g0an =86*b$co3+44.5*b$hco3+79.8*b$so4+76.3*b$cl+71.4*b$no3
  b$g0kat=59.5*b$ca+53.1*b$mg+50.1*b$na+73.5*b$k+349*b$h3o+73.5*b$nh4+54*b$fe+78*b$al
  b$zan =(4*(b$co3+b$so4)+b$cl+b$hco3+b$no3)  /(2*(b$co3+b$so4)+b$cl+b$hco3+b$no3)
  # Aanname 1.55*al
  b$zkat=(4*(b$ca+b$mg+b$fe+1.55*b$al)+b$na+b$k+b$h3o+b$nh4)/(2*(b$ca+b$mg+b$fe+1.55*b$al)+b$na+b$k+b$h3o+b$nh4)
  b$gaman =b$g0an /b$san
  b$gamkat=b$g0kat/b$skat
  b$q=b$zan*b$zkat*(b$gaman+b$gamkat)/((b$zan+b$zkat)*(b$zkat*b$gaman+b$zan*b$gamkat))
  b$kross=0.885*(b$g0kat+b$g0an-((b$gaman+b$gamkat)*b$zan*b$zkat*2*b$q/(115.2*(b$zan+b$zkat)*(1+sqrt(b$q)))+0.668)*((b$zan+b$zkat)*b$sgem)^1.5)
  b$KROSS=b$kross
  b[b$rcl>=0.67&!is.na(b$rcl),'KROSS']=1.0003*b[b$rcl>=0.67&!is.na(b$rcl),'kross']-2
  b[b$rso4>=0.33&!is.na(b$rso4),'KROSS']=0.989*b[b$rso4>=0.33&!is.na(b$rso4),'kross']
  b[b$rhco3>=0.67&!is.na(b$rhco3),'KROSS']=1.025*b[b$rhco3>=0.67&!is.na(b$rhco3),'kross']-8
  
  z[z$meth=='rossum','rk20']=b$KROSS
  rk20uitRossumIndataframeuitMaakKolomMeth=z
  return(rk20uitRossumIndataframeuitMaakKolomMeth)
}
McNeal<-function(z=dataframeuitMaakKolomMeth){
  #' McNeal routine voor ec25 berekening
  #McNeal volgens Ec_voor_Patrick.docx
  b=z[z$meth=="mcneal",]
  b$caT=b$ca/2000
  b$mgT=b$mg/2000
  b$so4T=b$so4/2000
  b$alfa=(b$gam2^2*204.174)^-1
  b$beta=(b$gam2^2*229.087)^-1
  b$caso4=500*(b$caT+b$so4T+b$alfa)-500*sqrt((b$caT+b$so4T+b$alfa)^2-4*b$caT*b$so4T)
  # caso4=500*(caT+so4T+alfa)-500*sqrt((caT+so4T+alfa)^2-4*caT*so4T)
  b$so4L=b$so4T-b$caso4/1000
  b$mgso4=500*(b$mgT+b$so4L+b$beta)-500*sqrt((b$mgT+b$so4L+b$beta)^2-4*b$mgT*b$so4L)
  # mgso4=500*(mgT+so4L+beta)-500*sqrt((mgT+so4L+beta)^2-4*mgT*so4L)
  b$caf=b$ca-2*b$caso4
  b$mgf=b$mg-2*b$mgso4
  b$so4f=b$so4-2*b$caso4-2*b$mgso4
  #methode 1 algemeen
  # 
  b$kmcneal=885*(0.0660*(b$cl+b$k)+0.0414*b$caf+0.0356*b$mgf+0.0452*b$na+0.0507*b$so4f+0.0470*b$co3+0.0348*b$hco3+0.0603*b$no3+0.0629*(b$caso4+b$mgso4)+(1/b$san)*(0.03*b$cl+0.029*b$hco3+0.077*b$so4f+0.034*b$no3+0.07*b$co3)+(1/b$skat)*(0.055*b$caf+0.06*b$mgf+0.023*b$na+0.03*b$k+0.183*(b$caso4+b$mgso4)))
  # kmcneal=885*(0.0660*(cl+k)+0.0414*caf+0.0356*mgf+0.0452*na+0.0507*so4f+0.0470*co3+0.0348*hco3+0.0603*no3+0.0629*(caso4+mgso4)+(1/san)*(0.03*cl+0.029*hco3+0.077*so4f+0.034*no3+0.07*co3)+(1/skat)*(0.055*caf+0.06*mgf+0.023*na+0.03*k+0.183*(caso4+mgso4)))
  b$KMCNEAL=0.964*b$kmcneal+8
  #  methode 1 speciaal geval 
  b[b$rso4>=0.33&!is.na(b$rso4),'KMCNEAL']=1.181*b[b$rso4>=0.33&!is.na(b$rso4),'kmcneal']-275
  b[b$rso4>=0.33&b$kmcneal<1100&!is.na(b$kmcneal)&!is.na(b$rso4),'KMCNEAL']=1.052*b[b$rso4>=0.33&b$kmcneal<1100&!is.na(b$kmcneal)&!is.na(b$rso4),'kmcneal']-45
  
  # b[b$rso4>=0.33&b$kmcneal<1100,'KMCNEAL']=1.052*b[b$rso4>=0.33&b$kmcneal<1100,'kmcneal']-45
  # methode 2 overschrijft methode 1
  b$kmcneal=885*(0.0620*(b$cl+b$k)+0.0355*b$caf+0.0269*b$mgf+0.0402*b$na+0.0407*b$so4f+0.0382*b$co3+0.0291*b$hco3+0.0528*b$no3+0.0492*(b$caso4+b$mgso4)+(1/b$san)*(0.23*b$cl+0.320*b$hco3+0.590*b$so4f+0.400*b$no3+0.51*b$co3)+(1/b$skat)*(0.260*b$caf+0.44*b$mgf+0.270*b$na+0.23*b$k+0.870*(b$caso4+b$mgso4)))
  # kmcneal=885*(0.0620*(cl+k)+0.0355*caf+0.0269*mgf+0.0402*na+0.0407*so4f+0.0382*co3+0.0291*hco3+0.0528*no3+0.0492*(caso4+mgso4)+(1/san)*(0.23*cl+0.320*hco3+0.590*so4f+0.400*no3+0.51*co3)+(1/skat)*(0.260*caf+0.44*mgf+0.270*na+0.23*k+0.870*(caso4+mgso4)))
  
  b[b$sgem>50&!is.na(b$sgem),'KMCNEAL']=0.953*b[b$sgem>50&!is.na(b$sgem),'kmcneal']+58
  z[z$meth=='mcneal','rk20']=b$KMCNEAL
  rk20uitMcNealIndataframeuitMaakKolomMeth=z
  return(rk20uitMcNealIndataframeuitMaakKolomMeth)
}

BerekenGeleidbaarheid<-function(metveldgemiddelden=metveldgemiddelden,celcius=25,add_bicarbonate = TRUE,add_phosphate=FALSE){
  #' nu de geleidbaarheid ec25 volgens Stuyfzand en de hco3 in mequivalent/liter berekenen
  #' Stuyfzand, P. (1987). 
  #' Een zeer nauwkeurige berekening van het elektrischgeleidingsvermogen, ter controle en aanvulling van wateranalyses: 
  #' 2e versie (SWE 87.006). Retrieved from Rijswijk: 

  #   matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3')

  # metveldgemiddelden=dr
  # celcius=25
  # add_bicarbonate=TRUE
  # add_phosphate=FALSE
  
  z=MaakKolomMeth(metveldgemiddelden=metveldgemiddelden,celcius=celcius,add_bicarbonate = add_bicarbonate,add_phosphate = add_phosphate)
  z=Blanquet(z)
  z=Logan(z)
  z=Dunlap(z)
  z=McNeal(z)
  z=Rossum(z)
  
  mvr=metveldgemiddelden[row.names(z),]
  h=cbind(mvr,z)
  
  # omrekenen naar 25 celcius en mS/m ipv uS/cm
  # temperatuur formule uit SWE 87-006
  h$ec25=0.10*h$rk20/(1-0.023*5)
  h$ec25xecv=h$ec25/h$xecv
  h$tienskatxecv=10*h$skat/h$xecv
  h$tienskatec25=10*h$skat/h$ec25
  
  
  # h is net zo lang als metgeleidbaarheid
  # alle afwezige en negatieve xecv eruit halen
  myrows=h$xecv>0&!h$meth=='leeg'&h$ec25>0&!h$ec25==Inf
  myrows[is.na(myrows)]=FALSE
  # k=h[myrows,]
  # h$pxecv=NA
  # h$pec25=NA
  # h$prinslabel=NA
  # h$ec25_xecv_sr=NA
  # 
  # x=log10(h[myrows,'ec25'])[3200:3300]
  # y=log10(h[myrows,'xecv'])[3200:3300]
  # q=cbind(x,y)
  # # mlm<-lm.fit(x,y)
  # mlm=lm(x~y)
  mlm<-lm(log10(h[myrows,'ec25'])~log10(h[myrows,'xecv']))
  h[myrows,'ec25_xecv_sr']=rstandard(mlm)
  
  # deze formule  komt van Herman Prins en klopt vrij nauwkeurig  drie standaardresiduen 
  # uit een logaritmische correlatie van xecv en ec25
  h[myrows,'pxecv']=2^-log10(h[myrows,'xecv'])
  h[myrows,'pec25']=2^-log10(h[myrows,'ec25'])
  h$prinslabel=(h$xecv*(1+h$pxecv)<h$ec25*(1-h$pec25))|(h$xecv*(1-h$pxecv)>h$ec25*(1+h$pec25))
  h$percentageverschil_xecv_ec25=100*(h$xecv-h$ec25)/h$ec25
  # metveldgemiddelden$po4=NA
  # metveldgemiddelden$hco3=NA
  # metveldgemiddelden$ec25=NA
  # metveldgemiddelden$prinslabel=NA
  # metveldgemiddelden$ec25_xecv_sr=NA
  # 
  # metveldgemiddelden[myrows,'po4']=h[myrows,'po4']
  # metveldgemiddelden[myrows,'hco3']=k$hco3
  # metveldgemiddelden[myrows,'ec25']=k$ec25
  # metveldgemiddelden[myrows,'prinslabel']=k$prinslabel
  # metveldgemiddelden[myrows,'ec25_xecv_sr']=k$ec25_xecv_sr
  metallegeleidbaarheid=h
  save(metallegeleidbaarheid,file='metallegeleidbaarheid.rda')
  mycols=c(names(metveldgemiddelden),'cl','so4','no3','na','k','ca','mg','po4','hco3','xhco3e','percentageverschil_xecv_ec25','ec25','prinslabel','ec25_xecv_sr')
  metgeleidbaarheid=h[,mycols]
  WriteListPointComma(metgeleidbaarheid,filename='metgeleidbaarheid.csv')
  save(metgeleidbaarheid,file='metgeleidbaarheid.rda')
  return(metgeleidbaarheid)
}

BerekenBalans<-function(mv=metgeleidbaarheid,pmrn='pmrn'){
  #' Berekent de N en P en ion balans en nog andere parameters
  # pmrn zijn de rijnamen uit de platte matrix
  # kolomnamen=c('Norg_N','Norgv_N','Ntot_N','NO3_N','NO3v_N'
  #              ,'NO3l_v','NH4_N','Porg','kation10','anion10','kation10.1v'
  #              ,'ionbalans','Na.Cl')
  
  mv=metgeleidbaarheid
  
  if  (!'xh3ov'%in%names(mv)&'xhv'%in%names(mv)){
    #  pH omrekenen naar mili-equivalent dus maal 1000
    mv$xh3ov=1000*10^-mv$xhv
  }
  
  # xh3ov is in mmol/liter
  # xal is in ug/liter
  mv$kation10 <- (10*(
    (2*(mv$xca/40.1))+
      (mv$xk/39.1)+
      (3*(mv$xal/26980))+
      (2*(mv$xmg/24.31))+
      (mv$xnh4/18)+
      (2*(mv$xfe/55.85))+
      (mv$xna/23) + 
      mv$xh3ov +
      (2*(mv$xmn/55))))
  mv$anion10 <- 10*(
    (mv$xcl/35.45)+
      2*(mv$xso4/96)+
      3*(mv$xpo4/31)+
      (mv$xno3/62))
  mv$kation10.1v<- (mv$kation10-mv$xecv)
  mv$kation10.1v_fractie<-mv$kation10.1v/mv$xecv
  # Je kunt de geleidbaarheid berekenen door het aantal kationen in millimol/liter met 10 te vermenigvuldigen. 
  # De verhouding kation10/xecv moet tussen de 1.25 en 0.75 schommelen. 
  # Wanneer het verschil tussen kation10 en xecv kleiner is dan 25 dan is het ook niet de moeite waard.
  # afwijkende EC's. Minimum 25 mS/mv
  # wanneer deze label waar is dan klopt of na, k, ca, mg niet of xecv
  mv$kation10.1v_label <-(mv$xecv>50&abs(mv$kation10.1v)>25&
                            ((mv$kation10/mv$xecv)>1.25)|((mv$kation10/mv$xecv)<0.75))
  
  # De ion balans is het verschil tussen de kationen en de anionen. 
  # Deze klopt niet wanneer er meer anionen zijn dan kationen.
  # Wanneer er meer kationen zijn dan anionen kan het prima kloppen 
  # omdat we immers geen bicarbonaat meten.
  mv$ionbalans<- mv$kation10-mv$anion10
  mv$ionbalans_fractie<-abs(mv$ionbalans/mv$kation10)
  mv$ionbalans_label<-mv$ionbalans<0.1&mv$ionbalans_fractie>0.25
  
  
  # De natriumchloridebalans neemt aan dat er equimolaire
  # hoeveelheden natrium en chloride in het water zitten.
  # Dit is het geval in zeewater en zout grondwater
  # het moet niet meer dan een factor drie verschillen
  mv$Na.Cl<- (mv$xna/22.99)/(mv$xcl/35.45)
  mv$Na.Cl_label<-(mv$Na.Cl>3|mv$Na.Cl<0.33)
  mv$NaCl_sr=NA
  
  # een betere maat is NaCl_sr dat het aantal standaarddeviaties geeft
  # dat natrium en chloride afwijken.
  # Dit moet tussen de drie en -3 liggen
  myrows=row.names(mv[mv$xna>0&mv$xcl>0&!is.na(mv$xna)&!is.na(mv$xcl),])
  nacl=lm(log10(mv[myrows,'xna'])~log10(mv[myrows,'xcl']))
  mv[myrows,'NaCl_sr']=rstandard(nacl)
  # plot(log10(mv[myrows,'xna'])~log10(mv[myrows,'xcl']))
  # abline(nacl$coefficients["(Intercept)"],nacl$coefficients[2],col="red")
  
  if ('xntot' %in% names(mv)&'xno3' %in% names(mv)){
    mv$Norgv_N<-mv$xntot-(mv$xno3v*14.00674/62.00494)-(mv$xnh4*14.00674/18.0385)
    mv$Norg_N<-mv$xntot-(mv$xno3*14.00674/62.00494)-(mv$xnh4*14.00674/18.0385)
    mv$Norg_N_fractie<-abs(mv$Norg_N/mv$xntot)
    mv$Norgv_N_fractie<-abs(mv$Norgv_N/mv$xntot)
    
    # er kan niet meer nitraat en ammonia stikstof in het water zitten dan er totaal stikstof in het water aanwezig is
    # het verschil wordt berekend als organisch stikstof en mag dus niet negatief zijn.
    # selecteer Norg waarden onder 0 mg N/l volgens mijn regels (Eke) met combi van Rens > 18% (omgezet naar 10%)
    # regels Herman Prins vm09-08-15.doc
    # Patrick heeft het label strenger gemaakt het wordt nu minder vaak toegekend
    # het was
    # mv$Norg_N_label<-(mv$Norg_N<0 & mv$Norg_N_fractie>0.1)|mv$Norg_N>10
    mv$Norg_N_label<-(mv$Norg_N<(-0.1) & mv$Norg_N_fractie>0.25)|mv$Norg_N>10
    mv$Norgv_N_label<-(mv$Norgv_N<(-0.1) & mv$Norgv_N_fractie>0.25)|mv$Norgv_N>10
  }else{
    mv$Norg_N<-NA
    mv$Norg_N_fractie<-NA
    mv$Norg_N_label=NA
    mv$Norgv_N<-NA
    mv$Norgv_N_fractie<-NA
    mv$Norgv_N_label=NA
    
  }
  
  # mv$Norgl_N<-mv$xntot-(mv$xno3*14.00674/62.00494)-(mv$xnh4*14.00674/18.0385)
  # mv$Norgl_N_fractie<-abs(mv$Norgl_N/mv$xntot)
  if ('xno3v' %in% names(mv)&'xno3' %in% names(mv)){
    mv$NO3_N<- (mv$xno3 * 14.00674 / 62.00494)
    mv$NO3v_N<- (mv$xno3v * 14.00674 / 62.00494)
    mv$veldminlab<-mv$NO3v_N-mv$NO3_N
    mv$NO3l_v<- (mv$xno3/mv$xno3v)
    # Selecteer monsters met een grote afwijking lab/veld (>25%) & >10 mg N/l
    # dat is abs(xno3-xno3v)<2.2
    # mv$NO3l_v_label<-((((mv$NO3_N-mv$NO3v_N)/mv$NO3_N)>0.25) & abs(mv$NO3v_N)>10)
    mv$NO3l_v_label<-(((abs(mv$xno3-mv$xno3v)/mv$xno3)>0.25) & abs(mv$veldminlab)>10)
    # een betere maat is NO3l_v_sr dat het aantal standaarddeviaties geeft
    # dat nitraatlab en veld afwijken.
    # Dit moet tussen de drie en -3 liggen
    # de detectiegrenzen zijn xno3 0.13 en xno3v 3
    mv$NO3l_v_sr=NA
    # myrows=row.names(mv[mv$xno3>(0.13*3)&mv$xno3v>(3*3)&!is.na(mv$xno3)&!is.na(mv$xno3v),])
    myrows=row.names(mv[mv$xno3>(0.13)&mv$xno3v>(3)&!is.na(mv$xno3)&!is.na(mv$xno3v),])
    # no3lv=lm(log10(mv[myrows,'xno3'])~log10(mv[myrows,'xno3v']))
    no3lv=lm(mv[myrows,'xno3']~mv[myrows,'xno3v'])
    # plot(no3lv)
    # plot(log10(mv[myrows,'xno3']),log10(mv[myrows,'xno3v']))
    # plot((mv[myrows,'xno3']),(mv[myrows,'xno3v']))
    # 
    mv[myrows,'NO3l_v_sr']=rstandard(no3lv)
    
  }else{
    mv$NO3v_N<- NA
    mv$veldminlab<-NA
    mv$NO3l_v<-NA
    mv$NO3l_v_label<-FALSE
    mv$NO3l_v_sr=NA
  }
  
  mv$NH4_N<- (mv$xnh4* 14.00674 / 18.0385)
  # let op xpo4 is uitgedrukt in mg P en niet in mg PO4
  mv$Porg<- (mv$xptot-mv$xpo4)
  mv$Porg_fractie<-abs(mv$Porg/mv$xptot)
  # Voor fosfor geldt hetzelfde als voor stikstof. 
  # Er kan niet meer fosfaat P in het water zitten 
  # dan er aan totaal fosfaat aanwezig is.
  # Dat wil zeggen Porg mag niet negatief zijn
  # totaal fosfaat - orthofosfaat vergelijking. Als detectielimiet voor PO4 0.013 gebruikt, voor Ptot 0.05
  # Porg kan wel heel groot zijn maar nooit negatief 
  # mv$Porg_label<-(((abs(mv$Porg)/mv$xptot)>0.1)&(abs(mv$Porg)>0.1))
  # Hier boven zat een fout in mv$Porg_label en  ik stel de gevoeligheid op minimaal 25% verschil
  mv$Porg_label<-(((abs(mv$Porg)/mv$xptot)>0.25)&(-mv$Porg>0.1))
  # het pmrn voorop zetten
  mv$PminP_tot=mv$xpo4-mv$xptot
  
  metbalans=mv[,c(pmrn,names(mv)[names(mv)!='pmrn'])]
  WriteListPointComma(metbalans,filename='metbalans.csv')
  save(metbalans,file='metbalans.rda')
  saveRDS(metbalans,'metbalans.rds')

  return(metbalans)
}

