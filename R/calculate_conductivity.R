rm(list=ls())
graphics.off();
close.screen(all.screens=TRUE)# Inladen pakketten

library(tidyverse)
# save data in mytempdir
mytempdir=tempdir()

NA2zero <- function (x) {
  x[is.na(x)] <- 0
  x[is.nan(x)] <- 0
  return(x)
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


MaakKolomMeth<-function(LMM_broad_input_dataframe=LMM_broad_input_dataframe,celcius=celcius,add_bicarbonate=add_bicarbonate,add_phosphate=add_phosphate){
  #' voorbereiding van methoden kolom voor ec25 berekening volgens Stuyfzand 
  # add_bicarbonate
  #   matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3')

  zm=as.data.frame(LMM_broad_input_dataframe)
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
  z=data.frame(matrix(ncol=1,nrow=length(zm[,1])))
  row.names(z)=row.names(zm)
  names(z)="myrownames"
  z$myrownames=zm$myrownames
  # colnames(z)=colnames(zm)
  # de xhco3e is het beste gemiddelde van de hco3 metingen en heeft NA=0
  if ('xhco3e'%in%names(zm)){
    z$hco3=zm$xhco3e/61
  }
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
  dataframeuitMaakKolomMeth=z
  return(dataframeuitMaakKolomMeth)
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


#' Calculate the conductivity of groundwater from its ionic composition
#' @description
#' you will need a data frame with the concentrations of a number of ions
#' as shown in data/StuijzandTable31.csv
#' The calculations can use calcium, chloride, Iran, potassium, magnesium, manganese, sodium, ammonium, nitrate, phosphate, zinc, #' bicarbonate, carbonate in milligrams/liter. 
#' Aluminum is used in microgram/liter and the pH is used as such.
#' The conductivity is calculated at 25°C so the measured conductivity might need some temperature adjustment.
#' Missing data are assumed to be zero except a missing pH which will be assumed to be 7.
#' @example
#' Stuyfzandtest<-calculate_conductivity(inputfilename="data/StuyfzandTable31.csv",
#'  inputstyle='Stuyfzand',
#' outputstyle='Stuyfzandstyle',
#' celcius=25)
calculate_conductivity<-function(inputfilename="data/metingen.rda",inputstyle='KRWQCinput',outputstyle='KRWQCoutput',celcius=25){
  if(inputstyle=='KRWQCinput'){
    load(inputfilename)
    inputfile=metingen
    w=pivot_wider(s,id_cols = 'monsterid',names_from = 'parameter',values_from = 'waarde')
    mycols=c("monsterid", "al", "as", "ba", "ca", "cd", "cl", "cod_o2", 
             "cr", "cu", "doc_c", "ec", "eox", "f", "fe", "h", "hco3", "hg", 
             "k", "kmno4_o", "mg", "mn", "nh4_n", "no2_n", "no3_n", "na", 
             "ni", "ptot_p", "po4_p", "pb", "so4", "sio2_si", "sr", "toc_c", 
             "vox", "zn", "o2_1__veld", "o2_5__veld", "t_1__veld", "t_5__veld", 
             "ec_1__veld", "ec_5__veld", "h_1__veld", "h_5__veld", "sh_veld", 
             "rp_5__veld", "tg", "co3", "hco3_veld", "b", "ag", "be", "sb", 
             "co2", "br", "co", "li", "so3", "tl", "v", "kj_n", "mo")
    mycols=c("monsterid", "al","ca","cl", "ec", "fe", "h", "hco3_veld",
             "k", "mg", "mn", "nh4_n", "no3_n", "na", 
             "ptot_p", "so4", "zn", "co3", "hco3_veld")
    s=w[,mycols]
    s$myrownames=s$monsterid
    s=data.frame(s)
    matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3','myrownames')
    l=data.frame(matrix(nrow=length(s[,1]),ncol=length(matrixnamen),0)) 
    names(l)=matrixnamen
    l$xcl=s$cl
    l$xso4=s$so4
    l$xno3=62*s$no3_n/14
    l$xhv=s$h
    l$xna=s$na
    l$xk=s$k
    l$xca=s$ca
    l$xmg=s$mg
    l$xnh4=18*s$nh4_n/14
    l$xhco3=s$hco3
    l$xco3=s$co3
    # 
    l$xpo4=s$ptot_p
    # l$xecv=0.10*s$k20/(1-0.023*5)
    l$xecv=s$ec
    l$myrownames=s$myrownames
    # # omrekenen naar 25 celcius en mS/m ipv uS/cm
    # # temperatuur formule uit SWE 87-006
    # h$ec25=0.10*h$rk20/(1-0.023*5)
    LMM_broad_input_dataframe=l
    add_phosphate=FALSE
    add_bicarbonate=FALSE
  }
  
  
  if(inputstyle=='Stuyfzand'){
    # read the original inputfile and save with extra myrownames column
    input_dataframe=ReadListPointComma(inputfilename)
    myrownames=row.names(input_dataframe)
    s=cbind(input_dataframe,myrownames)
    matrixnamen=c('xal',"xca","xcl","xfe","xhv","xk","xmg","xmn","xna","xnh4","xno3",'xpo4',"xso4",'xecv','xzn','xhco3','xco3','myrownames')
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
    l$myrownames=s$myrownames
    # # omrekenen naar 25 celcius en mS/m ipv uS/cm
    # # temperatuur formule uit SWE 87-006
    # h$ec25=0.10*h$rk20/(1-0.023*5)
    
    LMM_broad_input_dataframe=l
    add_phosphate=FALSE
    add_bicarbonate=FALSE
    
    save(LMM_broad_input_dataframe,file = "data/originalinputfile.rds")
  }  
  z=MaakKolomMeth(LMM_broad_input_dataframe=LMM_broad_input_dataframe,celcius=celcius,add_bicarbonate = add_bicarbonate,add_phosphate = add_phosphate)
  z=Blanquet(z)
  z=Logan(z)
  z=Dunlap(z)
  z=McNeal(z)
  z=Rossum(z)
  
  h=left_join(s,z,by="myrownames",suffix=c(' mg/l',' meq/l'))
  # omrekenen naar 25 celcius en mS/m ipv uS/cm
  # temperatuur formule uit SWE 87-006
  h$ec25=0.10*h$rk20/(1-0.023*5)
  if (!'xecv'%in%names(h)){
    h$xecv=0.10*h$k20/(1-0.023*5)
    
  }
  # h$ec25xecv=h$ec25/h$xecv
  # h$tienskatxecv=10*h$skat/h$xecv
  # h$tienskatec25=10*h$skat/h$ec25
  
  
  # h is net zo lang als with_calculated_conductivity
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
  h$percentage_xecv_ec25=100*(h$xecv-h$ec25)/h$ec25
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
  with_all_calculated_conductivity=h
  
  if(outputstyle=='Stuyfzandstyle'){
    inputname=unlist(strsplit(inputfilename,split='.',fixed=T))
    newname=paste0(inputname[1],"_",outputstyle,".",inputname[2])
    rdsname=paste0(inputname[1],"_",outputstyle,"_full.rds")
    
    save(with_all_calculated_conductivity,file=rdsname)
    
    mycols=c("cl mg/l", "hco3 mg/l", "so4 mg/l", "no3 mg/l", "co3 mg/l", 
             "h", "na mg/l", "k mg/l", "ca mg/l", "mg mg/l", "nh4 mg/l", 
             "myrownames","meth_", "k20", "rk20", 
             "ec25", "xecv",  
             "percentage_xecv_ec25")
    
    with_calculated_conductivity=h[,mycols]
    # WriteListPointComma(with_calculated_conductivity,filename='with_calculated_conductivity.csv')
    write.csv(with_calculated_conductivity,file=newname)
    }
  
  
  
  if(outputstyle=='BroadLMMstyle'){
    
    save(with_all_calculated_conductivity,file='with_all_calculated_conductivity.rda')
    mycols=c(names(LMM_broad_input_dataframe),'cl','so4','no3','na','k','ca','mg','po4','hco3','xhco3e','percentageverschil_xecv_ec25','ec25','prinslabel','ec25_xecv_sr')
    with_calculated_conductivity=h[,mycols]
    # WriteListPointComma(with_calculated_conductivity,filename='with_calculated_conductivity.csv')
    save(with_calculated_conductivity,file='with_calculated_conductivity.rda')
  }
  
  
  return(with_calculated_conductivity)
}



Stuyfzandtest=calculate_conductivity(inputfilename="data/StuyfzandTable31.csv",inputstyle='Stuyfzand',outputstyle='Stuyfzandstyle',celcius=25)

Stuyfzandnamen=c("cl", "hco3", "so4", "no3", "co3", "h", "na", "k", "ca", "mg", 
                 "nh4", "k20")
Stuyfzandnamen[!Stuyfzandnamen=='h']

StuyfzandTable31 <- ReadListPointComma("data/StuyfzandTable31.csv")
load(file='data/metingen.rda')
input<-as.matrix(StuyfzandTable31)
typeof(input)

input[4,4]=NA
beschikbarenamen=input[names(input)%in%Stuyfzandnamen]
typeof(input)
output=NA2zero(as.data.frame(input))
if (length(beschikbarenamen)==12){
  replace_na(input,99)
}

