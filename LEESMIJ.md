# calculate conductivity

*Functies om geleidbaarheid grondwater te berekenen*

In dit project worden functies verzameld om geleidbaarheid (EC) te
berekenen in grondwater. Voor de berekening zijn gemeten concentraties
van cat/anionen nodig.

Dit project is zo opgezet dat de functies makkelijk in een bestaand of
nieuw R package overgenomen kunnen worden.

## Gerelateerde projecten

In dit project proberen we op basis van de functies uit de volgende
projecten tot een coherentie set functies te komen:

 * https://gitl01-int-p.rivm.nl/beelenvp/PatricksFuncties
 * https://gitl01-int-p.rivm.nl/spijkerj/krwqcprotocol-proto
 * https://gitl01-int-p.rivm.nl/meeringm/algemene-validatie-lmg

Het script geleidbaarheidstest.R berekent de geleidbaarheid met behulp van de methode van Stuyfzand in het rapport r:\Projecten\M350001_ondersteuning_mestbeleid_data\Patrick\calcgeleidbaarheid\SWE 87.006 Een zeer nauwkeurige berekening van het elektrisch geleidingsvermogen, ter controle en aanvulling van wateranalys.pdf
Uit dit rapport is tabel31 nagerekend. Deze tabel staat op r:\Projecten\M350001_ondersteuning_mestbeleid_data\Patrick\PatricksFuncties\StuyfzandTable31.csv
De gemeten geleidbaarheid in deze tabel was in µS/cm bij 20 °C en wordt omgerekend naar mS/m bij 25 °C omdat binnen het LMM deze geleidbaarheidparameter gebruikt wordt. De parameters staan beschreven in s:\Model&Applicatie\qBase\plattematrix_info_29102021.docx.
De functie BerekenGeleidbaarheid heeft de mogelijkheid om bicarbonaat te schatten uit de ionbalans zoals bij LMM gebruikelijk is omdat daar geen bicarbonaat gemeten wordt. De functie heeft ook de mogelijkheid om het fosfaatgehalte te schatten uit totaal fosfor zoals binnen het LMG gebruikelijk is. De geleidbaarheid wordt berekend bij 25 °C maar je kunt ook een andere temperatuur invoeren.

De geleidbaarheidberekeningen kunnen gebruikt worden voor de data controle. Wanneer de gemeten geleidbaarheid xecv sterk afwijkt van de berekende geleidbaarheid ec25 dan is er ergens iets niet in orde. Dat zal één of meerdere van de voor berekeningen gebruikte metingen xal, xca, xcl, xfe, xhv, xk, xmg, xmn, xna, xnh4, xno3, xpo4, xso4, xecv, xzn, xhco3 en xco3 zijn. Bij het LMM worden de laatste twee parameters niet gemeten en wordt xco3 op nul gezet en xhco3 geschat op grond van de ionbalans. Je berekent dan het aantal milli-equivalenten kationen en anionen. Het verschil tussen de kationen en de anionen wordt dan vermoedelijk veroorzaakt door xhco3. Wanneer er meer anionen zijn dan kationen dan klopt de ionbalans niet en is er weer een extra aanwijzing dat er iets mis is. Het script voor het berekenen van de geleidbaarheid is eenvoudig uit te voeren in standaard R.

source("/rivm/r/M350001_ondersteuning_mestbeleid_data/Patrick/PatricksFuncties/AlgemeneFunctiesPatrick.R")
uitvoer=BerekenGeleidbaarheid(metveldgemiddelden=invoer,celcius=25,add_bicarbonate = TRUE,add_phosphate=FALSE)

Een voorbeeld van het gebruik van de geleidbaarheids berekeningen bij de datacontrole staat op 
r:\Projecten\M350001_ondersteuning_mestbeleid_data\Patrick\calcgeleidbaarheid\geleidbaarheid.pdf
