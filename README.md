# groundwaterconductivity

*Calculate the electric conductivity of water from the ionic composition*

## Useful for water monitoring programs
The R script calculates the electric conductivity of water at 25°C from the ionic composition of the water. The comparison of the calculated and the measured electric conductivity is useful for quality control of groundwater and surface water monitoring programs. The user must make an input file with the proper names and dimensions of the measured cations and anions. The script reads the input file and produces an output file with the calculated conductivity using the formulas described by a Dutch report by Stuyfzand, P. (1987). Een zeer nauwkeurige berekening van het elektrisch geleidingsvermogen, ter controle en aanvulling van wateranalyses: 2e versie. Rijswijk, KIWA. This report selects an optimal calculation method for each individual set of measured ions. It allows an accurate calculation of the conductivity for a wide range of different groundwater types. The method was compared with a well-known publication of McCleskey, R. B., et al. (2012). "A new method of calculating electrical conductivity with applications to natural waters." Geochimica et cosmochimica acta 77: 369-382. The Dutch report gave accurate results over a much wider range of different groundwater types.

## Requirements
You will need a data frame with the concentrations of a number of ions. See for example data/StuijzandTable31.csv
The calculations can use calcium, chloride, iron, potassium, magnesium, manganese, sodium, ammonium, nitrate, phosphate, zinc, bicarbonate, carbonate in milligrams/liter. 
Aluminum is used in microgram/liter and the pH is used as such. When data are missing they are assumed to be zero except for the pH which is assumed to be seven. Bicarbonate is in equilibrium with carbon dioxide which can escape from the water sample. Therefore bicarbonate is not easy to measure and sometimes it is actually not measured. In that case the script can estimate bicarbonate from the ion balance of the cations and anions. When there is an excess of cations one can assume that bicarbonate was present in the original sample to complete the ion balance having equal amounts of cations and anions. The script can accept a number of input styles. When bicarbonate is never measured it will be estimated. In some monitoring programs phosphate is not measured while total phosphorus is used instead. This can be appropriate when most of the total phosphorus is present as phosphate. When phosphate is never measured in a monitoring program it will be estimated from total phosphorus. 

## Input styles
As for now the following input styles are supported: KRWQCinput, Stuyfzand, broadLMM, broadLGW. To check whether your input file matches the expectations of the selected input style an intermediate standard input file is produced named 
StuyfzandTable31_Stuyfzandstyle_LMM_broad_input_dataframe.rds for the input file StuyfzandTable31.csv and the Stuyfzandstyle input style. 
Note that the elements are preceded with an x and are expressed in milligrams/liter except for xal which is expressed in microgram/liter, xpo4 which is expressed in milligrams P/liter. xhv is the pH and xecv is the measured electric conductivity at 25°C expressed in mSiemens/meter. This was manually calculated from the k20 measured correcting for the 5°C temperature difference.

## Output
The file StuyfzandTable31_Stuyfzand_LMM_broad_output_dataframe.rds generates the complete output of the calculations. Most users appreciate a more tailored output generated by the proper output style. For example StuyfzandTable31_Stuyfzandstyle.csv. This small file is easily readable with Microsoft Excel. The last column shows the percentual difference between the measured and the calculated conductivity at 25°C. It ranges from -7.74 till 1.35%. 

## Interpretation
When the calculated conductivity deviates strongly from the measured conductivity something might be wrong. It can be the measured conductivity or some of the contributing cations or anions. When also the ion balance is off it becomes more probable that there is a mistake in the measurement of cations or anions. In addition one can look at previous measurements at the same location which can also give a clue on which parameter is causing the discrepancy between the calculated and the measured conductivity.
