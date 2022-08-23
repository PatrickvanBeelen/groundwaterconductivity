---
editor_options: 
  markdown: 
    wrap: 72
---

# groundwaterconductivity

*Calculate the electric conductivity of water from the ionic
composition*

## Useful for water monitoring programs

The R script calculates the electric conductivity of water at 25°C from
the ionic composition of the water. The comparison of the calculated and
the measured electric conductivity is useful for quality control of
groundwater and surface water monitoring programs. The user must make an
input file with the proper names and dimensions of the measured cations
and anions. The script reads the input file and produces an output file
with the calculated conductivity using the formulas described by a Dutch
report by Stuyfzand, P. (1987). Een zeer nauwkeurige berekening van het
elektrisch geleidingsvermogen, ter controle en aanvulling van
wateranalyses: 2e versie. Rijswijk, KIWA. This report selects an optimal
calculation method for each individual set of measured ions. It allows
an accurate calculation of the conductivity for a wide range of
different groundwater types. The method was compared with a well-known
publication of McCleskey, R. B., et al. (2012). "A new method of
calculating electrical conductivity with applications to natural
waters." Geochimica et cosmochimica acta 77: 369-382. The Dutch report
gave accurate results over a much wider range of different groundwater
types.

## Installation

You can install groundwaterconductivity from GitHub with:

```{r}
# install.packages("devtools")
devtools::install_github("PatrickvanBeelen/groundwaterconductivity")
```

## Usage

After installation you can load the library and start with the script.

```{r}
library(groundwaterconductivity)
```

If you don't know what to do, type calculate_conductivity() and the
script will generate the standard input data frame from the Stuyfzand
report table 3.1. It is saved in the package under
data/input_groundwaterconductivity.rda

```{r}
input_groundwaterconductivity <- calculate_conductivity()
save(input_groundwaterconductivity,file="input_groundwaterconductivity.rda")

my_Stuyfzand_output<-calculate_conductivity(inputfilename="input_groundwaterconductivity.rda",inputstyle="Stuyfzand",outputstyle="Stuyfzand",celcius=25)
my_minimal_output<-calculate_conductivity(inputfilename="input_groundwaterconductivity.rda",inputstyle="Stuyfzand",outputstyle="minimal",celcius=25)
```

## Requirements

You will need a dataframe with the concentrations of a number of ions.
See for example input_groundwaterconductivity.rda The calculations can
use calcium, chloride, iron, potassium, magnesium, manganese, sodium,
ammonium, nitrate, phosphate, zinc, bicarbonate, carbonate in
milligrams/liter. Aluminum is used in microgram/liter and the pH is used
as such. When data are missing they are assumed to be zero except for
the pH which is assumed to be seven. Bicarbonate is in equilibrium with
carbon dioxide which can escape from the water sample. Therefore
bicarbonate is not easy to measure and sometimes it is actually not
measured. In that case the script can estimate bicarbonate from the ion
balance of the cations and anions. When there is an excess of cations
one can assume that bicarbonate was present in the original sample to
complete the ion balance having equal amounts of cations and anions. The
script can accept a number of input styles. When bicarbonate is never
measured it will be estimated. In some monitoring programs phosphate is
not measured while total phosphorus is used instead. This can be
appropriate when most of the total phosphorus is present as phosphate.
When phosphate is never measured in a monitoring program it will be
estimated from total phosphorus.

## Input styles

As for now the following input styles and output styles are supported:
KRWQC, Stuyfzand, broadLMM, broadLGW and general. To check whether your
input file matches the expectations of the selected input style an
intermediate standard input file is produced named
input_groundwaterconductivity.rda. Note that the elements are preceded
with an x and are expressed in milligrams/liter except for xal which is
expressed in microgram/liter, xpo4 which is expressed in milligrams
P/liter. xhv is the pH and xecv is the measured electric conductivity at
25°C expressed in mSiemens/meter. This was calculated from the k20
measured correcting for the 5°C temperature difference.

## Output

If you have installed the package library(groundwaterconductivity) will
load the functions. If you run: '''{r}
myoutputdataframe\<-calculate_conductivity(inputfilename="input_groundwaterconductivity.rda",inputstyle
= "Stuyfzand",outputstyle = "minimal", celcius = 25) ''' The general
input file LMM_broad_input_groundwaterconductivity.rda will be
generated. The calculations will be performed and the general output
file with_all_calculated_conductivity.rda will be generated. The
outputstyle "minimal" will give you a selection of the output in
myoutputdataframe and in with_calculated_conductivity.rda. You can
replace LMM_broad_input_groundwaterconductivity.rda with your own data.

The with_calculated_conductivity.rda file generated by the Stuyfzand
output style has a number of column names:

-   "myrownames" your original row numbers from the input file can also
    be expressed as text

-   "method" a reference to the calculation method used for this
    specific sample

-   "ionbalance" the sum of the positive ions minus the sum of the
    negative ions divided by the sum of all ions

-   "k20_calc" the conductivity originally calculated by Stuyfzand at
    20°C in micro Siemens/centimeter

-   "rk20_calc" the conductivity at 20°C calculated by this R script in
    micro Siemens/centimeter

-   "ec25_calc" the conductivity at 25°C calculated by this R script in
    milli Siemens/meter

-   "xecv_measured" the measured conductivity by Stuyfzand converted to
    25°C in milli Siemens/meter

-   "percentage_xecv_ec25" the difference between the measured and
    calculated conductivity in percent of the calculated conductivity at
    25°C. It ranges from -7.74 till 1.35% in the Stuyfzand data.

-   "ec25_xecv_sr" the normalized difference of the logarithmically
    transformed measured and calculated conductivity. 95% of the
    measurements range between -2 and +2.

-   "skat_san_sr" the normalized difference between the logarithmically
    transformed sum of the cations and anions.

-   "kations meq/l" the sum of the cations H+, Na+, K+, Ca2+,Mg2+, NH4+,
    Fe2+, Mn2+

-   "anions meq/l" the sum of the anions Cl-, HCO3-, SO4-, NO3-, CO32-
    in charge milli-equivalents/liter "max_anion meq/l", the major
    negative ion in charge milli-equivalents/liter

-   "max_anion_name" "max_kation meq/l" the major positive ion in charge
    milli-equivalents/liter

-   "max_kation_name"

-   "suspect" When the calculated conductivity largely exceeds the
    measured one ("ec25_xecv_sr">2) it is possible that either the
    highest cation or the highest anion is actually much lower than the
    reported value. When there is a large excess of cations
    ("skat_san_sr">2) then the highest cation is suspect. When there is
    a large excess of anions ("skat_san_sr"\<(-2) ) the highest anion is
    suspect.

-   The last column shows Interpretation

# Interpretation

When the calculated conductivity deviates strongly from the measured
conductivity something might be wrong. It can be the measured
conductivity or some of the contributing cations or anions. When also
the ion balance is off it becomes more probable that there is a mistake
in the measurement of cations or anions. The script calculates the ion
balance and the cations and anions that contribute most to the
conductivity. When the calculated conductivity is much higher than the
measured one and the ion balance has an excess of anions then the
maximal anion is suspect to be a measurement error. This is indicated in
the output.

[The vignette](https://rpubs.com/PatrickvanBeelen/934472)
vignettes/groundwaterconductivity.Rmd gives more details.
