
* Example Heckman selection models
* Lecture 9 November 2022

clear all
set more off
cap log close

* change to your current directory 
cd "C:\Users\...\"

* opens log file for collecting results
log using classexercises.log, replace

* open the dataset available on CANVAS
use mroz.dta

/*

the data contain information on American women with the purpose of understanding 
the determinants of their wages - e.g., the effect of education and experience

note that the sample contains 736 women, but only 411 are participating in the
labor market

*/

tab inlf						// 411 women in the labor force, 325 out
sum lwage, detail
tabstat lwage, by(inlf)			// lwage is missing for women with inlf = 0
hist lwage

mdesc lwage		// gives you the % of observations with missing values for lwage

* quick summary of the vars to be used in this exercise
d educ exper expersq nwifeinc age kidslt6 kidsge6
sum educ exper expersq nwifeinc age kidslt6 kidsge6

* are the differences between W in and out of the labor force significant?
local varlist "educ exper expersq nwifeinc age kidslt6 kidsge6"
foreach var of local varlist {
	describe `var'
	ttest `var', by(inlf)
}

* OLS in the selected sample
reg lwage educ exper expersq 


* Probit model of labor force participation <=> Heckman 1st step
probit inlf educ exper expersq nwifeinc age kidslt6 kidsge6

* estimating IMR manually
predict xbeta, xb
gen mills = normalden(xbeta) / normal(xbeta)
label var mills "Inverse Mills ratio"


* now add this new variable to the previous OLS:
reg lwage educ exper expersq mills

* Automatically instead, using Heckman 2step
heckman lwage educ exper expersq, select(inlf = educ exper expersq nwifeinc age kidslt6 kidsge6) twostep 

* Maximum Likelihood estimation
heckman lwage educ exper expersq, select(inlf = educ exper expersq nwifeinc age kidslt6 kidsge6)

* Still: check how reasonable are the exclusion restrictions. 
* aren't they related to the outcome variable at all?
reg lwage educ exper expersq nwifeinc age kidslt6 kidsge6
test nwifeinc age kidslt6 kidsge6 

* the coefficients are both individually and jointly insignificant
* so these exclusion restrictions seem appropriate

log close
