clear all
set more off
set logtype text
cd "/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/"

* opens log file for collecting results
* cap log using 'stata-log/stata-current-log.txt', replace

use input/workshop-first-ex-cis3samp.dta, clear

********** MERGING **********
* Merge information from the register dataset
merge 1:1 id using input/workshop-first-ex-register.dta
tab _merge
drop _merge
* Merge information from the exercise3_extra_data dataset
merge 1:1 id using input/workshop-first-ex-exercise3_extra_data.dta
tab _merge
drop _merge

********** GENERATING KEY VARIABLES **********
* R&D Intensity = (Internal R&D investment)/(turnover)
gen rdint = xinterm/idbrturn
* in percent
gen rdintpct=rdint*100
* Export Intensity = (Exports volume)/(Turnover)
gen expint = export00/idbrturn
* Capital Expenditure Intensity = (Capital Intensity)/(Turnover)
gen capint = capex00/idbrturn
* Natual Log of the number of employees in the year 2000
gen lempl00 = log(employ00)

drop if expint > 1 
/*we drop observations that don't make sense, but not observations with missing data*/
drop if rdint > 1 & rdint != . 



reg prodnew extsource rdintpct inconst lempl00 
gen insample = e(sample) // This command creates a variable that indicates who belongs to the sample.
* We also want to make sure that we can remove the controls without missing valuable information.
