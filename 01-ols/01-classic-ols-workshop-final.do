clear all
set more off
set logtype text
* single-line comment
* this points stata to the directory where your data files are found 
cd "/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/"

* opens log file for collecting results
* cap log using 'stata-log/stata-current-log.txt', replace

use workshop-01/cis3samp.dta, clear

********** MERGING **********
* Merge information from the register dataset
merge 1:1 id using workshop-01/register.dta
tab _merge
drop _merge
* Merge information from the exercise3_extra_data dataset
merge 1:1 id using workshop-01/exercise3_extra_data.dta
tab _merge
drop _merge

********** GENERATING KEY VARIABLES **********
* R&D Intensity = (Internal R&D investment)/(turnover)
gen rdint = xinterm/idbrturn
* Export Intensity = (Exports volume)/(Turnover)
gen expint = export00/idbrturn
* Capital Expenditure Intensity = (Capital Intensity)/(Turnover)
gen capint = capex00/idbrturn
* Natual Log of the number of employees in the year 2000
gen lempl00 = log(employ00)

drop if expint > 1 
/*we drop observations that don't make sense, but not observations with missing data*/
drop if rdint > 1 & rdint != . 


********** Some descriptives **********
count
su prodnew extsource rdint, detail

pwcorr prodnew extsource rdint inconst lempl00
pwcorr prodnew extsource rdint inconst lempl00, star(0.01)

pwcorr prodnew extsource rdint inconst lempl00


pwcorr prodnew extsource rdint, star(0.01)


hist prodnew
hist extsource
hist rdint

twoway scatter prodnew extsource
twoway scatter prodnew rdint
twoway scatter prodnew lempl00
twoway scatter rdint lempl00


********** Regression w/o interactions **********
reg prodnew extsource rdint inconst lempl00 
*Making it more readable:
gen rdintpct=rdint*100
reg prodnew extsource rdintpct inconst lempl00 
*Now it's easier to see the significant effect if R&D intensity - as we increase the R&D intensity by 1pp, the share of turnover from new products will increase by 0.82pp.
*An increase by one std deviation of extsource will increase prodnew by 1.78pp - but it's not significant at the 5% level.
*The effects of size (and internal constraints) are not significant.


//F-tests for individual significance of each variable
test rdint 
test extsource
test lempl00

log close



