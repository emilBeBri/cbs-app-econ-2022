clear all
set more off
set logtype text
* single-line comment
* this points stata to the directory where your data files are found 
cd "C:\Users\hck.si\Dropbox (CBS)\Z-drev\Undervis\PhDeconometrics\2022\Workshop 3"
* opens log file for collecting results
cap log using statalog.txt, replace

use cis3samp.dta, clear

********** MERGING **********
* Merge information from the register dataset
merge 1:1 id using register.dta
tab _merge
drop _merge
* Merge information from the exercise3_extra_data dataset
merge 1:1 id using exercise3_extra_data.dta
tab _merge
drop _merge

********** GENERATING KEY VARIABLES **********
* R&D Intensity = (Internal R&D investment)/(turnover)
gen rdint = xinterm/idbrturn
gen rdintpct=rdint*100
* Export Intensity = (Exports volume)/(Turnover)
gen expint = export00/idbrturn
* Capital Expenditure Intensity = (Capital Intensity)/(Turnover)
gen capint = capex00/idbrturn
* Natual Log of the number of employees in the year 2000
gen lempl00 = log(employ00)

drop if expint > 1
drop if rdint > 1 & rdint != .

/* Note: define unic as: */ 
gen unic=0
replace unic=1 if punivl==1 | punivn==1 | punive==1 | punivu==1 | punivo==1 

* OLS for comparison
reg prodinov lempl00 i.unic, r 
rvfplot
predict yhatlin, xb
su yhatlin, de
count if yhatlin <=0 
* average partial effect (will just give you the coefficients back)
margins, dydx(*) 
* logit
* note the factor variable notation for i.unic
logit prodinov lempl00 i.unic
* marks the available sample
g insample = e(sample)
* average marginal effect
margins, dydx(*) 
* conditional marginal effect (at the average)
margins, dydx(*) atmeans

** repeat by "manual" computation

* average marginal effect
* evaluate partial effect at values for each firm
* employment (continuous)
g APE_empl = _b[lempl00]*exp(_b[_cons]+_b[lempl00]*lempl00+_b[1.unic]*1.unic)/(1+exp(_b[_cons]+_b[lempl00]*lempl00+_b[1.unic]*1.unic))^2 
* unic (discrete)
g APE_unic1 = exp(_b[_cons]+_b[lempl00]*lempl00+_b[1.unic]*1)/(1+exp(_b[_cons]+_b[lempl00]*lempl00+_b[1.unic]*1)) 
g APE_unic0 = exp(_b[_cons]+_b[lempl00]*lempl00+_b[1.unic]*0)/(1+exp(_b[_cons]+_b[lempl00]*lempl00+_b[1.unic]*0)) 
g APE_unic = APE_unic1 - APE_unic0
su APE_empl APE_unic

* partial effect at the average
* we need the means of the variables
egen emp_m = mean(lempl00) if insample
egen unic_m = mean(1.unic) if insample
* empl00ment (continuous)
* note this is just one number
scalar PEA_empl = _b[lempl00]*exp(_b[_cons]+_b[lempl00]*emp_m+_b[1.unic]*unic_m)/(1+exp(_b[_cons]+_b[lempl00]*emp_m+_b[1.unic]*unic_m))^2 
* unic (discrete)
* also just numbers
g PEA_unic1 = exp(_b[_cons]+_b[lempl00]*emp_m+_b[1.unic]*1)/(1+exp(_b[_cons]+_b[lempl00]*emp_m+_b[1.unic]*1))
g PEA_unic0 = exp(_b[_cons]+_b[lempl00]*emp_m+_b[1.unic]*0)/(1+exp(_b[_cons]+_b[lempl00]*emp_m+_b[1.unic]*0))
g PEA_unic = PEA_unic1 - PEA_unic0
disp PEA_empl 
disp PEA_unic

* Probit vs logit
logit prodinov lempl00 i.unic 
margins, dydx(*)
probit prodinov lempl00 i.unic
margins, dydx(*)


