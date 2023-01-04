
* Example Panel Data
* Lecture 30 November 2022

clear all
set more off
cap log close

* change to your current directory 
cd  "C:\...\

* opens log file for collecting results
log using classexercises.log, replace

* open the dataset available on CANVAS
use nlswork.dta

* have a look at the data format - example
list idcode year ln_wage grade age ttl_exp in 1/24

* declare the data as a panel
xtset idcode year

* have a quick look at how the data look like
xtdescribe
xtsum idcode year ln_wage grade age ttl_exp

**** Estimations ****
* FE/Within estimator manually
* we want to regress wages on schooling, age & experience (and their square), 
* race, and area of residence

** we need to 1st compute the transformed variables
* we need to get the average of each variable for each individual - slide 23

* get the squared age and tenure:
gen age_sq = age*age
gen ten_sq = tenure*tenure

* create dummies for different races
tab race, gen(race)

local varlist "ln_wage grade age age_sq tenure ten_sq race1 race2 race3 not_smsa south"
foreach var of local varlist {
	egen av_`var'=mean(`var'), by(idcode)
	gen new_`var'=`var'-av_`var'
	
}
/*
we could do it manually as follows


egen av_ln_wage = mean(ln_wage), by(idcode)
gen new_wage = ln_wage-av_ln_wage

egen av_grade = mean(grade), by(idcode)
gen new_grade = grade-av_grade

...

*/

* Run an OLS on the demeaned data: 
reg new_ln_wage new_grade new_age new_age_sq new_tenure new_ten_sq new_race2 new_race3 new_not_smsa new_south

* compare with FE on the original data: 
xtreg ln_wage grade age age_sq tenure ten_sq race2 race3 not_smsa south, fe 
estimates store FE

	** note that results are practically the same, except for the constant term
	** in the FE model, the constant term corresponds to the average of all
	** indvidual FE. in the OLS run on the demeaned data, it has no real meaning
	
	** be aware of the different s.e. - why? different degrees of freedom in 
	** each model - due to the use of dummy variables for each individual in FE
	
	** note also the missing coefficients for time-invariant variables
	** we lose these variables if we use FE, since there is not within variation
	** to identify those coefficients

* run RE model instead
xtreg ln_wage grade age age_sq tenure ten_sq race2 race3 not_smsa south, re 
estimates store RE

* test the equality of the coefficients using Hausman Test
hausman FE RE

	* We reject HO, so the coefficients are not the same
	* this means that we reject the hypothesis that RE gives consistent estimates
	
* opt for a FE model and use robust SE

xtreg ln_wage grade age age_sq tenure ten_sq race2 race3 not_smsa south, fe r
	
	* Note that using clustered SE (clustered at the same level of the FE) here is the same as using robust SE
	* if you would use a pooled OLS model (i.e. no FE), you should use clustered SE instead of robust SE, so you account for the correlation between errors of the same "unit"

log close

