
* Workshop 7: Panel Data Models
* 2 Dec 2022

clear all
set more off
cap log close

* change to your current directory 
* cd "C:\..."

* opens log file for collecting results
log using workshop_paneldata.log, replace

* open the dataset (available on CANVAS)
use wagecps.dta

************************************
* Exercise 1 - describing the data *
************************************

describe
summarize

	** note that using "sum" with panel data combines the within & between
	** variation of each variable altogether. We cannot really see whether
	** the variables are time-invariant or not, or whether they vary a lot
	** across subjects (rather than within subjects)

browse 		// to have a look at the data structure
* Note that the data are organized in "long" format - ready for xt commands

list id t exp wks occ in 1/21 	// see how individuals are now observed over time

* declare the data to be a panel
xtset id t
order id t	// if we want to make these 2 vars the first ones appearing in the dataset

* Describe the overall structure of our panel data
xtdescribe
		* we have a perfectly balanced panel
		* every individual is observed every year, for 7y
		
*Summarize variables in terms of within vs between variation for key variables
xtsum lwage exp ed wks id t

		* "ed" does not change within individuals - time-invariant
		* this may be a drawback for some estimators
		* likewise, id does not change within individuals; the "t" variable in turn does not change between individuals
		* we can see that if we type "xtsum id t"
		* all the other variables vary both across individuals and over time for the same individual

* Plot wage against time
twoway scatter lwage t 		
		* not too revealing
		* variation in wages seems to increase over time
		* at each point in time it only says something about between variation 
		* i.e., across individuals
		* we cannot see anything about within variation (for the same individual)
		
xtline lwage if id < 10
xtline lwage if id < 10, overlay

		* these are more appropriate to see both within and between variation in 
		* wages. We see that most individuals increase their wage over time, but
		* also there are great differences between individuals - individual hete-
		* rogeneity may be significant in this setting


***************************
* Exercise 2 - Pooled OLS *
***************************

* pooled OLS with (incorrect) default standard errors
reg lwage ed exp exp2 wks

* pooled OLS with clustered standard errors
reg lwage ed exp exp2 wks, vce(cluster id)
estimates store pooledOLS
        * While in the normal OLS Stata does not know that the different 
		* observations pertains to the same individuals, by clustering the SE at
		* the individual level, we get closer to do this (even if it is not the 
		* best solution yet). By clustering the s.e. we are acknowledging that 
		* some observations are correlated – in this case, observations pertaining 
		* to the same individual. This way, unobservable characteristics that 
		* pertain to the same individual (e.g. ability, personality) and that 
		* could be correlated with observables are partially accounted for – but 
		* this is far from perfect. This way, observations pertaining to the same 
		* person are “grouped” and Stata adjusts for this correlation within that 
		* same “cluster” – here, the individual. 

**********************************
* Exercise 3 - Within estimation *
**********************************

* data transformation - manually:

* generate individual averages 
egen mlwage = mean(lwage), by(id)
egen mexp = mean(exp), by(id)
egen mexp2 = mean(exp2), by(id)
egen mwks = mean(wks), by(id)
egen med = mean(ed), by(id)

* generate deviations from the individual averages
gen dmlwage = lwage-mlwage
gen dmexp = exp-mexp
gen dmexp2 = exp2-mexp2
gen dmwks = wks-mwks
gen dmed = ed-med


/* we could do it more efficiently with a loop 
(both means and deviations from means), as follows

foreach var of varlist lwage exp exp2 wks ed {
egen m`var' = mean(`var'), by(id) 
gen dm`var' = `var' - m`var'
} 

check our previous example in class
*/

sort id t
list id t lwage mlwage dmlwage in 1/21, sep(7)


* Within regression manually - run OLS on the demeaned data:
reg dmlwage dmed dmexp dmexp2 dmwks 

* Within (or FE) estimator 
xtreg lwage ed exp exp2 wks, fe

	** note that results are practically the same, except for the constant term
	** in the FE model, the constant term corresponds to the average of all
	** individual FE. In the OLS run on the demeaned data, it has no real meaning
	
	** be aware of the different s.e. - why? different degrees of freedom in 
	** each model - due to the use of dummy variables for each individual in FE
	
	** note also the missing coefficients for time-invariant variables
	** we lose these variables if we use FE, since there is not within variation
	** to identify those coefficients
	
	** The F-test checks whether all fixed effects are insignificant versus the
	** alternative that at least one of the individual fe is significantly != 0
/* 
Rho: indicates the proportion of the variance that is explained by individual 
specific effects 
sigma_u: sd of the individual's fixed effect (a_i in the slides)
sigma_e: sd of the transitory shocks (u_i in the slides) */

estimates store within

* run LSDV instead, using individual dummies
*set matsize 800	/// probably no longer necessary in the latest versions of Stata	
reg lwage exp exp2 wks i.id  
	
// i.id creates automatically dummies for each N-1 individual
// Education is not in the model as it does not vary within the individual
// Note we now get the same standard errors as in the "automatic" FE model.


*******************************
* Exercise 4 - Random Effects *
*******************************
xtreg lwage ed exp exp2 wks, re
estimates store randeff


**********************************
* Exercise 5 - Comparing Results *
**********************************
estimates table pooledOLS within randeff
* nicer table (install esttab - if you don't have it)
esttab pooledOLS within randeff, replace cells(b(star fmt(3)) se(par fmt(3))) r2(3) ///
	star(* 0.10 ** 0.05 *** 0.01) mtitles("OLS" "FE" "RE")

* Hausman test
hausman within randeff
//H0 is equivalent to "RE estimator is appropriate" (i.e., no correlation between 
// individual effects and the other X variables) -- we reject it, FE is preferred

* re-run FE, including interactions between education & time
xtreg lwage c.ed##i.t exp exp2 wks, fe


	* we actually see that the return to education had increased over time
	* i.e., the return to education in years 3-7 seems to be about 1 pp higher
	* than in year 1.
	* however, note that we cannot estimate the return to education per se, only
	* whether the return to education has been changing over time (relative rather
	* than absolute return)
	

********************************************
* Exercise 6 - First differences estimator *
********************************************
* Generate first-differences 
sort id t
gen Dlwage = D.lwage
gen Dexp = D.exp
gen Dexp2 = D.exp2
gen Dwks = D.wks
gen Ded = D.ed

* run OLS on the transformed data
reg Dlwage Ded Dexp Dexp2 Dwks

* or use D. operator and obtain the same estimates automatically
reg D.lwage D.ed D.exp D.exp2 D.wks

	// note that the nr of observations drops because we lose one year of info.
	// because we need to calculate the differences between adjacent years for
	// all variables, the difference in education and experience becomes constant
	// 0 and 1, respectively (because education does not change and experience
	// always increases by 1 every year). The variables are dropped due to collinearity


log close

