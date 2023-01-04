* clear all
* set more off
* set logtype text
* * single-line comment
* * this points stata to the directory where your data files are found 
* cd "/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course" 

* * opens log file for collecting results
* * cap log using statalog.txt, replace

* * use input/workshop-first-ex-cis3samp.dta, clear


do "stata/data-preprocessing-workshop-first-example-v01.do"


********** Some descriptives **********
count
su prodnew extsource rdintpct, detail
pwcorr prodnew extsource rdintpct inconst lempl00
hist prodnew
hist extsource
hist rdintpct
twoway scatter prodnew extsource
twoway scatter prodnew rdintpct
twoway scatter prodnew lempl00
twoway scatter rdintpct lempl00


********** Regression w/o interactions **********
reg prodnew extsource rdintpct inconst lempl00 
gen insample = e(sample) // This command creates a variable that indicates who belongs to the sample.
* We also want to make sure that we can remove the controls without missing valuable information.


//F-tests for significance of subset of variables
test inconst lempl00 // We cannot reject the null hypothesis that both controls are not signifcantly different from 0 at the same time.
// F-test for joint significance of the model as a whole.
test extsource rdintpct inconst lempl00 //significant!

pwcorr prodnew extsource rdintpct inconst lempl00 if insample==1 , star(0.05)

* Before we run the restricted model we need to run the full model. 
* Then we make sure that the sample in the restricted model is the same as in the full model.
* The reason is that, in order to compare both models correctly, they should be based on the same observations.
reg prodnew extsource rdintpct inconst lempl00 

// Restricted model (w/o the controls)
reg prodnew extsource rdintpct if insample==1
* We see that the extsource variable becomes less significant 
* This may be because it is absorbing part of the (negative) effect coming from size


// F-Test

* Let us now compare both models based on the F-test for restricted vs unrestricted models.
* We first need to run the unrestricted (full) model to obtain the SSRur
* Then we do the same for the restricted model 
* Finally we apply the formula as in Wooldridge (2012)

// Unrestricted model
reg prodnew extsource rdintpct inconst lempl00
// Sum of Squared Residuals of the Unrestricted Model (SSRur)
scal ssr_ur = e(rss)
// Restricted model
reg prodnew extsource rdintpct if insample
// Sum of Squared Residuals of the Restricted Model (SSRr)
scal ssr_r = e(rss)
// F-Test formula (as in Wooldridge, 2012)
scal F = ((ssr_r - ssr_ur) / 2) / (ssr_ur / (431-4-1))

display F // We get the same result as in the joint test for inconst and lempl00.


// Rerun full model before hettest
reg prodnew extsource rdintpct inconst lempl00

// Test for heteroskedasticity
hettest 
* There is heteroskedasticity (We reject the null of Homoskedasticity due to P smaller than 0.05)
* Let us run the regressions again while controlling for heteroskdasticity

// Full model (correcting for heteroskedasticity)
reg prodnew extsource rdintpct inconst lempl00 if insample, robust
// Joint significance test
test inconst lempl00 
// Restricted model (correcting for heteroskedasticity)
reg prodnew extsource rdintpct if insample, robust
* We now see that the coefficient of extsource becomes more significant again
* This is because, after controlling for heteroskedasticity, the standard error is now smaller
* We prefer these models because we are correcting for heteroskedasticity


	
********** Regression with interaction and centered variables **********
// Generating the interaction variables
gen rdintpct_extsource = rdintpct*extsource
reg prodnew extsource rdintpct rdintpct_extsource inconst lempl00,r

* we want to make sure that the following transformations are done on the estimation sample
keep if e(sample) == 1

// Centering around the mean (using the regression sample)
egen avgrdintpct = mean(rdintpct)
gen dm_rdintpct = rdintpct-avgrdintpct
egen avgextsource = mean(extsource)
gen dm_extsource = extsource-avgextsource
gen dm_rdintpct_extsource = dm_rdintpct*dm_extsource

// Regression with centered explanatory variables 
reg prodnew extsource rdintpct dm_rdintpct_extsource inconst lempl00, r
test dm_rdintpct_extsource

// using margins
// note the notation c. for continuous variables
// # denotes an interaction term
// ## produces the full set of main and interaction effects
reg prodnew c.rdintpct##c.extsource inconst lempl00, r
margins, dydx(extsource)
margins, dydx(extsource) atmeans
marginsplot
graph export "stata/test-stata.png", replace
margins, dydx(extsource) at(rdintpct = (0(2)70))
marginsplot
graph export "stata/test-stata.png", replace


/log
log close
