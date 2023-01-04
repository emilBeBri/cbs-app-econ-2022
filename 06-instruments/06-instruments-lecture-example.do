
* Example Instrumental Variables Estimation
* Lecture 23 November 2022


************ NOTE: You will need to install the following commands *************
************************** ivreg2 **** overid **** ivendog *********************
********* Type "ssc install ivreg2" & repeat for the other 2 commands***********

clear all
set more off
cap log close

* change to your current directory 
cd "C:\...\"

* opens log file for collecting results
log using classexercises.log, replace 	// just an example 

* open the dataset 
use mus06data.dta

/*

This dataset will be used to study some determinants of expenditures on prescribed
medications, using a sample of individuals older than 65 years old. 

ldrugexp will be the dependent variable: log of total out-of-pocket expenditures

Regressors (X's): 
- hi_empunion: dummy = 1 if the individual holds either employer or union-sponsored
health insurance
- totchr: # of chronic conditions/diseases
- age: age in years
- female: dummy = 1 for women
- blhisp: dummy = 1 if black or Hispanic
- linc: log of annual household income, in thousands of dollars"

*/

* quick descriptive stats: 
sum ldrugexp hi_empunion totchr age female blhisp linc

/*
Let's consider two potential instruments for hi_empunion (which is possibly endogenous)

ssiratio: ratio of an individual's social security income to the overall income
	      from all sources

lowincome: dummy = 1 if the individual is a low-income status person

why are they relevant instruments? they are both likely to be negatively correlated
with having supplementary insurance. In order to be valid, we need to assume they
can be omitted from the equation for ldrugexp, and argue that the direct role of income
is already captured by the regressor linc. 
*/

* check possible relevance of these instruments
sum ssiratio lowincome
pwcorr hi_empunion ssiratio lowincome, star (0.01)
reg hi_empunion ssiratio lowincome totchr age female blhisp linc, r
test ssiratio lowincome

	* pairwise correlations are statistically significant
	* F-test confirms they are jointly significant in the reduced form equation
	* note that we need to control for all the other exogeneous variables as well

	
******** IV estimation *********

* NOTE: for the sake of comparing the OLS model with the IV model below
* we will have to run those models without robust standard errors at some point
* because the Hausman test that needs to be run below cannot be used with robust s.e.
* However, remember that you should use robust s.e. as much as possible

	
* Use the IV estimator to obtain the effect of health insurance on expenditures
* first use only 1 instrument: ssiratio

* doing it manually: 

* reduced form eq: regress health insurance on all exogeneous vars and 1 instrument	
reg hi_empunion ssiratio totchr age female blhisp linc, r

* obtain the predicted values for hi_empunion
predict hi_hat, xb

* plug-in this new variable in the structural eqt for wage: 
reg ldrugexp hi_hat totchr age female blhisp linc, r

* we can also do it automatically with ivreg2 and get the same results
ivreg2 ldrugexp (hi_empunion = ssiratio) totchr age female blhisp linc, first 
estimates store IV_1inst    // store these estimates to run a Hausman test later

	// NOTE: you might want to use robust s.e. in the structural eqt
	// bcs IV estimates are less efficient than OLS!
	// but to perform the Hausman test, we need to run it without robust s.e. for a moment


* is there an endogeneity problem indeed?
* we could compare these estimates with OLS estimates (run them without robust option just to perform the Hausman test)
reg ldrugexp hi_empunion totchr age female blhisp linc
estimates store ols

hausman IV_1inst ols

	* we reject "H0: the difference is not systematic." 
	* the estimate for hi_empunion is clearly different (and not reasonable in OLS)
	* OLS estimator would actually indicate that people with insurance have more
	* expenses than people who don't have any extra insurance 
	* we also see other coefficients being quite different in OLS vs IV
	* this indicates an endogeneity problem
	
* we can explicitly test for the presence of endogeneity in hi_empunion this way:
ivreg2 ldrugexp (hi_empunion = ssiratio) totchr age female blhisp linc
ivendog	hi_empunion
	// note that we can only run this test if we don't use the "robust" option

* or: 
ivreg2 ldrugexp (hi_empunion = ssiratio) totchr age female blhisp linc, endog(hi_empunion)

	// in both cases, we reject the null hypothesis that hi_empunion is exogenous
	// so endogeneity is present


* or manually, using instructions of slide 20
* predict errors from the 1st stage, using 1 instrument
reg hi_empunion ssiratio totchr age female blhisp linc
predict hi_res, res

* add them to the second stage, where hi_empunion and other exog vars are included
reg ldrugexp hi_res hi_empunion  totchr age female blhisp linc, r

	// the residual is significant, so endogeneity is a present and must be solved


* using 2 instruments allows to test for overidentifying restrictions
* are the instruments valid? - follow test from slide 22

* 1st predict the residuals from the 2SLS regression, using both instruments
ivreg2 ldrugexp (hi_empunion = ssiratio lowincome) totchr age female blhisp linc
predict uhat, res

* regress those residuals on all exogenous vars, including the TWO instruments
reg uhat ssiratio lowincome totchr age female blhisp linc

* obtain the test statistic: 
scalar overidtest = e(r2)*e(N)
di overidtest

* and compare with the reference value of a Chi-Square distribution with 
* 1 degree of freedom: 3.841 for 5% level of significance. Our statistic is way
* larger than that, so we reject the null hypothesis that all instruments are 
* valid and uncorrelated with u (the structural error).
* This means that at least one instrument is not valid, i.e. one at least is
* correlated with the structural error term. 

* we can do the test automatically as follows: 

ivreg2 ldrugexp (hi_empunion = ssiratio lowincome) totchr age female blhisp linc
overid

	// we clearly reject HO, so we conclude that at least one instrument is not valid
	// note that the results are exactly the same (the test statistic if we do not use robust s.e.)
	// if we do, the test statistic is slightly different but the final conclusion is the same
	
	log close
	clear










