clear all
set more off
set logtype text
cd "/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/"
use input/mroz-1.dta, clear
* Select only active/working participants
drop if inlf == 0
 
***************************************
* Exercise 2 - descriptive statistics *
***************************************

* Dependent Variable: wage
sum lwage, d
hist lwage, normal


* Endogenous Variable
* educ (years of education)
sum educ, d
tab educ // Majority of women have 12 years of education

* Exogenous Variable
* exper (years of experience in the labor market)
sum exper, d
tab exper
hist exper, discrete

* Potential Intrumental Variables
* motheduc (Years of education of the mother)
sum motheduc, d
tab motheduc

* fatheduc (Years of education of the father)
sum fatheduc, d
tab fatheduc

* huseduc (Years of education of the husband)
sum huseduc, d
tab huseduc

* check correlations between the endogeneous variable and the potential IVs
* it's the very first step to check whether they might be RELEVANT IVs
pwcorr educ motheduc fatheduc huseduc, star(0.05)
	// all of them are highly correlated with educ, as required 
	
reg educ motheduc fatheduc huseduc exper expersq, r
test motheduc fatheduc huseduc
	// the F-test confirms they are jointly significant, so they seem to be
	// relevant instruments for education. They are strongly correlated with educ
	
	
*********************************
* Exercise 3 - Wage regressions *
*********************************

* NOTE: for the sake of comparing the OLS model with the IV model
* we will temporarily run the models without robust standard errors
* because the Hausman test that needs to be run below cannot be used with robust s.e.
* at the very end, we will repeat the models with the "robust" option and
* show that the results are virtually the same 
* remember that you should use robust s.e. as much as possible


* a) OLS - neglecting possible endogeneity in educ
reg lwage educ exper expersq
estimates store ols			
// store these estimates to run Hausman test later

	// from this output, we would conclude that each additional year of 
	// education (experience) is associated with a 10.6% (2.4%) increase in 
	// wages for American working women. However, educ is likely correlated with
	// unobserved factors that are included in the error term that also affect
	// wages - e.g. ability, quality of education institutions, ambition, network
	
* b) Manual IV estimation using one instrument - fatheduc

* reduced form eqt: regressing educ on all exogeneous vars and one instrument	
reg educ fatheduc exper expersq

test fatheduc 	// this indicates that fatheduc is a relevant instrument 

* obtain the predicted values for educ
predict educ_hat, xb
sum educ_hat
predict educ_res, res
sum educ_res


* plug-in this new variable in the structural eqt for wage: 
reg lwage educ_hat exper expersq

* automatically, using ivreg2
ivreg2 lwage (educ = fatheduc) exper expersq, first 
estimates store iv_1inst		

// store IV estimates using 1 instrument

		// we see that the coefficient of educ is now smaller, but still significant
		// the results indicate an increase in wages of 7.9% for each additional
		// year of education
		
		// the difference in the coefficients could indicate that endogeneity can
		// actually be present, and that part of the effect we obtain in OLS
		// is attributed to unobserved factors (e.g., ability) rather than 
		// education per se.

		
* c) compare the estimates of OLS and IV-1 instrument
hausman iv_1inst ols

	// surprisingly, we do not reject H0, which states that the coefficients are
	// not significantly different. We could still suspect that endogeneity
	// is an issue due to the difference in "educ" coefficient. 
	// we can test this more explicitly using the following steps - question d)


	
* d) testing for the presence of endogeneity manually - slide 20
reg educ fatheduc exper expersq	
* predict the residuals of this equation and add them as a regressor in the wage eqt
predict educ_res, res 
sum(educ_res)
reg lwage educ exper expersq educ_res

	// the residuals' coefficient is not statistically significant
	// so apparently endogeneity is not really a problem in this case, despite
	// the smaller coefficient for education
		
	
* e) testing whether endogeneity is indeed present using ivendog post-estimation

ivreg2 lwage (educ = fatheduc) exper expersq, first
ivendog	educ
	         // remember to install it first (ssc install ivendog)

	// none of the tests reject the hypothesis that educ is exogeneous!
	
* in alternative, we could do: 
ivreg2 lwage (educ = fatheduc) exper expersq, first endog(educ)
	
		// we get a test at the bottom of the output, and still we do not reject
		// the null hypothesis that educ is exogeneous

		
* f) Using both mother's and father's educ as instruments

reg educ fatheduc motheduc exper expersq
predict educ2_hat, xb 
sum(educ2_hat)

reg lwage educ2_hat exper expersq

* or do it automatically:
ivreg2 lwage (educ = fatheduc motheduc) exper expersq, first

	* the return to education is very similar to the one obtained with only 1
	* instrument - now 7.7% instead of 7.9% for each extra year of education
	
* Test for Overidentification 
* first, manually 

/* In order to test for overidentying restrictions (check slide 22):

1. We need to perform the 2SLS regression with the instruments and then obtain 
the residuals.

2. We regress those residuals on all the EXOGENOUS variables including the 
instruments (educ should not be included). We obtain the R squared.

3. Finally, we calculate the statistic for the test, which is N (the number of 
observations) multiplied by the R-squared. 

The test is accomplished by comparing that statistic to a Chi-Square distribution
with "q" degrees of freedom (where "q" is the # of restrictions, i.e., the 
number of instruments we have minus the number of endogenous variables we have). 
In this case, q = 2 - 1 = 1. --- check slide 22 */

* Predicting the residuals from the 2SLS IV regression
ivreg2 lwage (educ = fatheduc motheduc) exper expersq, first
predict uhat, res
sum uhat


* Regressing the residuals on all the exogenous variables, including the instruments
reg uhat exper expersq motheduc fatheduc
* Obtaining the statistic for the test
scalar overidtest = e(r2)*e(N)
di overidtest

/* For a 5% level of significance, the value of a Chi-square with 1 degree of 
freedom is 3.841. Our statistic is way smaller than that (0.0268). Therefore,
it does not fall in the rejection area, so we cannot reject the null hypothesis 
in this case. Therefore, the conclusion is that instruments are valid 
(they are NOT correlated with the error term). If we rejected the hypothesis, 
it would mean that *at least one* would not be valid.
*/

* you can also do it automatically, as follows: 
ivreg2 lwage (educ = fatheduc motheduc) exper expersq, first
overid 		



// you need to install it first, type ssc install overid

	// we conclude once more that the instruments are valid, and we get the 
	// same test statistic as we obtained before (0.027)



* g) using 3 instruments

ivreg2 lwage (educ = fatheduc motheduc huseduc) exper expersq, first
overid 	

		// the return to education is estimated to be 8.7% per each extra
		// year of education
		// overid test still indicates that our instruments are exogeneous
		// if we would reject H0 here, at least one would not be exogeneous
		// note that the estimate is now closer to the one we get in OLS
		

* h) wrap up and get the different estimates once more (and using robust s.e.)

* OLS 
reg lwage educ exper expersq, r		         	//beta: 0.1056; se: 0.0110

* 1 instrument only - fatheduc (question 3b)
ivreg2 lwage (educ = fatheduc) exper expersq, r	//beta: 0.0789; se: 0.0311

* 2 instruments - question 3f
ivreg2 lwage (educ = fatheduc motheduc) exper expersq, r	//b: 0.0768; se: 0.0280

* 3 instruments - question 3g
ivreg2 lwage (educ = fatheduc motheduc huseduc) exper expersq, r //b:0.0874; se: 0.0187

// using 3 instruments would give a more efficient estimate of the returns to 
// education - the lowest standard error, so that would be preferred IF EDUC
// WAS INDEED ENDOGENOUS! BUT THE TESTS ABOVE ALL INDICATE THAT ENDOGENEITY IS
// NOT AN ISSUE IN THIS DATA --- SO OLS IS RECOMMENDED - MORE EFFICIENT THAN IV!


log close





