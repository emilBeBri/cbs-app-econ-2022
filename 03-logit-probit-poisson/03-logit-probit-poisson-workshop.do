clear all


set more off
set logtype text

cd /home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/
do "stata/data-preprocessing-workshop-first-example-v01.do"




* ********** MERGING **********
* * Merge information from the register dataset
* merge 1:1 id using register.dta
* tab _merge
* drop _merge
* * Merge information from the exercise3_extra_data dataset
* merge 1:1 id using exercise3_extra_data.dta
* tab _merge
* drop _merge

* ********** GENERATING KEY VARIABLES **********
* * R&D Intensity = (Internal R&D investment)/(turnover)
* gen rdint = xinterm/idbrturn
* gen rdintpct=rdint*100
* * Export Intensity = (Exports volume)/(Turnover)
* gen expint = export00/idbrturn
* * Capital Expenditure Intensity = (Capital Intensity)/(Turnover)
* gen capint = capex00/idbrturn
* * Natual Log of the number of employees in the year 2000
* gen lempl00 = log(employ00)

* drop if expint > 1
* drop if rdint > 1 & rdint != .

***EXERCISE 4 - LOGIT & PROBIT ***

* Let us first explore what type of variable PRODINOV is
sum prodinov
tab prodinov
* PRODINOV is a binary variable. Logit or Probit regressions are appropriate in this case.

// Logit Regression
logit prodinov extsource rdintpct inconst lempl00, robust

* In a logit (or probit), the size of the coefficients are not directly interpretable.
* We can only interpret the sign and the significance of the coefficients.

//// Marginal Effects ////
// We can calculate average marinal effects (average partial effects in wooldridge), conditional average marginal effects, and the predicated probabilities //
/// For linear regression (OLS), addigng at means does not change the margianl effects estimated. 
// However, in a non linear model (such as logit) it matters!
/// Average marginal effects - we interpret:
/// One unit (sd) increase in external source, holding all other variables constant, increases the probability for product innovation by 0.080732 ///
logit prodinov extsource rdintpct inconst lempl00, robust
margins, dydx(*)
/// conditional marginal effects - we interpret:
// Holding all other variables constant at thier mean, the increase of probability is now 0.0885114 //
margins, dydx(*) atmeans 
marginsplot
graph export "stata/test-stata.png", replace

* linear regression for comparison
reg prodinov extsource rdintpct inconst lempl00, robust
** We can see that the condtional marginal effects, avrage marginal effects, and OLS estimations are very similar. ** 

logit prodinov extsource rdintpct inconst lempl00, robust
** predicted probabilities
** the probability that a firm is product innovating, when all regressors are set at their means is 0.5549194**
margins, atmeans predict()
** Change in predicted probabilities on diffrent levels of rdintpct **
margins,  predict() at(rdintpct = (0(2)70))
marginsplot 
graph export "stata/test-stata.png", replace


/*
* extra 
* what would the default give us?
margins, predict()
* calculate directly from post-estimates from logit
predict y1prob, pr
count if y1prob != .
su y1prob
*/


// Probit regression
probit prodinov extsource rdintpct inconst lempl00, robust
* Note that the coefficients in the logit are (approx) 1.6 times higher than in the probit.

//// Marginal Effects ////
margins, dydx(*)
margins, dydx(*) atmeans 

** predicted probabilities
margins, atmeans predict()

* Note that marginal effects are roughly the same as the ones obtained from the Logit.



* EXERCISE 5: COUNT MODELS

// Checking the dependent variable (patapply) 
sum patapply, detail
tabstat patapply, stats(mean sd v n) 
histogram patapply, discrete freq 
histogram patapply if patapply < 100, discrete freq 
histogram patapply if patapply < 20, discrete freq 

/* There seem to be some negative values (which makes no sense). 
we can set them to missing values */
tab patapply if patapply==-1

replace patapply=. if patapply==-1
tab patapply

tab market /* It is a categorical variable (include it with the i. prefix in the regression) */

// Poisson model 
poisson patapply lempl00 rdintpct i.pcoop i.market, robust
// Interpret coefficients: 
// (1) If a company was to start cooperating (pccop==1), the expected counts of patents would multiply by a factor of exp(1.192) = 3.29 (= the IRR of that coefficient). This is an increase of 100*(exp(1.192)-1) = 229 per cent, while holding the other variables in the model constant.//
// (2) Employment enters the model in logs, so the coefficient 0.719 is the elasticity of the expected number of patents with respect to employment. This means that for a one percent change in employment, we would expect to see a 0.719 percent change in the number of patents, all else equal. This approximation is valid for small changes.
// (3) rdintpct is a continuous variable. Following an increase of one percentage point in R&D investment relative to turnover, the number of expected patents would multiply by a factor of approximately exp(0.04)=1.04 (=IRR). That is, an increase of approx. 100*0.04=4 per cent while holding the other variables in the model constant //
// (4) When interpreting Market, we need to remember that the base category is the local market (market==1) therefore, let's interpret the coefficient  market==3: relative to firms who operate in the local market, the expected counts of patents applications would multiply by a factor of exp(2.08) =  8.03 (= the IRR of that coefficient). This is a huge increase of 100*(exp(2.08)-1) = 703 per cent, while holding the other variables in the model constant.//

poisson, irr 
// IRR (incidence-rate ratios) gives you the multiplier that applies to the expected value of the DV if the IV is increased by one unit (or from zero to one, in the case of a dummy)
// While the interpretation also applies for log transformed variables, meaning a one log-unit increase (for example, from n=10 to n=27 employees (ln(27)-ln(10) is approx. equal to 1), for those variables, the elasticity interpretation is more straightforward.


// Expected number of patents for a firm with average values on all variables
margins, atmeans
margins, atmeans at (pcoop=1 market=3 rdintpct=(0(1)70)) predict()
marginsplot

*** EXTRA: If you want to look at how the number of expected patents at different levels of R&D investment differ between markets run the next two lines ***  
margins, atmeans at (pcoop=1 rdintpct=(0(10)40) market=(1 2 3 4)) predict()
marginsplot



// Negative Binomial model
nbreg patapply lempl00 rdintpc i.pcoop i.market
nbreg, irr

//to interpret the coefficients of a Negative Binomial model, we will apply a similar method: (1) exp(beta) for multiplier (=irr). (2) for dummy variables 100*(exp(beta)-1) (3) for continues variables approx. 100*exp(beta).
margins, atmeans

// Remember that Poisson regression with the robust SE is the superior model for when you have an outcome variable with count data.

log close
