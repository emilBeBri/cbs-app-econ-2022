
* Workshop 4: Sample selection and Heckman models
* 11 November 2022

clear all
set more off
cap log close

cd "/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/"

* opens log file for collecting results
* log using workshop_selection.log, replace

* open the dataset (available on CANVAS)
use input/workshop-second-ex-reflexitaly.dta, clear 


***************************************
* Exercise 2 - descriptive statistics *
***************************************

sum lnwage, detail
hist lnwage
tabstat lnwage, by(employed)

mdesc lnwage // shows the amount of missing values in the variable (19%)

local varlist "female age child stem goodgrades unemp6more lookjobbefore livewpartner livewparents"
foreach var of local varlist {
	describe `var'
	ttest `var', by(employed)
}
 
ttable2  female age child stem goodgrades unemp6more lookjobbefore livewpartner livewparents,by(employed)
 
 /*
 we find some significant differences for some variables - e.g. female
 there is a higher proportion of women in the non-employed group than in the
 employed group. Women may actually be more likely to be non-employed due to 
 a variety of reasons - e.g., materny leave. Indeed, people with kids are more often out of employment. 
 If we neglect this factor we may end up obtaining biased estimates of the gender wage gap, since we do not know how much those women who are out of the labor market would earn, would they be
 employed. Other significant differences may also indicate that those who are not 
 employed by the time of the survey may be different in some respect; 
 neglecting these differences may bias the estimation of their effect on wages.
 
 */
 
****************************************
* Exercise 3 - OLS regression for wage *
****************************************
 
reg lnwage  female age stem goodgrades unemp6more, r
 
 /*
 All the coefficients are statistically significant in this regression.
 Since the dependent variable is in logs, women are estimated to earn on average
 approx. 5.8% less than men, everything else constant, while STEM graduates seem
 to earn approx. 7.4% higher hourly wages, on average. There is also an approx. 6.2% wage premium
 for the best students with good grades, while those who suffered a period of 
 unemployment of 6 or more months after graduation earn approx. 8% lower earnings on avg. 
 Hourly wage seems to increase by 0.6% for every additional year of age. 
 
 However, we should be aware of the incidental truncation problem we have in
 these data. We did see in ex 2 that there is a much higher (lower) share of
 women (stem graduates) among those who are not employed. This sample selection
 may bias the estimates of the "true" effects of these variables on wages. 
 */
 
********************************************
* Exercise 4 - Probit for Heckman 1st step *
********************************************
 
probit employed female age stem goodgrades unemp6more child livewpartner livewparents lookjobbefore, r

				
 /*				

 Note that now all the 2,108 observations are used, while the OLS was only using
 the observations for employed individuals. Remember the interpretation in probit
 regressions in terms of log of odds. 
 
 We see, for instance, that women are significantly less likely to be in the 
 labor force, which implies that there is also a potential gender gap in labor
 participation. 
  
 */
 
 // to interpret the coeff of female, we need to compute the mg effects:
margins, dydx(female)
// women have a prob. of being employed 7.3 percentage points lower than that of men
/*
overall, we also see that STEM graduates and those who looked for jobs before 
graduating are more likely to be employed at the time of the data collection; 
graduates with kids and those who live with parents have a lower chance to be employed
we cannot interpret the size of the coefficients directly, we would need to compute mg effects
there is no significant association between employment and individuals' age, their grades
and whether they live with a partner (according to this dataset)
*/

test child livewpartner livewparents lookjobbefore
[]
 /*
 even though "livewpartner" does not seem to be significantly associated with
 labor participation, these 4 variables altogether are significant predictors
 of selection into the labor market
 
 It makes sense (theoretically) to include these variables in the labor force
 selection equation. By extending this probit model with these variables, we 
 are assuming they do not affect the main outcome equation. They are therefore
 our exclusion restrictions. Theoretically they make sense, but we should test
 whether or not they affect wages.
 
 */

*** estimating IMR manually

predict xbeta, xb
gen mills = normalden(xbeta) / normal(xbeta)
label var mills "Inverse Mills ratio"
sum mills

save output/workshop-5-test.dta

**********************************
* Exercise 5 - OLS including IMR *
**********************************

reg lnwage female age stem goodgrades unemp6more mills, r

pwcorr female age stem goodgrades unemp6more mills if employed == 1, star (0.01) 

/*

 the gender wage gap and the premium to STEM education is no longer significant
 other coefficients on the other hand remain more or less unchanged

 As in Certo et al. (2016), one way to "imperfectly" assess the strength of the exclusion
 restrictions is to look at the correlation between IMR and X's. 
 
 In their paper, a correlation of 0.31 was perceived to be fine (the lower, the better). 
 In our case, corr(gender, IMR) = 0.399, which seems close to the reference used in
 Certo et al. 2016. But if one can find exclusion restrictions that would decrease
 the correlations even further, it would be desirable. 
 
 
*/

*******************************
* Exercise 6 - Heckman models *
*******************************

heckman lnwage female age stem goodgrades  unemp6more , select(employed = female age child stem goodgrades unemp6more  livewpartner livewparents lookjobbefore) twostep 
* the coefficients are the same as obtained in ex 5 (OLS with mills)		
		
	
heckman lnwage female age stem goodgrades  unemp6more , select(employed = female age child stem goodgrades unemp6more  livewpartner livewparents lookjobbefore) vce(r)  

/*

 rho is negative and significantly different from zero;
 it gives the correlation between the error terms of the 2 equations.
 
 the fact that it is negative indicates that there are unobserved factors
 making individuals more likely to participate in the labor market that 
 simultaneously DECREASE their hourly wage. In other words, wages in this sample
 are shifted downwards due to a NEGATIVE selection effect. A person with sample average
 characteristics who selects into wage employment earns lower
 wages than a person randomly drawn from the population would earn. 
 
 Note however that this is in contrast with what labor economics papers normally find - usually, 
 people in the labor market tend to be POSITIVELY selected, meaning that the
 unobserved characteristics that make them more likely to work also tend to contribute positively 
 to their wages - this could be, for instance, innate ability. 
 
 this sample here may have been collected at a "special" point in time and space - it captures Italian graduates in certain fields, 
 who may have struggled to find jobs at that point in time for different reasons (e.g. economic conditions) and this may contribute to this "different finding". 
 Indeed, the study where this dataset comes from finds that many graduates found jobs for which they were overeducated/overskilled, 
 which pushed their wages downwards. Still, those employed may be a "negative" selection of young individuals; those who were not working at the
 time were probably "brighter" and were studying more (e.g. masters), or migrated out of the country, waiting/looking for better job opportunities.
 
 */
 
	
***************************************
* Exercise 7 - Exclusion Restrictions *
***************************************

reg lnwage  female age stem goodgrades unemp6more child livewpartner livewparents lookjobbefore, r

/*
none of them are significantly related to wages, except livewparents

Yet, note that "livewparents" may capture other job characteristics that we cannot
control for - e.g., job location or the skill level required. Theoretically, it 
makes sense that wages will not DIRECTLY depend on whether one lives with their parents; 
however, living with their parents may signal something else about the individual, which is 
unobservable, that could correlate with their labor market participation - e.g. financial
dependency? 

	as long as this variable does not correlate with any "unobservables" that could also 
	predict wages (e.g., individual's intelligence), it may make sense theoretically to consider it
	but ideally, we would find variables that, controlling for everything else, no longer relate to wages. 

*/
		

* heckman without exclusion restrictions

heckman lnwage female age stem goodgrades  unemp6more , select(employed = female age stem goodgrades unemp6more) twostep

heckman lnwage female age stem goodgrades  unemp6more , ///
		select(employed = female age stem goodgrades unemp6more) vce(r)


* IMR is no longer significant and rho is no longer statistically different than 0
* we would then wrongly conclude that there is no selection bias in our sample
 

log close
