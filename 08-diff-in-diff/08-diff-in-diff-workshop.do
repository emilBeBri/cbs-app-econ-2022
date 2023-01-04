
* 9 Dec 2022
* Workshop on DiD - based on VodkaData.dta

* Exercise 1


clear all
set more off
cap log close

* change to your current directory 
cd "C:\Users\ys.si\OneDrive - CBS - Copenhagen Business School\teaching\Econometrics for PhD\Session 8"

* opens log file for collecting results
log using workshop_Vodka.log, replace

* open the dataset (available on CANVAS)
use VodkaData.dta

* a)
ttest logvodka, by(y2011)
	/*
	we observe a decrease in consumption of vodka after 2011.
	the difference is large and statistically significant
	*/
	

* b) 
ttest logvodka, by(hdrinker50)
	/*
	yes, we do confirm that heavy drinkers have consumed (much) more vodka than others 
	heavy drinkers consume about 2.5 times more vodka
	we need to compute the following ratio:
	*/
	di exp(2.815)/exp(1.913)
	

*c.i)
 
ttest logvodka if y2011 == 0, by(hdrinker50)
ttest logvodka if y2011 == 1, by(hdrinker50)	

	/*
	the difference has decreased from 4.14 pre-policy to 3.84 post-policy
	based on these tests, we cannot yet see if the difference is statistically significant
	*/

* c.ii)

ttest logvodka if hdrinker50 == 0, by(y2011)
ttest logvodka if hdrinker50 == 1, by(y2011)
	
	/*
	non-heavy drinkers reduced their vodka intake by approx 32% post-2011
	heavy drinkers reduced their intake by 63%
	we would conclude, based on these simple t-tests, that the tax has been effective in reducing
	vodka consumption, specially among heavy drinkers
	*/

* d) 
ttest logprice_vodka_real , by(y2011)
	/*
	23% higher prices post-2011
	*/


* Exercise 2
* a) 
bysort year: egen vodka_nonheavy=mean(logvodka) if hdrinker50==0
lab var vodka_nonheavy "Non-heavy drinkers"
bysort year: egen vodka_heavy=mean(logvodka) if hdrinker50==1
lab var vodka_heavy "Heavy drinkers"


* b)
twoway (connected vodka_nonheavy vodka_heavy year), xline(2011) ytitle("Vodka consumption") xtitle("Year") xlab(1994(2)2014)

/*
	overall, one can see a slight decrease in alcohol consumption from the mid-90s to the 2000s for both groups
	one could say that the two groups have fairly parallel trends though it is difficult to assess whether their different fluctuations are significantly different from each other
	focusing the attention on the 2-3 years just before the tax introduction, we do see some fluctuations: heavy drinkers reduce their consumption slightly and the others seem to increase it slightly as well. In other words, there "vertical distance" reduces slightly which may indicate some potential selection concerns. (we will go back to this question more formally in Exercises 3 and 4)
	after the tax: we see a drop in consumption for both groups but this drop seems much more pronounced for heavy drinkers
*/

* c)
reg logvodka  i.hdrinker50##i.y2011, cluster(idind)

	/*
	the coeff of "hdrinker50" corresponds to the pre-policy difference in consumption between the two groups 
	this corresponds to the vertical difference we observe between the two lines in b) - pre-2011
	the coefficient of y2011 represents the effect of the federal tax on non-heavy drinkers (decline of 32% in consumption)
	the interaction term captures the additional change in consumption, resulting from the tax policy, on heavy-drinkers.
	their consumption decreases by an additional 31%
	in conclusion, the effect of the policy was more pronounced for heavy-drinkers than non-heavy-drinkers
	
	the std errors should be clustered to account for the correlation between observations belonging to the same individual
	*/
 
*d) 
global controls logincome age age_2 smokes married college curwrk stroke 
reg logvodka  i.hdrinker50##i.y2011 $controls, cluster(idind)

	/*
	we observe a positive elasticity between income and vodka consumption; a 1% increase in income is associated with a 1.3% increase in vodka consumption
	older individuals drink more than younger, but at a decreasing pace (inverted U-shaped relationship)
	individuals that smoke tend to drink approx 10% more 
	married individuals also drink more (!), as do individuals with college degrees (!) and individuals who are currently working (!) - apply interpretation of dummy variables here
	individuals who had a stroke in the past drink less than those who never experienced a stroke
	
	the estimates of interest change slightly because part of the differences previously captured in the simpler regression could be attributed to those other differences (socio-economic characteristics, health issues, etc). 
	Excluding those variables - which are, individually and jointly, significant predictors of alcohol consumption, could easily overestimate the effects of the policy and the differences between the two groups
	*/


	
* Exercise 3	

* a) 	
xtset idind	
xtreg logvodka  i.hdrinker50##i.y2011, fe r
xtreg logvodka  i.hdrinker50##i.y2011 $controls, fe r

	/*
	i. the estimated impact of the federal tax on non-heavy-drinkers, based on a model without control variables, is a reduction of approx 13% in vodka consumption. 
	however, when we control for other aspects in which these individuals differ, we obtain a positive estimate for the coefficient of y2011, meaning that everything else equal (including unobservable aspects), non heavy-drinkers increased their consumption by 10% (approx) after 2011. We should therefore be carefull when attributing changes in alcohol consumption to the federal tax vs. other aspects that may make individuals different from each others
	
	ii. in both cases, we obtain a negative and significant coefficient for the interaction term, suggesting that the policy had a different effect for heavy and non-heavy drinkers
	when it comes to estimating the impact of the federal tax on vodka consumption of this particular group of individuals, we need to "sum up" the two coefficients: 
	in a model without controls, we estimate a decrease of nearly 39% in consumption (-0.132-0.254)
	when we include control variables, the change in consumption within this particular group is much smaller: 10.6% decrease (0.101-0.207)

	*/

*b) 
* in case they want to reproduce the output

gen years_post = year - 2011
gen years_pre = 2011 - year
replace years_post = 0 if years_post < 0
replace years_pre = 0 if years_pre < 0
xtreg logvodka  i.hdrinker50##i.years_pre i.hdrinker50##i.years_post $controls if year >= 2008, fe r

	/*
	
	i. there are different coefficients for years_pre and years_post because these are "dummy variables" for each respective year; having one coefficient for each variable would impose a linear trend pre- and post- policy 
	
	ii. looking at the coefficients of years_pre, we see that all of them are not significant - so no significant pre-trend for non-heavy drinkers
	
	iii. looking at the interaction between hdrinker50 and years_pre, we see that 2 out of 3 coefficients are statistically insignificant. However, in t-2 (i.e. 2009) we see a drop in the relative consumption of heavy-drinkers (compared to non-heavy drinkers). 
	We could test whether this gap between the two groups in 2009 was significantly different from the gap observed in 2011, based on the following test:	
	*/
	
	test 2.years_pre - 1.hdrinker50#2.years_pre=0
	
	/*
	this difference is statistically different from zero, which indicates that the difference between the two groups was smaller in 2009 than in 2011. This could raise concerns and bias the estimated effect of the tax change. (We will return to this in question 4c)
	
	
	iv. non-heavy drinkers reduced their consumption by nearly 13% from 2011 to 2012
	this difference is significant (p = 0.017)
	
	v. heavy-drinkers' consumption of vodka was about 69% lower in 2014 than their own consumption in 2011 (we need to sum the two coefficients: -0.228-0.459)
	compared to the change in consumption by non-heavy drinkers in that same year, heavy-drinkers consumed 46% less vodka. 
	
	vi. the effect of the policy does not seem to be short-lived; we see a persistent decrease in the consumption of alcohol, especially for heavy drinkers, in the 3 years following the tax reform. 
	
	*/

	
* Exercise 4

*a) 
diff logvodka, treated(hdrinker50) period(y2011) cluster(idind)

	/*
	similar estimate of DiD as obtained in 2c
	note however that this output gives us the difference between T and C before and after
	the first difference is comparable to the coefficient of hdrinker50 in the OLS model
	the second difference is comparable to the sum of the coefficients of hdrinker50+y2011 in the OLS model (4.142-0.305 = 3.836)
	*/
	
*b) 
diff logvodka, treated(hdrinker50) period(y2011) cov($controls) report cluster(idind)


*c) with matching
diff logvodka if year >=2008, treated(hdrinker50) period(y2011) cov($controls) id(idind) k cluster(idind)

/*
DIFFERENCE-IN-DIFFERENCES ESTIMATION RESULTS
Number of observations in the DIFF-IN-DIFF: 27510
            Before         After    
   Control: 6959           3907        10866
   Treated: 6963           9681        16644
            13922          13588
--------------------------------------------------------
 Outcome var.   | logvo~a | S. Err. |   |t|   |  P>|t|
----------------+---------+---------+---------+---------
Before          |         |         |         | 
   Control      | 0.524   |         |         | 
   Treated      | 4.588   |         |         | 
   Diff (T-C)   | 4.064   | 0.035   | 115.07  | 0.000***
After           |         |         |         | 
   Control      | 0.311   |         |         | 
   Treated      | 4.126   |         |         | 
   Diff (T-C)   | 3.816   | 0.040   | 94.91   | 0.000***
                |         |         |         | 
Diff-in-Diff    | -0.249  | 0.054   | 4.65    | 0.000***
--------------------------------------------------------
R-square:    0.45
* Means and Standard Errors are estimated by linear regression
**Inference: *** p<0.01; ** p<0.05; * p<0.1


*/

/*
the estimate is slightly smaller now, we can even compare it with the DiD obtained in the 2 models above when restricting to the same period
(-0.286 vs -0.249)
implementing propensity score matching and DiD simultaneously may have reduced concerns with the non-parallel trends identified in Exercise 2 and 3. This must be a less biased estimate of the "true" effect of the tax reform, net of selection effects (driven by observables)
*/
diff logvodka if year >= 2008, treated(hdrinker50) period(y2011) cov($controls) report cluster(idind)



log close
