
* Example Propensity Score Matching
* Lecture 16 November 2022


clear all
set more off
cap log close

* change to your current directory 
cd "C:\Users\...\"

* opens log file for collecting results
log using classexercises.log, replace

* open the dataset from the web (also on CANVAS: DataPSM_birthweight.dta)

webuse cattaneo2
d
sum			// % of smoking mothers? 18.6%

* check average difference in outcome variable between smokers and non-smokers
ttest bweight, by(mbsmoke)
	// significant difference - birth weight is lower for smoking mothers than non-smokers

* simple OLS - what would be wrong with this approach?
reg	bweight	mbsmoke	mmarried mage medu foreign alcohol deadkids	monthslb fedu fbaby	frace

* check pairwise correlations between smoking status and several characteristics
* of the mother and/or father

pwcorr mbsmoke mmarried mhisp fhisp foreign alcohol deadkids mage medu fage ///
	   fedu nprenatal monthslb order mrace frace prenatal fbaby prenatal1, ///
	   star (0.05)

		// there you see for instance the negative correlation between smoking 
		// and mothers' age, education, being white, or being the 1st pregnancy
		// on the other hand, positive correlation with alcohol consumption

* we could compare the means of some variables for smokers and non-smokers:
tabstat mmarried mhisp fhisp foreign alcohol deadkids mage medu fage fedu ///
		nprenatal monthslb order mrace frace prenatal fbaby prenatal1, ///
		by(mbsmoke) stats(mean) 
		
* and test whether the differences are significant
local varlist "mmarried mhisp fhisp foreign alcohol deadkids mage medu fage fedu nprenatal monthslb order mrace frace prenatal fbaby prenatal1"
foreach var of local varlist {
	describe `var'
	ttest `var', by(mbsmoke)
}
* these steps are important for selecting the variables to include in the
* Propensity Score estimation

* let's investigate which variables predict the probability of smoking
* (i.e., probability of being observed in the "treatment group")

logit mbsmoke mmarried mage medu foreign alcohol deadkids monthslb fedu fbaby frace

	// good selection of variables that could predict the "treatment"

* let's estimate the ATE (and then the ATET) using this equation for the Propensity Score:

teffects psmatch (bweight) (mbsmoke mmarried mage medu foreign alcohol ///
							deadkids monthslb fedu fbaby frace)

teffects psmatch (bweight) (mbsmoke mmarried mage medu foreign alcohol ///
							deadkids monthslb fedu fbaby frace), atet

							
* what if we want to impose to use at least 3 closest neighbors for each treated mom?

teffects psmatch (bweight) (mbsmoke mmarried mage medu foreign alcohol ///
							deadkids monthslb fedu fbaby frace), nn(3)
							
			// consequences in terms of bias and variance?
			// note that this forces Stata to match with more distant neighbors

* let's check the matches for each individual
teffects psmatch (bweight) (mbsmoke mmarried mage medu foreign alcohol ///
							deadkids monthslb fedu fbaby frace), gen(match)

* predict the propensity score for each individual
* note that ps1 will be the prob of treatment (smoking during pregnancy); ps0 = 1-ps1				
predict ps0 ps1, ps

* check what Stata does in terms of matching pairs:
browse bweight mbsmoke ps0 ps1 match*


* predict potential outcomes
predict y0 y1, po
* predict treatment effect
predict te		

* now get the ATE and ATET - the same as obtained above
sum te
sum te if mbsmoke == 1			

* example: 
list bweight mbsmoke mmarried mage medu foreign alcohol deadkids monthslb ///
	 fedu fbaby frace ps0 ps1 y0 y1 te match1 match2 in 1
list bweight mbsmoke mmarried mage medu foreign alcohol deadkids monthslb ///
	 fedu fbaby frace ps0 ps1 y0 y1 te match1 match2 in 4043					
		
* overlap condition
teffects overlap

* check also through these stats (which are what the plot above illustrates): 
sum ps0 if mbsmoke == 0  // in between 0.13 and 0.99
sum ps0 if mbsmoke == 1  // in between 0.09 and 0.96

	// so common support region: 0.13-0.96 (quite large)

* balancing condition
tebalance summarize, baseline
tebalance summarize

* check plots for the quality of the match for mother's age for instance:
tebalance density mage
* for mothers' education, given the distribution of the variable, it may be
* more informative to visualize the boxplot before and after matching
tebalance box medu

****************************
** caliper matching instead
****************************

* if we impose a max distance of 0.05 between the PS of the observations being matched
* we get the same results - which means that the previous estimation is already
* getting differences not larger than 0.05

teffects psmatch (bweight) (mbsmoke mmarried mage medu foreign alcohol ///
							deadkids monthslb fedu fbaby frace), caliper (0.05)

* if we decrease the tolerance level to 0.04, Stata stops because at least one match
* exceeds this distance. Check the error message with caliper = 0.04

teffects psmatch (bweight) (mbsmoke mmarried mage medu foreign alcohol ///
							deadkids monthslb fedu fbaby frace), caliper (0.04)
		
* we could relax the tolerance and include more distant matches as well, but
* Stata will include the closest ones and give the same results are before:

teffects psmatch (bweight) (mbsmoke mmarried mage medu foreign alcohol ///
							deadkids monthslb fedu fbaby frace), caliper (0.10)
		
		
********** if we used psmatch2 instead of "teffects psmatch" ******************
** install it first in case you don't have it yet
** type "ssc install psmatch2"
** or type "findit psmatch2" and choose the right one to install manually
	
* example:

psmatch2 mbsmoke mmarried mage medu foreign alcohol deadkids monthslb ///
		 fedu fbaby frace, out(bweight) logit ate
		
		* note that SE are not always estimated, or if they are, they are not
		* correctly estimated. teffects psmatch is preferrable in that regard!
		** ATT here stands for Average Treatment Effect on the Treated
		** ATE means Average Treatment Effect
		** be aware of the different notation in teffects (ate & atet)
					
log close
clear							
							
							
							
							
							
							
							
							
							
							
							
							
							
							
							