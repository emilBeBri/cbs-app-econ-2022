* Workshop 5: Treatment evaluation and matching methods
* 18 Nov 2022

clear all
set more off
cap log close



cd /home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/


* opens log file for collecting results
* log using workshop_matching.log, replace

* open the dataset (available on LEARN)
use input/datamatching.dta, replace
d
sum	

**********************************
* Exercise 1 - experimental data *
**********************************
	
* comparing individuals in the treatment and control group 

local varlist "age education nodegree black hispanic married re74 re75"
foreach var of local varlist {
	describe `var'
	ttest `var', by(treat_experiment)
}



ttable2 age education nodegree black hispanic married re74 re75, by(treat_experiment)

	* note that you may need to install the command ttable2 first; 
	* if that is the case, type "ssc install ttable2"

	/*
	
	note that "treat_experiment" is missing for PSID individuals 
	i.e., individuals not participating in the Dehejia and Wahba’s experiment
	
	we conclude that the individuals in the treatment and control groups are
	not significantly different in terms of these variables (except for "nodegree" and
	"hispanic", but the difference in the latter is only weakly significant) 
	
	This satisfies the Balancing Condition: assignment to treatment is random
	So treated and untreated individuals look similar in terms of X variables

	*/
	
ttest re78, by(treat_experiment)
* actual difference is therefore ~ 1794 USD. This is what we're trying to hit in the rest of the 
	// significant difference, with treated individuals earning nearly 1,800 USD more


***********************************
* Exercise 2 - observational data *
***********************************


* comparing individuals in the treatment group with those from PSID 

drop if treat_experiment == 0

local varlist "age education nodegree black hispanic married re74 re75"
foreach var of local varlist {
	describe `var'
	ttest `var', by(training)
}

ttable2 age education nodegree black hispanic married re74 re75, by(training)

	/*
	
	note that "training" is 1 for treated individuals in the experiment and 0
	for all PSID individuals
		
	the individuals are very different. Those who received the treatment are on
	average younger, less educated, more often black, less often married, and 
	earned way less before 1978
	
	This violates the Balancing Condition: individuals are not randomly assigned
	to training programs anymore. Treatment is correlated with X variables
	
	In this setting a simple comparison between treated and non-treated men in
	terms of re78 is misleading. Unconditionally, there is a negative and
	significant difference:
	
	*/

ttest re78, by(training)
	// we observe the opposite difference - non-trained individuals earn much more than trained ones; this is not a reasonable comparison/control group

* OLS regression
reg re78 training age nodegree black hispanic married re74 re75

	// coefficient of training is now positive but n.s. (after controlling for many other aspects in which trained and non-trained people differ)
	// note that we could replace nodegree by education instead. we find that education is +.vly linked to earnings either way
	// in the next exercise, the coefficient of nodegree tends to be significant while education's is not, so let's use "nodegree" from now on


******************************************
* Exercise 3 - Propensity Score Matching *
******************************************

* logit model for the probability of participating in the job training programme

logit training age nodegree black hispanic married re74 re75

	/*
	note: 
	
	coefficients interpreted in terms of their effect in the log odds of partici-
	pating (vs not participating) in the training program; otherwise we would need to compute mg effects
	
	all variables significantly determine the probability of treatment
	*/

* use PSM to estimate ATET
teffects psmatch (re78) (training age nodegree black hispanic married re74 re75), atet
teffects psmatch (re78) (training age nodegree black hispanic married re74 re75), atet nn(5)

	* we include possibly poorer but more matches: + bias but reduced variance
	* note that ATET is positive but n.s. in both cases

	
***********************************************
* Exercise 4 - Balance and Overlap Conditions *
***********************************************

* Checking balancing condition
tebalance summarize, baseline
tebalance summarize

	* remember that ideally: matched difference is close to 0 and variance ratio close to 1
	
* Improving the propensity score equation by (FOR INSTANCE) adding a quadratic 
* term for age and an interaction term between age and nodegree:

teffects psmatch (re78) (training c.age##c.age c.age##i.nodegree black hispanic married re74 re75), atet
teffects psmatch (re78) (training c.age##c.age c.age##i.nodegree black hispanic married re74 re75), atet nn(2)

	* ATET is now significant and a better balance between variance and
	* bias is obtained with 2 nearest neighbors at the minimum
						 
		
* to predict propensity scores, repeat ATET estimation with gen(match) option
teffects psmatch (re78) (training c.age##c.age c.age##i.nodegree black hispanic married re74 re75), atet nn(2) gen(match)
						 
predict ps0 ps1, ps
teffects overlap 
graph export "stata/test-stata.png", replace

/*
	Overlap assumption clearly violated, the estimated propensities are
	concentrated around 0 or 1 depending on the group. 
	We cannot draw valid inferences from these data. We would need another
	set of untreated people to conduct the analysis.
*/

sum ps0 if training == 0
sum ps0 if training == 1

/*

	Note that looking at the statistics only and not at the plot would be erroneous, since
	here we would see that the min and max of the values of ps1 for both groups have a good
	overlapping. This shows that this test is not enough, we should also look at the plot, which
	shows better that the assumption is violated.
	
	overall, we conclude that we have a bad control group, not suitable for a proper matching analysis
*/

log close
