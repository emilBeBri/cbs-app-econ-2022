
use AERexam_cross.dta, clear

// Q1

// a. how many startups "migrated" to the US? which percentage of total startups do they represent in the sample?
tab us_hq 	// 174 startups; 8%

// b. how many startups have raised money? what is the mean value of funding raised by these startups (in US Dollar million)? 
sum total_raised if total_raised > 0 // 1,869 startups; raised on avg 10.57 US million

// c. draw a histogram of the total amounts raised by startups. Comment on the distribution
hist total_raised
// big spike at zero, very skewed distribution with long tail to the right

// d. Conti and Guzman consider the following transformation of the amount of venture capital raised after an initial round of funding: ln_raised = ln(total_raised +1).  Why do they add 1 in this transformation? Draw the histogram of ln_raised. Comment on the distribution and compare to the one you saw in Q1.c.
hist ln_raised
// less skewness due to the logarithmic transformation, add 1 to keep the zero observations in (without the addition, the log is not def for those obs and they drop out), transformed variable can be interpreted in relative terms for non-zero obs

// e. it is believed that startups that migrate to the US raise more money - do a t-test for significance of this difference based on the variable ln_raised. What is the estimated difference? Can you interpret it as the effect of moving a startup's HQ from Israel to the US, as indicated by the dummy variable us_hq, keeping all else equal? Why/why not? What is the direction of bias that you would expect?
ttest ln_raised, by(us_hq)	// difference is large (approx 173 percent higher for migrated startups, note: large!) and statistically significant. If better startups are more likely to move, the estimate has an upward bias

// f. run regression for ln_raised on us_hq, n_founders, prof, n_ssfp and industry dummies (note that there are 8 industry dummies: cleantech comms it_software internet life_science medical_dev semicond misc)
// interpret the coefficients that are statistically significant. Are there statistically significant differences between industries?
reg ln_raised us_hq n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond , r

	// firms that locate their HQ in the US raise approx (exp(1.52)-1)*100% = 358% more funding (the change is substantial  the simple approx is not appropriate here) cet.paribus
	// one additional founder is associated with an approx increase of 18% in funding raised cet.paribus
	// prof is not statistically significant at conventional levels
	// for each additional successful startup founded earlier, the amount of funding raised is estimated to increase 21% cet.paribus
	// joint significance for industry dummies below (the regression uses misc ("hardware") as reference category)

test cleantech comms it_software internet life_science medical_dev semicond 
	// the F-test shows that the industry dummies are jointly significant, so yes, there are significant industry differences
	
	
	// g. It is believed that the funding premium obtained from moving a startup to the US could vary according to the state in which the HQ is set up. The data has dummy variables defined for different US destinations. Specifically, CA_hq  equals 1 if a startup that moved its HQ to California, zero otherwise, NY_hq equals 1 if a startup that moved its HQ to New York, zero otherwise, and other_us_hq equals 1 if the HQ is set up elsewhere in the US. Run a regression model in which you can test the hypothesis that startups which move to CA raise significantly more money than firms moving to New York. Can you confirm the expectation? 
	
*gen NY_hq = (migration_state == "NY" & us_hq == 1)
*gen CA_hq = (migration_state == "CA" & us_hq == 1)
*g other_us_hq = us_hq == 1 & NY == 0 & CA == 0

// using all non-movers as baseline

reg ln_raised NY_hq CA_hq other_us_hq n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond, r
test NY_hq = CA_hq

	// no - the amount of funding raised by firms that move HQ to CA and NY is equivalent
	// should be performed as a one-sided test, given how the question is phrased

	
// h. it is believed that the funding premium of moving your firm to the US increases with the founders' prior success - is this true? produce a plot where you illustrate the funding premium depending on the number of prior successes.	

reg ln_raised i.us_hq##c.n_ssfp n_founders  prof cleantech comms it_software internet life_science medical_dev semicond , r
margins, dydx(us_hq) at(n_ssfp = (0(1)10))
marginsplot

	// no - we actually see a negative interaction effect - so the two seem to be substitutes rather than complements
    // above 5 previous startups, the funding premium of migration is no longer significant 	
	// to be interpreted with caution as not many startups with > 3 prior successes

	
	
// Q2

// a. run a probit model for the probability to migrate based on n_founders, prof, n_ssfp and industry dummies  
// comment on the significance and sign of the coefficients
	
probit us_hq n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond, r

	// larger founding teams are more likely to "migrate" to the US
	// the greater the number of prior successful startups founded, the higher the prob of migrating
	// there are signficant industry differences: startups in communications, IT software, life sciences and semiconductors are more prone to move their HQ to the US than firms in the baseline category (hardware)
	// firms founded by professors are as likely to migrate as firms with no professors in their founding teams
	// firms in the medical device industry and cleantech are no different in their prob of migrating compared to firms in hardware
	

// b. how would you measure the average marginal effect of n_ssfp on the likelihood to migrate to the US based on the probit? what would be the marginal effect estimate from a linear model? Compare.
margins, dydx(*) 
reg us_hq n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond, r

	// 0.023 (average marginal effect) vs 0.034 (LPM)

// c. Focus on the link between startup migration and funding as in Q1. Estimate ATET of us_hq using PSM and all the X variables that were found to be statistically significant above to construct matched samples

teffects psmatch (ln_raised) (us_hq n_founders n_ssfp comms it_software internet life_science semicond), atet

	// atet is 1.417
	// note that prof, cleantech and medical_dev were excluded
	
// d. balance

tebalance summarize

	// balance seems to have improved considerably except for semicond
	
// e. overlap

teffects overlap

	teffects psmatch (ln_raised) (us_hq n_founders n_ssfp comms it_software internet life_science semicond), atet gen(match)
	predict ps0 ps1, ps
	sum ps0 if us_hq == 1		// bet 0.17 and 0.99
	sum ps0 if us_hq == 0		// bet 0.50 and 0.99
		// region of common support is 0.50 and 0.99 (though we see in the plot less satisfactory overlap for 0.90+)
	

// f. When you submitted your analysis for journal publication, reviewer 2 of your study remains concerned with us_hq being endogeneous, despite your efforts to match on observables. But they do not specify why (which source of endogeneity is potentially at play here). Discuss briefly what could be their concerns and if and how the concerns could be addressed by an instrumental variable strategy. What would be the required properties of an instrumental variable for this strategy to work here?
	
	// room to discuss about potential self-selection into "treatment"/omitted variable bias
	// need to find at least one instrument that is relevant (significantly associated with us_hq) and valid (uncorrelated with the error term that remains after controlling for observables)
	

// Q3

// Reviewer 1 of your study suggests to run a panel data model with firm fixed effects, since these firms can be followed over time and funding can also change over time. Following the reviewer's suggestion, the results for this question are run on a set of data that covers the first 4 years (age 0, 1, 2, 3) of the lives of 2,096 startups. There are a total of 186 firms in the panel data set are "migrant startups" in the sense that they have relocated to the US before or at age 3.

// a. if performance has an unobserved timeinvariant component, and selection into migration depends on that component, then FE takes care of potential bias in the relation between migration and performance

/*
use migration_panel.dta, clear
xtset firm_id age
g earlymover = age_move == 0
xtreg ln_cum_raised i.age##i.earlymover i.industry_code if (age_move == . | age_move == 0) & age <= 3, fe r
*/   

/*
.                    
. xtreg ln_cum_raised i.age##i.earlymover i.industry_code if (age_move == . | age_move == 0) & age <= 3, fe r
note: 1.earlymover omitted because of collinearity.
note: 2.industry_code omitted because of collinearity.
note: 3.industry_code omitted because of collinearity.
note: 4.industry_code omitted because of collinearity.
note: 5.industry_code omitted because of collinearity.
note: 6.industry_code omitted because of collinearity.
note: 7.industry_code omitted because of collinearity.
note: 8.industry_code omitted because of collinearity.

Fixed-effects (within) regression               Number of obs     =      7,580
Group variable: firm_id                         Number of groups  =      1,895

R-squared:                                      Obs per group:
     Within  = 0.2471                                         min =          4
     Between = 0.1036                                         avg =        4.0
     Overall = 0.1177                                         max =          4

                                                F(6,1894)         =     134.10
corr(u_i, Xb) = 0.0861                          Prob > F          =     0.0000

                                (Std. err. adjusted for 1,895 clusters in firm_id)
----------------------------------------------------------------------------------
                 |               Robust
   ln_cum_raised | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-----------------+----------------------------------------------------------------
             age |
              1  |   .2144036   .0116772    18.36   0.000     .1915021    .2373052
              2  |   .3432571   .0148633    23.09   0.000     .3141069    .3724074
              3  |   .4509673   .0171808    26.25   0.000      .417272    .4846625
                 |
    1.earlymover |          0  (omitted)
                 |
  age#earlymover |
            1 1  |   .4993587   .1116276     4.47   0.000     .2804327    .7182847
            2 1  |   .7628553   .1206916     6.32   0.000     .5261528    .9995578
            3 1  |   .9220344   .1311628     7.03   0.000     .6647958    1.179273
                 |
   industry_code |
 Communications  |          0  (omitted)
       Hardware  |          0  (omitted)
  IT / Software  |          0  (omitted)
       Internet  |          0  (omitted)
  Life Sciences  |          0  (omitted)
Medical Devices  |          0  (omitted)
  Semiconductor  |          0  (omitted)
                 |
           _cons |   .2826638   .0103126    27.41   0.000     .2624385     .302889
-----------------+----------------------------------------------------------------
         sigma_u |  .67426116
         sigma_e |  .38544398
             rho |  .75369985   (fraction of variance due to u_i)
----------------------------------------------------------------------------------


b) expected increase in funding between a 1y old and a 3y old startups, if both stayed in Israel?

	0.45-0.21 = 0.24 - a 3y old stayer is estimated to raise approx 24% more money than a 1y old stayer

	the *extra* difference between 1yr -> 3yr for movers is
	0.922-0.499 = 0.423 
	
	a 3y old mover is estimated to raise approx 42% + 24% = 66% more money than a 1y old mover
	
	the gain is therefore much larger for movers than stayers (the interactions for age and earlymover are significant)
	
	
   
c) we cannot. we can only estimate differences between migrants and non-migrants as time elapses (their differences over time), given it is a firm FE model. we cannot obtain an estimate of the funding received by migrants at age 0, because "early mover" is timeinvariant. And although we could look at the constant term to get an estimate of the amount raised by non-migrants at age 0, the constant term in a FE model is also hard to intepret as it includes the average of all firm fixed effects. 

d) time invariant variables 

e) even if firms have been founded in different calendar years, their founding year is constant within firms, so we cannot estimate the coefficients of founding year in a FE model. 

f) pros of RE: estimates coefficients of time invariant variables, + efficient estimation
cons: too strong assumptions that are unlikely to be supported (X is not correlated with the unobserved heterogeneity, in this case, firm unobserved heterogeneity, which is unlikely to hold)

*/


		


// 4. heckman

// While VC funding is considered an intermediate performance outcome for startups, there is a considerable interest also in their ultimate "exit" outcomes, which include being acquired by another firm or going public via an IPO (initial public offering). 

use AERexam_cross.dta, clear

// Consider again the full cross section of startups that you will find in the data file AERexam_cross.dta.dta

// a. What is the proportion of startups that are being acquired by other firms? How does this proportion vary with the migration status of the startup?
tab us_hq acquired, row

	// 23% are acquired; this proportion is 19% for non-migrants and 61% for migrants

// b. The variable exit_amount records the amount obtained when a startup is acquired (in Millions of US dollars). What is the average amount obtained by acquired startups?
tabstat exit_amount, by(acquired)

// or simply
sum exit_amount

	// nearly 78 million US dollars

// c. Run an OLS regression of exit_amount on  us_hq  n_founders prof n_ssfp and industry dummies. What is the problem in interpreting the coefficient of us_hq in this regression as an estimate of the effect of migration on "exit_amount"? 

reg exit_amount us_hq  n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond , r

	// positive coef of us_hq
	// us_hq may affect probability of being acquired in the first place
	// we may obtain a positively biased estimate
	
// d. We need an estimation strategy that can possibly correct for the fact that exit_amount is only observed for acquired firms. For this, reviewer 2 suggests to take a closer look at two variables available in your data set: incubator and has_us_inventors. The reviewer argues that these variables should be related to the likelihood that a startup is acquired by another firm. Follow the reviewer's suggestion and investigate if this strategy can provide a solution to the problem of estimating the causal effect of migration on exit amounts. What do you conclude?
	
heckman exit_amount us_hq  n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond, select(acquired = us_hq  n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond incubator has_us_inventors)

// or
 
heckman exit_amount us_hq  n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond, select(acquired = us_hq  n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond incubator has_us_inventors) twostep

	// us_hq has a smaller coefficient now, and us_hq is a significant predictor of selection into the sample (i.e. being acquired)


	
// e) sample  selection does not seem to be significant (either based on the non-signif coefficient of IMR or the non-significance of rho)
	// yet one could have doubts regarding the validity of the exclusion restrictions. They are significantly predictors in the selection equation but 
	// are they unrelated to the main outcome of interest?

	reg exit_amount us_hq  n_founders prof n_ssfp cleantech comms it_software internet life_science medical_dev semicond incubator  has_us_inventors, r
test incubator has_us_inventors
	// if we include these exclusion restrictions in the outcome equation, we see no significant association between them and exit_amount




