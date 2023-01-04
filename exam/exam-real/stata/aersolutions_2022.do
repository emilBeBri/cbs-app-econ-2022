cd "C:\Users\hck.si\Dropbox (CBS)\Z-drev\Undervis\PhDeconometrics\2022\x\113886-V1\Data"


cd "/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/exam-real"

use input/stackcity, clear

* Question 1

* a) descr stats
bysort edgrp: su dmfwg28 
	* largest decline for edgrp 1 (dropouts)
ttest dmfwg28 if edgrp == 2 | edgrp == 4, by(edgrp) uneq
	* yes, difference is statistically significant
	
* b) pc use
egen urate1980_median = median(urate1980)
g urate_below = urate1980 < urate1980_median
* note: obs of pc use are repeated, averages are the same but std differ. Choose any edgrp
ttest pcemp_m_c2000 if edgrp == 1, by(urate_below)  uneq
	* if obs are repeated: std error under estimated
	* we conclude that pc adoption was higher in cities with lower initial levels of unemployment 

* c) male/female wage gap
pwcorr dmfwg28 pcemp_m_c2000 if edgrp == 1, sig obs
pwcorr dmfwg28 pcemp_m_c2000 if edgrp == 2, sig obs
pwcorr dmfwg28 pcemp_m_c2000 if edgrp == 3, sig obs
pwcorr dmfwg28 pcemp_m_c2000 if edgrp == 4, sig obs
pwcorr dmfwg28 pcemp_m_c2000 if edgrp == 5, sig obs
	* negative pairwise correlations between the two variables, in line with the B&L conjecture
	* less significant for edgrp 4 (college educated)

* repeat the above for the gap in returns to education bet College and HS workers
pwcorr drtc28 pcemp_m_c2000 if edgrp == 1, sig obs
	* correlation is now positive and significant in line with their conjecture

* d) main regression:
reg dmfwg28 pcemp_m_c2000 i.edgr
reg dmfwg28 pcemp_m_c2000 i.edgr, r
reg dmfwg28 pcemp_m_c2000 i.edgr, cluster(cmsa99)
test 2.edgr 3.edgr 4.edgr 5.edgr
	* clustering has no major effect, could use robust se
	* yes, there are statistically significant differences between educ groups
	* overall significance
	* all groups significantly different from reference group

* e) college/HS wage gap (returns to education)
* positive and significant (but quite volatile)
* note: effectively just 1 obs per cmsa99
reg drtc28 pcemp_m_c2000, r
reg drtc28 pcemp_m_c2000 i.edgrp, r
reg drtc28 pcemp_m_c2000 if edgrp == 1, r
	* positive and significant coefficient of PC/worker but p > 0.05 when we use just one obs per city
	* we should use only 230 obs, 1 per city

* f)
reg dmfwg28 c.pcemp_m_c2000##i.edgrp, r
* testing parallel lines: OK
test 2.edgrp#c.pcemp_m_c2000 3.edgrp#c.pcemp_m_c2000 4.edgrp#c.pcemp_m_c2000 5.edgrp#c.pcemp_m_c2000
* no significant difference between groups
margins, dydx(pcemp_m_c2000) at(edgrp=(1(1)5))
marginsplot
* no significant difference between groups, CI overlap with each other

* g) back to parallel line model, make it the main case
reg dmfwg28 pcemp_m_c2000 i.edgrp, r
reg dmfwg28 pcemp_m_c2000 i.edgr durmf1980 nondmf1980 durmf1980lo nondmf1980lo, r
test durmf1980 nondmf1980 durmf1980lo nondmf1980lo
	* the variables are jointy significant and introducing them reduces the coefficient of PC use from -0.224 to -0.175
	
	

* Question 2

*egen pcemp_m_c2000_median = median(pcemp_m_c2000)
*g HIGHpcemp_m_c2000 = pcemp_m_c2000 > pcemp_m_c2000_median

*a) 
probit HIGHpcemp_m_c2000 urate1980  pctbl1980 lnlf1980 erate_f1980lo erate_f1980hi if edgrp == 1, r
	* cities with larger shares of black workers (or residents?) had more modest PC adoption (they were less likely to be among the cities with high PC adoption)
	* in contrast, cities with larger labor forces in 1980 and with higher less-educated female participation in the labor market, had high PC adoption (i.e. these cities were more likely to have a PC adoption intensity above the sample median)
	* the remaining variables (initial unemployment rate and high-educated female participation in 1980 were not significantly related with high-PC adoption at the city level)

*b) 
margins, dydx(erate_f1980lo)
margins, dydx(erate_f1980lo) atmeans
	* based on the latter, if the female participation (of less educated women) would increase 10 percentage points (0.1) from its sample mean value (0.523), the city's estimated probability of high PC adoption would increase by 0.162 p.p.
	
*c)
teffects psmatch (dmfwg28) (HIGHpcemp_m_c2000 urate1980  pctbl1980 lnlf1980 erate_f1980lo erate_f1980hi) if edgrp == 1, atet
teffects psmatch (dmfwg28) (HIGHpcemp_m_c2000 urate1980  pctbl1980 lnlf1980 erate_f1980lo erate_f1980hi) if edgrp == 1, nn(3) atet
reg dmfwg28 HIGHpcemp_m_c2000 urate1980  pctbl1980 lnlf1980 erate_f1980lo erate_f1980hi if edgrp == 1, r

	* ATET = -0.045 for dropouts and -0.052 when using a min of 3 neighbors; trade-off between bias and variance
	* from OLS, estimated change in gender wage gap for school dropouts is -0.046 (very similar to PSM with 1 nn)
	* in conclusion, the male-female wage gap among school dropouts decreased by 4 to 5 p.p. more in cities with high PC adoption (compared to cities with lower PC adoption) 
	* it is important to control for/match on those initial characteristics because cities with high PC adoption may be different from cities with lower PC adoption, and neglecting those pre-existing differences may bias (upwards or downwards) the estimated association between HIGHpcemp_m_c2000 and dmfwg28
	* e.g. without controls
	reg dmfwg28 HIGHpcemp_m_c2000 if edgrp == 1, r
  
*d)   
teffects psmatch (dmfwg28) (HIGHpcemp_m_c2000 urate1980  pctbl1980 lnlf1980 erate_f1980lo erate_f1980hi) if edgrp == 1, nn(3) atet

tebalance summarize
* balance has improved for unemployment and % black ppl, but there is still room for improvement on the other variables
* we could try to change the specification of the propensity score estimation (e.g. with interaction terms) or, if we could, gather more data capturing other (unobserved) differences between the two groups that may also affect the trajectories in gender gaps and include them in the analysis

*e)
teffects overlap
* enough density around the full range of values, overlap condition seems ok
  
  
  
* Question 3	
* IV 
* a)
* two issues here: is it ok to have a relationship between a change (in the gender wage gap) and the (end-point) level of PC use? PC use in 1980 was essentially zero, so PC use in 2000 is essentially also a change. So OK for that. The second concern is endogeneity: reverse causality/simultaneity - the explanatory variable is measured at the end of the period considered, thus any unobserved shock happening over the period that affected both the wage gap and PC use could be causing endogeneity. Or it could be reverse causality, it is changes in male/female wages that affect the relative costs of PCs and other inputs into production.

*b) 
ivreg2 dmfwg28 (pcemp_m_c2000=lncs1980) durmf1980 nondmf1980  if edgrp == 4, r first
* -0.502, numerically higher than in OLS

*c) 
twoway scatter pcemp_m_c2000 lncs1980 if edgrp == 4
reg pcemp_m_c2000 lncs1980 if edgrp == 4, r 
test lncs1980
	* instrument is relevant: figure shows (positive) correlation with explanatory variables, confirmed by numerically large t- and F-tests in first-step regression 
	* validity cannot be tested. just-identified model does not allow to test for overidentifying restrictions


* d)
* more instruments
* agglomoration effects first
* fairly strong instrument, overid test ok
ivreg2 dmfwg28 (pcemp_m_c2000=lncs1980 lnlf1980) if edgrp == 4, r first

* share of immigrants (from Mexico): not a valid instrument
ivreg2 dmfwg28 (pcemp_m_c2000=lncs1980 lnlf1980 mexish1980) if edgrp == 4, r first
* include as control? IV estimate not much affected
ivreg2 dmfwg28 (pcemp_m_c2000=lncs1980 lnlf1980) mexish1980 if edgrp == 4, r first

*e) the DVs used by the authors only account for employed individuals, who may be different in unobservables from those out of the labor market. if people select into the labor market differently based on their gender or education, the wage differentials captured by the DVs used in this study may be confounded by these selection effects. if on top PC adoption intensity is correlated with this selection, the estimates of interest may be biased. if the authors would have data on people out of the labor market, they could have estimated wages based on an heckman selection model, and obtain "corrected" measures of city-level wage gaps, net of selection effects.  

* Question 4
* no need for coding, output provided
* just to show how the output was produced

* diff in diff
* annual changes for comparability pre/post

use citypanel, clear

* a) 2 x 2
su dmfwga if per == 0 & highimm == 0
su dmfwga if per == 0 & highimm == 1
su dmfwga if per == 1 & highimm == 0
su dmfwga if per == 1 & highimm == 1
* difference between per1 and per0 for low immigration cities
di -.001361--.0027016
* different between per1 and per0 for high immigration cities
di  -.000507--.0050217
* did: 
di .0045147-.0013406
* or
di (-.000507--.0050217)-(-.001361--.0027016)
* did = 0.0031741
* overall, we see that gender wage gaps decreased over time
* the decrease in GWG was larger in the first sub-period than in the second-one, i.e. the reduction in GWG slowed down over time (in both high and low immig cities)
* in the first sub-period, the GWG exhibited a more pronounced decline in high-immig cities than in low-immig cities
* the slowdown in the decline of the GWG was more pronounced in high-immig cities than in low-immig cities 
 
* b)
reg dmfwga i.per##i.highimm, cluster(cmsa99)

* a) the constant gives us by how much the gender gap changed during the period 1980-2000 in low-immigration cities
* b) yes, there was a sign difference between low- and high-immigration cities in mfwg: the yearly average change in gender wage gaps was more pronounced in high-immig cities than in low-immig cities; high-immig cities compressed the gender gap by 0.2 extra pp. compared to low-immig cities - we look at the coeff of "highimm"
* c) the change of the mfwg in low-immigration cities was not significantly different from per0 to per1 - we look at the coeff of "per". The coeff is positive, meaning that the decline in GWG slowed down over time in these cities, but this "slowdown" is n.s.
* d) mfwg d-i-d positive but only marginally significant - we look at the interaction term; it suggests that the decline in GWG observed in the data slowed-down a bit more in high-immig cities than in low-immig cities (in line with the stats in a)). 


* c) 
reg drtca i.per##i.highimm, cluster(cmsa99)
* rtc d-i-d not significant
* returns to education were increasing more in high-immig cities compared to low-immig cities pre-2000
* this increase in returns to education however slowed down for low-immig cities in the second period
* the slowdown from 1980-2000 to 2001-2010 was not any different for high vs low-immig cities

* overall, the results do not seem to support that immigration flows were a common cause of changes in the male-female wage gap and the returns to education.


