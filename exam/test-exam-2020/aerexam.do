set more off 
set logtype text
cap log using AERexamlog.text,replace 

use caresdata.dta, clear
de

**************
*     Q1     *
**************
* a)
summarize perday, detail
//histogram(perday)
//histogram(perday),normal

/*
Mean = 14.07317
Min = 1 
Max = 60

From the histogram, we see that there are spikes around the values of 20 and 10
This means that most of the people in the dataset are smoking 10 or 20 cigrattes perday
By fitting in the normal graph we into the histogram we can see there is a fat tail on the 
left hand side

The variance of the variable perday is 95.5 while the Mean is 14.1, that is around 
7 times higher, this is signaling overdispersion in the variable

*/


* b)
summarize perday if female == 0 //Men
summarize perday if female == 1 //Women
ttest perday, by(female)

/*
Mean of cigrattes smoked perday by men = 14.34711
Mean of cigrattes smoked perday by women = 10.32075
By observation, the mean number of cigrattes smoked by men perday is 4 cigrattes 
more than women

We run a t-test based on the gender and test whether the mean is the same
We test:
h0 = Difference in mean number of cigrattes smoked by men and women is 0
We REJECT H0 at alpha = 0.01
We also test 
H0 = Differnce in mean number of cirattes smoked by men and women > 0 
We REJECT H0 at alpha = 0.01
This supports the claim smoking is more prevalent among men than women 
*/


*c)
poisson perday female age45 so_addicted want_quit
poisson, irr
estat gof 

nbreg perday female age45 so_addicted want_quit

/*
Since we suspect there is overdispersion in the variable perday, poisson count model
would not be a goodfit since it is used when the Mean(DV) = Var(DV). And indeed this is the
case when we test the goodness of fit for the poission count model and reject H0 of model 
being a goodfit at alpha = 1%

A good model for suspected overdispersion would be negative binomial since 
mean(DV) << Var(DV). We run the negative binomial and test whether alpha = 0. We reject
the H0 that alpha is 0. This tells us that there is indeed overdispersion in the dependent 
variable and negative binomial is a bette model.

Zero inflated model is not considerd because there are no 0s in the DV

*/

*d)
regress lnperday female age45 so_addicted want_quit,robust 

/*
OLS regression with robust SEs indicate that the R2 is 0.1941. This suggests that around 1/5
of smoking variations in the data is explained by this particular model. 

The coefficient of want_quit is a binary variable that equals to 1 if the respondant 
wants to quit smoking at a certain time point in life. The coefficient is -0.2143699.
This suggests holding all other factors constant, respondants who say they want to quit 
smoking at a certain time point in life smoke (exp(-.2143699)-1)*100 = -158.2717% less


*/

*e)
regress lnperday female age45 so_addicted want_quit i.female#i.want_quit 


**************
*     Q2     *
**************
* a) 
/*

If want_quit is indeed related to some unobserved characteristics of the respondant,
then the coefficient of the OLS output for want_quit will be biased since OLS assumtion
of cov(Xi,ui) = 0 is violated. In general, all coefficients estimated would be biased. 

*/

* b) 
ivreg2 lnperday (want_quit = avoid quit_6mos)female age45 so_addicted

/*
The coefficient estimation of want_quit after the introduction of IV varibles is 
-.134293. The variable is now however, insignificant
*/

* c)
*Relevance Test*

pwcorr want_quit avoid quit_6mos,star(0.01)
regress want_quit avoid quit_6mos female age45 so_addicted 
test avoid quit_6mos 

*Validity Test*
ivreg2 lnperday (want_quit = avoid quit_6mos)female age45 so_addicted
overid 

ivreg2 lnperday (want_quit = avoid)female age45 so_addicted
overid 

ivreg2 lnperday (want_quit = quit_6mos)female age45 so_addicted
overid 

/*
The correltion matrix shows that IVs are indeed correlated with want_quit 
By running a regression and testing the joing significance, we can also 
see that they are indeed relevant

Howver, by running the overidentification test, we can see when put into the 
model at the same time, the H0 is rejected, indicating at least one of the IV is not
valid. When ran separtely, the H0 is NOT rejected. This suggests using either one 
of them should be suffice. Using avoid as IV gives a singificant estimation for 
want_quit but not when quit_6mos is used as IV

*/

**************
*     Q3     *
**************

* a)
tab takeup passedtest 
/*
% passing urine tax without CARES = 13%
% passing urine tax WITH CARES = 69%

We see that the percentage of people passing urine tax while joining CARES is 
much higher than those who passed without joining CARES. This seems to suggest
that joining CARES increases the urine test passing rates. 

HOWEVER, there are several problems here
1) Only 42 out of 408 particpated in the CARES program
2) The choice of joining CARES was self-selection => It might be those who joined
were fundamentally differnt than those who didn't join, explaining the differnce 
in the passing rate. E.g. More motivated to quit, have a family etc. 

*/

* b)
logit takeup female age age2 perday smellsmoke want_quit if cares==1 
margins, dydx(want_quit)
margins, dydx(want_quit) atmeans

/*
 The marginal effect is 0.922566 on takeup or 0.0864149 when other variables are 
 fixed at their mean value. Both coefficients are significant at alpha = 5%
 
 We can see at alpha = 10%, age,age2,smellsmoke and want_quit are significant 
 predictors of takeup
*/

* c)
teffects psmatch (passedtest)(takeup female age age2 perday smellsmoke want_quit),atet
/*
 The coefficient of ATET is .5875 and is significant at alpha = 1%. This suggests 
 that takeup does have an effect on passing the urine test

*/

* d)
teffects psmatch (passedtest)(takeup age age2 smellsmoke want_quit),gen(match)
tebalance summarize 
//tebalance density age
//tebalance density age2
//tebalance density smellsmoke 
//tebalance density want_quit 
/*
 Ideally, the balanced covaraite would have a standardized differnce 
 of 0 and a variance ratio of 1. This however is NOT the case, indicating
 a poor balance.
 
 We can also plot the denisty curves for the variables and see that the 
 balancing condition is not met. 

*/

* e)
predict ps0 ps1,ps
//teffects overlap 

/*
As seen from the graph output, it seems that the overlap condition is violated 
since the density of the graphs are not concentrated in the region in which
they overlap. 

*/

* f)

/*
Personally, I don't trust the estimate of CARES-deposit treatment effect from the
PSM as an estimate of causal effect. Despite the coefficient being singficantly postive
(Which is the direction we expect it would be), we can see that the balacing codition 
and overlap assumtion were not strictly met. Hence, I would not trust the estimate

*/

**************
*     Q4     *
**************
* a) 
tab tooktest if cares ==1 
/*
7.69%
*/

* b)
tab tooktest want_quit if cares==1 
tab tooktest quit_6mos if cares==1
tab tooktest smellsmoke if cares==1
tab tooktest avoid if cares==1
tab tooktest so_addicted if cares==1
/*
 OLS assumes that data is selected at random. If the missing variables are indeed
 missing at random, then there won't be a problem. However, if they are not missing 
 at random, then the estimated coefficients would be biased since the random sample 
 selection assumption is violated. 
 
 As for the bias, it is unclear what the direction would be. For example, it might well
 be those who didn't take the test knew for sure they won't pass, this would overestimate 
 the treatment effect. It might also be that those who didn't take they test were those 
 that were successful in quiting smoking and the treatment effect would than be underestimated
 
 Crosstabulating tooktest with some of the variables such as want_quit quit_6mos etc shows that 
 it is not clear which direction the bias would be going since those 34 missing individuals have 
 similar underlying characteristics 
*/ 

* c)
/* 
 The reason for using phone_nr as an exclusion restriction could be individuals 
 who gave their phone number at the initial interviews were those who were more motivated 
 to quit smoking since giving out phone number would mean they would be contacted again
 
 Those who didn't give their phone number, were probably mot the motivated ones, 
 and they joined the program for whatever reason. e.g. forced by family members. 
 
 We see from the output that rho is 0.6328373 and the hypothesis test of 
 H0: rho = 0 is not rejected. This suggests that OLS would be consistent since 
 sample selection bias is not a problem. 
 
 This suggests that the missing observations are not a concern
 
*/



log close
