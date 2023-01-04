* prepare data

cd /home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/
do "stata/data-preprocessing-workshop-first-example-v01.do"


##############  #section ################
# OLS
#########################################



reg prodnew extsource rdintpct inconst lempl00 
reg prodnew extsource rdintpct inconst lempl00, robust


reg prodnew extsource rdintpct inconst lempl00 
*Now it's easier to see the significant effect if R&D intensity - as we increase the R&D intensity by 1pp, the share of turnover from new products will increase by 0.82pp.
*An increase by one std deviation of extsource will increase prodnew by 1.78pp - but it's not significant at the 5% level.
*The effects of size (and internal constraints) are not significant.



##############  #section ################
# f-test
#########################################



//F-tests for individual significance of each variable
test rdintpct 
testparm rdintpct inconst
test extsource
test lempl00



// F-Test

* Let us now compare both models based on the F-test for restricted vs unrestricted models.
* We first need to run the unrestricted (full) model to obtain the SSRur
* Then we do the same for the restricted model 
* Finally we apply the formula as in Wooldridge (2012)

// Unrestricted model
reg prodnew extsource rdintpct inconst lempl00
// Sum of Squared Residuals of the Unrestricted Model (SSRur)
scal ssr_ur = e(rss)
// Restricted model
reg prodnew extsource rdintpct if insample
// Sum of Squared Residuals of the Restricted Model (SSRr)
scal ssr_r = e(rss)
// F-Test formula (as in Wooldridge, 2012)
scal F = ((ssr_r - ssr_ur) / 2) / (ssr_ur / (431-4-1))

display F // We get the same result as in the joint test for inconst and lempl00.


// Rerun full model before hettest
reg prodnew extsource rdintpct inconst lempl00

// Test for heteroskedasticity
hettest 
* There is heteroskedasticity (We reject the null of Homoskedasticity due to P smaller than 0.05)
* Let us run the regressions again while controlling for heteroskdasticity

// Full model (correcting for heteroskedasticity)
reg prodnew extsource rdintpct inconst lempl00 if insample, robust
// Joint significance test
test inconst lempl00 
// Restricted model (correcting for heteroskedasticity)
reg prodnew extsource rdintpct if insample, robust
* We now see that the coefficient of extsource becomes more significant again
* This is because, after controlling for heteroskedasticity, the standard error is now smaller
* We prefer these models because we are correcting for heteroskedasticity



##############  #section ################
# interaction effects
#########################################

gen rdintpct_extsource = rdintpct*extsource
reg prodnew extsource rdintpct rdintpct_extsource inconst lempl00,r

* we want to make sure that the following transformations are done on the estimation sample
keep if e(sample) == 1


// Centering around the mean (using the regression sample)
egen avgrdintpct = mean(rdintpct)
gen dm_rdintpct = rdintpct-avgrdintpct
egen avgextsource = mean(extsource)
gen dm_extsource = extsource-avgextsource
gen dm_rdintpct_extsource = dm_rdintpct*dm_extsource

// Regression with centered explanatory variables 
reg prodnew extsource rdintpct dm_rdintpct_extsource inconst lempl00, r
test dm_rdintpct_extsource

// using margins
// note the notation c. for continuous variables
// # denotes an interaction term
// ## produces the full set of main and interaction effects
reg prodnew c.rdintpct##c.extsource inconst lempl00, r

reg prodnew c.rdintpct##c.extsource, r


margins, dydx(extsource)


margins, dydx(extsource) atmeans
marginsplot
margins, dydx(extsource) at(rdintpct = (0(2)70))
graph export output/stata-graph-tmp-out.png, replace
marginsplot
graph export output/stata-graph-tmp-out.png, replace

hist extsource
graph export output/stata-graph-tmp-out.png, replace



##############  #section ################
# logit/probit 
#########################################




probit 
logit 


logit prodinov extsource rdintpct inconst lempl00
logit prodinov extsource inconst lempl00
margins, dydx(*)
margins, dydx(*) atmeans







reg  prodinov extsource rdintpct inconst lempl00, robust


margins, dydx(extsource) at(rdintpct = (0(2)70))
marginsplot

margins, predict(*) at (rdintpct)



patapply


pat


summary prodinov 


sum patapply, detail

tabstat patapply, stats(mean sd v n)



histogram patapply, discrete freq
graph export test.png, replace

histogram patapply, saving(scatter1, replace)
histogram patapply if patapply < 50
graph export test.png, replace

histogram patapply, discrete freq

tab patapply

drop if patapply< 0


tab market
* market = 


help poisson



poisson patapply lempl00 rdintpct i.pcoop i.market, robust
poisson, irr
margins market,  atmeans 
margins pcoop,  atmeans 



margins market,  atmeans at() 

margins market if pcoop == 1, atmeans at(rdintpct = (0(2)70))
marginsplot
save
* slide 37 - linking functtion
exp()


tab market if rdintpct != .

exp() 

