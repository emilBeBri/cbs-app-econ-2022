


*****************************************************************************************************************************
**** EXAMPLES BASED ON CARD AND KRUEGER DATA ON CANVAS


cd "/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/"


* change directory to the relevant one 
* cd ...

* cap log using CK1994_example.smcl , replace


use 08-diff-in-diff/ck1994.dta, clear

**# GENERATE VARIABLES
gen treated=state
lab var treated "NJ = 1; PA = 0"
lab def treated 1 "NJ" 0 "PA"
lab value treated treated

gen post=time
lab var post "Feb.92 = 0; Nov. 1992 = 1"
lab def time 0 "Pre" 2 "Post"
lab value post post

gen id=store
lab var id "Restaurant ID"

gen fte=empft
lab var fte "Output: Full Time Employees"

lab var chain "Burger King = 1; KFC = 2; Roys = 3; Wendy's = 4" 
lab def chain 1 "Burger King" 2 "KFC" 3 "Roys" 4 "Wendy's" 
lab value chain chain

sort id post
xtset id post

d id treated post fte chain 

* Drop stores with missing outcomes
bys id: egen _fte=count(fte)
drop if _fte!=2


* save data for use in R
save 08-diff-in-diff/ck1994-modificed-by-script.dta, replace

list id fte treated post in 1/10, sep(2) nolabel

**# EXPLORING PRE-POLICY CHARACTERISTICS OF THE GROUPS

* The idea is to verify whether the two groups are comparable.
tab chain if  treated ==1 & post ==0
/*
Burger King |
 = 1; KFC = |
  2; Roys = |
 3; Wendy's |
        = 4 |      Freq.     Percent        Cum.
------------+-----------------------------------
Burger King |        129       40.82       40.82
        KFC |         68       21.52       62.34
       Roys |         79       25.00       87.34
    Wendy's |         40       12.66      100.00
------------+-----------------------------------
      Total |        316      100.00

*/
tab chain if  treated ==0 & post ==0
/*

Burger King |
 = 1; KFC = |
  2; Roys = |
 3; Wendy's |
        = 4 |      Freq.     Percent        Cum.
------------+-----------------------------------
Burger King |         34       44.74       44.74
        KFC |         12       15.79       60.53
       Roys |         17       22.37       82.89
    Wendy's |         13       17.11      100.00
------------+-----------------------------------
      Total |         76      100.00
*/

**# ATE ESTIMATOR AND COMPARE MEANS

*1) Average outcome (effect) of the treated after the treatment
sum fte if treated ==1 & post==1
scalar y_tpost = r(mean)

*2) Average outcome (effect) of the treated before the treatment
sum fte if treated ==1 & post==0
scalar y_tpre = r(mean)

*3) Average outcome (effect) of the control group before the treatment
sum fte if treated ==0 & post==1
scalar y_cpost = r(mean)

*4) Average outcome (effect) of the control group before the treatment
sum fte if treated ==0 & post==0
scalar y_cpre = r(mean)

* DiD estimator, as the Average Treatment Effect:
di (y_tpost - y_tpre) - (y_cpost - y_cpre)

* >> 3.4427881 

*** REGRESSION APPROACH 
reg fte i.treated##i.post, robust
/*
Linear regression                               Number of obs     =        784
                                                F(3, 780)         =       1.56
                                                Prob > F          =     0.1970
                                                R-squared         =     0.0084
                                                Root MSE          =     8.3213

------------------------------------------------------------------------------
             |               Robust
         fte |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
     treated |
         NJ  |    -2.6006   1.319187    -1.97   0.049    -5.190177   -.0110226
      1.post |  -2.743421   1.578217    -1.74   0.083    -5.841476     .354634
             |
treated#post |
       NJ#1  |   3.442788   1.700103     2.03   0.043     .1054694    6.780107
             |
       _cons |   10.31579   1.239793     8.32   0.000     7.882063    12.74952
------------------------------------------------------------------------------

	N.B the DiD estimates is the same as the Average Treatement Effect computed as 
	(y_tpost - y_tpre) - (y_cpost - y_cpre)
*/

*** ADDING COVARIATES
reg fte i.treated##i.post i.chain, robust
/*
. reg fte i.treated##i.post i.chain, robust

Linear regression                               Number of obs     =        784
                                                F(6, 777)         =      12.19
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0607
                                                Root MSE          =     8.1144

------------------------------------------------------------------------------
             |               Robust
         fte |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
     treated |
         NJ  |   -2.30713   1.258303    -1.83   0.067    -4.777207    .1629459
      1.post |  -2.743421   1.509528    -1.82   0.070    -5.706658    .2198161
             |
treated#post |
       NJ#1  |   3.442788   1.632124     2.11   0.035     .2388929    6.646683
             |
       chain |
        KFC  |  -5.123389   .6552841    -7.82   0.000    -6.409726   -3.837052
       Roys  |  -1.690295   .7306062    -2.31   0.021    -3.124491   -.2560989
    Wendy's  |   -1.00196    1.05185    -0.95   0.341    -3.066764    1.062845
             |
       _cons |   11.67423   1.289992     9.05   0.000     9.141944    14.20651
------------------------------------------------------------------------------

	N.B. DiD estimates stays the same! 
	This is a common robustness check in DiD design.

*/

cap log close


*****************************************************************************************************************************
**** EXAMPLES BASED ON THE PRISON DATA ON CANVAS

* log using prison_example.smcl, replace

use 08-diff-in-diff/prison.dta, clear

**# GENERATE VARIABLES
* "before" vs "after" dummy
gen post = (year>=86)
lab var post "0 = Before treatment; 1 = After Treatment"

* "treated" vs "control" dummy
gen treated = (state>=17 & state<=51)
lab var treated "0 = Control states; 1 = Treated States"

gen did=treated*post
lab var did "DiD estimator"

d pris treated post did state year 
* save for use in R
save 08-diff-in-diff/prison-modificed-by-script.dta, replace

**# GRAPHICAL EXPLORATION FOR PARALLEL TRENDS

* A) Plot raw data to visually test parallel trends
bysort year: egen pris0=mean(pris) if treated==0
lab var pris0 "Control group"
bysort year: egen pris1=mean(pris) if treated==1
lab var pris1 "Treatment group"

twoway (connected pris0 pris1 year), xline(86) ytitle("Prisoners per 100,000 inhab.") xtitle("Year") xlab(80(1)93)
graph export "stata/diff-in-diff-lecture-trends1.png", as(png) replace

	* The graph reveals a similar time pattern for both the control and the treatment group for the period previous to the introduction of the new policy.


* B) Alternative way to plot the trends: saturated model and margins
reg pris i.year##i.treated
margins treated, at(year=(81(1)93)) vsquish
marginsplot, noci xline(86) xlab(80(1)93) ytitle("Prisoners per 100,000 inhab.") xtitle("Year") title("")
graph export "stata/diff-in-diff-lecture-trends2.png", as(png) replace

	/* 
	N.B. trends1 and trends2 look the same: the second approach is very helpful when we need to test parallel trends conditional on observables 
	(that is, controlling for other covariates)
	*/


**# TESTING PARALLEL TRENDS THROUGH FULLY SATURATED MODEL REGRESSIONS

* A) Fully saturated model: 
reg pris i.treated##i.year, cluster (state)

/*

Linear regression                               Number of obs     =        714
                                                F(27, 50)         =     150.08
                                                Prob > F          =     0.0000
                                                R-squared         =     0.2298
                                                Root MSE          =     118.74

                                 (Std. Err. adjusted for 51 clusters in state)
------------------------------------------------------------------------------
             |               Robust
        pris |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
   1.treated |  -26.95774   23.03849    -1.17   0.248     -73.2319    19.31642
             |
        year |
         81  |   7.902258   2.572847     3.07   0.003     2.734542    13.06997
         82  |   26.54346   4.055156     6.55   0.000     18.39844    34.68848
         83  |   48.14177   7.273863     6.62   0.000     33.53179    62.75176
         84  |   59.75509   8.602639     6.95   0.000     42.47618      77.034
         85  |   69.97154   11.12025     6.29   0.000     47.63585    92.30722
         86  |   90.56271   18.27964     4.95   0.000     53.84698    127.2784
         87  |      105.2   19.39936     5.42   0.000     66.23521    144.1648
         88  |   126.9169   26.52769     4.78   0.000     73.63443    180.1993
         89  |   149.3067   35.73909     4.18   0.000      77.5226    221.0907
         90  |   173.1955    34.7398     4.99   0.000     103.4186    242.9725
         91  |   192.2164   37.49753     5.13   0.000     116.9004    267.5324
         92  |   208.9624   41.13044     5.08   0.000     126.3495    291.5753
         93  |   227.9274   45.63096     5.00   0.000     136.2749    319.5798
             |
treated#year |
       1 81  |  -2.929324   3.026349    -0.97   0.338    -9.007925    3.149277
       1 82  |  -10.55925   4.564066    -2.31   0.025    -19.72644   -1.392048
       1 83  |  -17.24801   8.315161    -2.07   0.043     -33.9495   -.5465155
       1 84  |  -20.72339   10.23496    -2.02   0.048    -41.28092   -.1658684
       1 85  |  -22.66707   12.78809    -1.77   0.082     -48.3527     3.01856
       1 86  |  -32.96124   19.38873    -1.70   0.095    -71.90466     5.98218
       1 87  |  -35.97616    21.1049    -1.70   0.094    -78.36659    6.414272
       1 88  |  -49.44072   27.81833    -1.78   0.082    -105.3155    6.434047
       1 89  |  -60.45866   36.97279    -1.64   0.108    -134.7207    13.80338
       1 90  |   -64.1597   36.27455    -1.77   0.083    -137.0193    8.699881
       1 91  |  -70.29631   39.17234    -1.79   0.079    -148.9763    8.383664
       1 92  |  -73.59571   43.01121    -1.71   0.093    -159.9863    12.79485
       1 93  |   -82.8252   47.50028    -1.74   0.087    -178.2323    12.58192
             |
       _cons |   138.9538   20.97328     6.63   0.000     96.82768    181.0798
------------------------------------------------------------------------------



		note that here the baseline year is 1980, which may make comparisons between pre- and post-policy difficult 
		nevertheless, we can see that prison rates are going up for control states (the year dummies are positive and statistically significant (p < 0.05) from 86 onwards)
		over time, we do see that the passage of time has a different effect on treated states (the increase in prison rates is lower for them), but everything is compared to 80. 
		post-86 we actually see that the coefficients tend to be n.s. (p > 0.05)
*/


* B) TESTING PRE-TRENDS VIA REGRESSION
/* 	In order to more clearly see the evolution of the outcome N years before, let's extend the model with pre-treatment dummies and
	consider only 3 years before and 3 years after the policy to make the output easier to digest */
gen years_post = (year - 86) if year >= 86
replace years_post = 0 if year <=86

gen years_pre = (86 - year) if year < 86
replace years_pre = 0 if year >= 86
reg pris i.treated##i.years_pre if year>= 83 & year <= 89 , cluster(state)

/*
. reg pris i.treated##i.years_pre if year>= 83 & year <= 89 , cluster(state)

Linear regression                               Number of obs     =        357
                                                F(7, 50)          =      13.52
                                                Prob > F          =     0.0000
                                                R-squared         =     0.1007
                                                Root MSE          =     108.57

                                      (Std. Err. adjusted for 51 clusters in state)
-----------------------------------------------------------------------------------
                  |               Robust
             pris |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
------------------+----------------------------------------------------------------
        1.treated |  -71.66694   45.43946    -1.58   0.121    -162.9348     19.6009
                  |
        years_pre |
               1  |  -48.02502   14.97244    -3.21   0.002    -78.09804   -17.95199
               2  |  -58.24146   17.17109    -3.39   0.001    -92.73061   -23.75231
               3  |  -69.85478   18.29084    -3.82   0.000     -106.593   -33.11654
                  |
treated#years_pre |
             1 1  |   22.04213   15.35685     1.44   0.157    -8.803008    52.88726
             1 2  |    23.9858   17.65608     1.36   0.180    -11.47747    59.44907
             1 3  |   27.46119   19.03627     1.44   0.155    -10.77429    65.69667
                  |
            _cons |   256.9503   42.93482     5.98   0.000     170.7132    343.1874
-----------------------------------------------------------------------------------



	- years_pre have significant coeff and these mean that, compared to 86, non-treated states had lower prison rates in the years just before (in line with the positive trend we see in the plot)
	
	- this trend was however not different for treated groups (the interaction terms between ds#years_pre are all n.s.)
	
	- we conclude that there is no significant pre-trend in this case
*/


* C) FULLY SATURATED MODEL for PRE-TREND TESTING, ADDING STATE FE
xtset state year
xtreg pris i.treated##i.years_pre if year>= 83 & year <= 89 , fe cluster(state)

/*

N.B. We obtain the same conclusion as before: 
note: 1.treated omitted because of collinearity

Fixed-effects (within) regression               Number of obs     =        357
Group variable: state                           Number of groups  =         51

R-sq:                                           Obs per group:
     within  = 0.3785                                         min =          7
     between = 0.0699                                         avg =        7.0
     overall = 0.0185                                         max =          7

                                                F(6,50)           =      15.51
corr(u_i, Xb)  = -0.0660                        Prob > F          =     0.0000

                                      (Std. Err. adjusted for 51 clusters in state)
-----------------------------------------------------------------------------------
                  |               Robust
             pris |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
------------------+----------------------------------------------------------------
        1.treated |          0  (omitted)
                  |
        years_pre |
               1  |  -48.02502   14.95103    -3.21   0.002    -78.05505   -17.99498
               2  |  -58.24146   17.14654    -3.40   0.001    -92.68131   -23.80162
               3  |  -69.85478   18.26469    -3.82   0.000    -106.5405   -33.16906
                  |
treated#years_pre |
             1 1  |   22.04213   15.33489     1.44   0.157    -8.758912    52.84317
             1 2  |    23.9858   17.63083     1.36   0.180    -11.42677    59.39838
             1 3  |   27.46119   19.00906     1.44   0.155    -10.71963      65.642
                  |
            _cons |   207.7671   2.548821    81.51   0.000     202.6477    212.8866
------------------+----------------------------------------------------------------
          sigma_u |  109.85421
          sigma_e |  30.799141
              rho |  .92712446   (fraction of variance due to u_i)
-----------------------------------------------------------------------------------
*/


**# DiD EXAMPLES  AND INTERPRETATIONS

* A) simple DiD regression, no FE
reg pris i.treated##i.post, cluster (state)

/*

Linear regression                               Number of obs     =        714
                                                F(3, 50)          =      44.58
                                                Prob > F          =     0.0000
                                                R-squared         =     0.1775
                                                Root MSE          =     120.61

                                 (Std. Err. adjusted for 51 clusters in state)
------------------------------------------------------------------------------
             |               Robust
        pris |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
   1.treated |  -39.31225   26.77102    -1.47   0.148    -93.08343    14.45893
      1.post |   123.9003   26.98897     4.59   0.000     69.69136    178.1092
             |
treated#post |
        1 1  |  -46.35971   27.96324    -1.66   0.104    -102.5255    9.806117
             |
       _cons |   174.3394   24.38248     7.15   0.000     125.3658    223.3131
------------------------------------------------------------------------------



	from here we confirm that 

	- after the policy change, non-treated states increased the population in prison by nearly 124 persons (per 100K inhabitants)

	- treated states, prior to the policy, already had lower imprisonment than non-treated (39 fewer people per 100K inhabitants),
	but this is n.s. when we cluster the s.e. at the state level

	- after the policy, treated states exhibited even lower levels of imprisonment than non-treated municipalities compared to before 
	(which we can see based on the larger vertical distance between the two lines), but again this effect is n.s. according to this regression

	- in other words, both treated and control states increased their prison rates, but treated states increased slightly less (123.9 - 46.36); 
	we could say that their positive prison rates slowed down after the policy was introduced; 
	however according to this regression, the policy was not effective


*/

* B) alternatively, using the diff command:
diff pris, treated(treated) period(post) cluster(state)

/*

	DIFFERENCE-IN-DIFFERENCES ESTIMATION RESULTS
	Number of observations in the DIFF-IN-DIFF: 714
				Before         After    
	   Control: 96             128         224
	   Treated: 210            280         490
				306            408
	--------------------------------------------------------
	 Outcome var.   | pris    | S. Err. |   |t|   |  P>|t|
	----------------+---------+---------+---------+---------
	Before          |         |         |         | 
	   Control      | 174.339 |         |         | 
	   Treated      | 135.027 |         |         | 
	   Diff (T-C)   | -39.312 | 26.771  | -1.47   | 0.148
	After           |         |         |         | 
	   Control      | 298.240 |         |         | 
	   Treated      | 212.568 |         |         | 
	   Diff (T-C)   | -85.672 | 52.324  | 1.64    | 0.108
					|         |         |         | 
	Diff-in-Diff    | -46.360 | 27.963  | 1.66    | 0.104
	--------------------------------------------------------
	R-square:    0.18
	* Means and Standard Errors are estimated by linear regression
	**Clustered Std. Errors
	**Inference: *** p<0.01; ** p<0.05; * p<0.1

*/


* C) WHAT IS THE EVOLUTION OF THE OUTCOME (AND POLICY EFFECT) n YEARS AFTER THE TREATMENT.

* Only effect after the policy, for each group of states, try: 
reg pris i.treated##i.years_post , cluster (state)

/*

Linear regression                               Number of obs     =        714
                                                F(15, 50)         =      31.56
                                                Prob > F          =     0.0000
                                                R-squared         =     0.2134
                                                Root MSE          =     118.96

                                       (Std. Err. adjusted for 51 clusters in state)
------------------------------------------------------------------------------------
                   |               Robust
              pris |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------------+----------------------------------------------------------------
         1.treated |  -42.25607   28.57561    -1.48   0.145    -99.65187    15.13973
                   |
        years_post |
                1  |   61.93186   12.50093     4.95   0.000     36.82301    87.04072
                2  |   83.64874   19.69039     4.25   0.000     44.09943     123.198
                3  |   106.0385   29.16796     3.64   0.001     47.45298    164.6241
                4  |   129.9274   28.26527     4.60   0.000     73.15496    186.6999
                5  |   148.9483    31.1193     4.79   0.000      86.4433    211.4532
                6  |   165.6943   34.79248     4.76   0.000     95.81153     235.577
                7  |   184.6592   39.31358     4.70   0.000     105.6956    263.6229
                   |
treated#years_post |
              1 1  |  -20.67783   13.54102    -1.53   0.133    -47.87577    6.520099
              1 2  |   -34.1424   20.45612    -1.67   0.101    -75.22971    6.944921
              1 3  |  -45.16033   29.95014    -1.51   0.138     -105.317    14.99629
              1 4  |  -48.86138   29.37937    -1.66   0.103    -107.8716    10.14882
              1 5  |  -54.99798   32.39587    -1.70   0.096     -120.067    10.07105
              1 6  |  -58.29739     36.266    -1.61   0.114    -131.1398    14.54502
              1 7  |  -67.52687   40.81254    -1.65   0.104    -149.5013    14.44752
                   |
             _cons |   182.2219   26.19161     6.96   0.000     129.6145    234.8293
------------------------------------------------------------------------------------

	- we see a common increase in prison rates over the years post-policy for non-treated states
	
	- for example, in 86 already (the year of the policy change, years_post == 1) non-treated states had almost 62 + persons per 100K 
	inhabitants in prison than before (i.e. the period 1980-85);
	
	- during this pre-policy period (80-85), treated states had 42 persons less (per each 100K inhab) in prison, but this difference 
	was not statistically significant;
	
	- based on the interaction effects, we see negative coefficients but none is statistically significant; this means that the positive trajectory 
	that we see in non-treated states after the policy was slightly muted in treated states (the increase in prison rates was smaller for them), 
	but this effect was not statistically significant.
*/

* Effect both before and after the policy, for each group of states, try: 

reg pris i.treated##i.years_post i.treated##i.years_pre if year>= 83 & year <= 89 , cluster (state)

/*

Linear regression                               Number of obs     =        357
                                                F(13, 50)         =      13.42
                                                Prob > F          =     0.0000
                                                R-squared         =     0.1117
                                                Root MSE          =     108.85

                                       (Std. Err. adjusted for 51 clusters in state)
------------------------------------------------------------------------------------
                   |               Robust
              pris |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------------+----------------------------------------------------------------
         1.treated |  -59.91898   39.08622    -1.53   0.132     -138.426      18.588
                   |
        years_post |
                1  |   14.63727   2.218082     6.60   0.000     10.18212    19.09242
                2  |   36.35415   8.914252     4.08   0.000     18.44935    54.25895
                3  |   58.74396   18.72665     3.14   0.003     21.13037    96.35755
                   |
treated#years_post |
              1 1  |   -3.01492   3.389226    -0.89   0.378     -9.82238     3.79254
              1 2  |  -16.47948   9.481491    -1.74   0.088    -35.52362    2.564654
              1 3  |  -27.49742   19.31209    -1.42   0.161     -66.2869    11.29206
                   |
         years_pre |
                1  |  -20.59117   8.149227    -2.53   0.015    -36.95938   -4.222969
                2  |  -30.80762      10.58    -2.91   0.005    -52.05818   -9.557056
                3  |  -42.42093    11.9499    -3.55   0.001    -66.42301   -18.41886
                   |
 treated#years_pre |
              1 1  |   10.29417   8.334959     1.24   0.223    -6.447086    27.03543
              1 2  |   12.23785   10.86806     1.13   0.266    -9.591288    34.06698
              1 3  |   15.71323   12.44591     1.26   0.213    -9.285112    40.71158
                   |
             _cons |   229.5165    36.6686     6.26   0.000     155.8654    303.1675
------------------------------------------------------------------------------------


	- now we see that treated states had lower prison rates (60 fewer prisoners/100K) than non-treated in 86 
	(the meaning of "treated" dummy here, alone, since we have pre- and post dummies; when both are 0, year = 86)
	
	- we continue to see an increase in prison rates for non-treated (years_post dummies are significant)
	
	- although this increase was slightly smaller for treated states (negative coeff of treated#years_post), the difference is n.s.
	
	- years_pre have significant coeff and these mean that, compared to 86, non-treated states had lower prison rates 
	in the years just before (in line with the positive trend we see in the plot)
	
	- this trend was however not different for treated groups (the interaction terms between ds#years_pre are all n.s.)
	
	- we conclude that there is no significant pre-trend in this case
	
	- but there is no significant effect of the policy either
	
*/

**# EXPLORE FURTHER: THE EFFECT OF THE POLICY ON DIFFERENT TYPES OF CRIMES

* A)  for example, did the policy have any effect on the rate of violent crimes?

reg criv i.post##i.treated , cluster (state)

/*

Linear regression                               Number of obs     =        714
                                                F(3, 50)          =      18.37
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0872
                                                Root MSE          =     3.2936

                                 (Std. Err. adjusted for 51 clusters in state)
------------------------------------------------------------------------------
             |               Robust
        criv |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      1.post |   1.277884    .243017     5.26   0.000     .7897697    1.765998
   1.treated |  -1.605713   1.107722    -1.45   0.153    -3.830638    .6192122
             |
post#treated |
        1 1  |  -.6390832   .2718694    -2.35   0.023    -1.185149   -.0930174
             |
       _cons |   5.700946   1.042665     5.47   0.000     3.606693    7.795199
------------------------------------------------------------------------------


		In this case, we would see that although violent crime went up in most states after the policy change,
		treated states suffered a lower increase in violent crime, so the policy may have helped prevent violent crime
*/

* B) Try the same for property crime (also in the dataset and we see, again, no signficant effect of the policy)

* C) Repeat the exercise above with this or some other DV and also include some control variables (state characteristics) to see if anything would change 


log close




