
setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course')
source('/home/emil/Dropbox/data-science/R/.Rprofile')
# source('r/data-preprocessing-workshop-first-ex-v01.r')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtcor.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/narv_omit.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtdesc.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dt_ttest.R')


library(haven)
library(Hmisc)
library(GGally)
library(plotly)
library(fixest)
library(Hmisc)
library(modelsummary)
library(marginaleffects)
library(sampleSelection)


a1 <- read_dta('input/workshop-second-ex-reflexitaly.dta') %>% data.table()

##############  #section #################
#  descriptive statS
#########################################

thecols  <- colc(a1, not='employed|lnwage')
pre_detect <- dt_ttest(a1,  'employed', thecols)

# make factors out of binary variables
thecols_bin <- dtdesc(a1)[unique_vals == 2, var]  %>% colc(not='employed')
a1[, (thecols_bin) := map(.SD, factor), .SDcol=thecols_bin]


dtdesc(a1)
dtcor(a1)
asdf
datasummary_skim(a1)

# does not work, because oF lnwage in some way, I guess
ggpairs( a1,
	upper = list(continuous = "density", combo = "box_no_facet"),
	lower = list(continuous = "points", combo = "dot_no_facet")) + theme_bw() + theme_bb()

ggpairs(a1)

##############  #section #################
# regression
#########################################

# standard ols
feols(lnwage ~ female + age + stem + goodgrades + unemp6more, a1) %>% etable(vcov='hetero')

# predicting labour mrket participation
mod_1stage <- feglm(employed ~ female + age + stem + goodgrades + unemp6more + child + livewpartner + livewparents + lookjobbefore, a1, family = 'probit', vcov='hetero') 
mod_1stage %>% etable()


# get the cof of female from margins
mfx <- marginaleffects(mod_1stage)
summary(mfx)

# see if the variables that could be of interest are jointly significant, even if one of them is not. Then we might include them all together anyway, on theoretical grounds.
wald(mod_1stage, 'live|look|child')


# Heckmann by hand: 1. stage, the probit model:
# http://www.stata.com/support/faqs/statistics/inverse-mills-ratio/

# get the predicted values
probit_lp <- predict(mod_1stage)
# make the mills inverse-mills-ratio, and put it in the DT
a1[, mills := dnorm(probit_lp)/pnorm(probit_lp) ]
summary(a1$mills)
dtdesc(a1$mills)


# correlation between the mills ratio and the variables of interest
cor1 <- dtcor(a1[employed == 1, .(female, age, stem, goodgrades, unemp6more, mills )])
cor1[var1 == 'mills' | var2 == 'mills']

# only the selected 
qplot(a1[employed == 1, mills0])
# including the unemployed
qplot(a1[, mills0])

# test assumption: that the selection into sample variables are NOt correlated with the DV. Here, one of them is.
feols(lnwage ~ female + age + stem + goodgrades + unemp6more + child + livewpartner + livewparents + lookjobbefore, vcov='hetero', data=a1) 

# manually
feols(lnwage ~ female + age + stem + goodgrades + unemp6more + mills0, a1) %>% etable(vcov='hetero')
# automatically
# first the 1 stage model, then the second stage model. 
mod_heck  <- selection(employed ~ child + livewpartner + livewparents + lookjobbefore + female + age + stem + goodgrades + unemp6more, lnwage ~ female + age + stem + goodgrades + unemp6more, data=a1, method='2step')





