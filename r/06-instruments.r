
setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course')
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



##############  #section ################
# workshop 
#########################################


# read in data and remove non-relevant obs (not working, wages not observed. ignore issues with missing data for now)
ff1 <- read_dta('input/mroz-1.dta') %>% data.table() %>% .[ inlf != 0]



# all_thecols  <-  qc(wage,  educ, exper, expersq, motheduc, fatheduc, huseduc)
all_thecols  <-  qc(lwage,  educ, exper, expersq, motheduc, fatheduc, huseduc)
ff1  <- ff1[, ..all_thecols]
# ff1[, lwage := log(wage)]
# ff1 <- ff1[, !'wage']



#######  #subsection ######
# descritpive stats


dtdesc(ff1)
dtcor(ff1)
# datasummary_skim(ff1)
# ggpairs(
# ff1,
# upper = list(continuous = "density", combo = "box_no_facet"),
# lower = list(continuous = "points", combo = "dot_no_facet")) + theme_bw() + theme_bb() 
# theme_dark()

##### subsection over #####


# test if endovar is correlated with exovar - they are
dtcor(ff1[, .(educ, fatheduc, motheduc, huseduc) ])


#######  #subsection ######
# estimation 


# everything is significant in correlations
# dtcor(ff1[, .(lwage, educ,  motheduc, fatheduc, huseduc)])


cof_x <- 'educ'
cof_y <- 'lwage'
cof_y_dv  <- cof_y %+% ' ~ ' %+% cof_x
# cof_iv  <- 'motheduc + fatheduc + huseduc'
cof_iv  <- 'motheduc + fatheduc'
# cof_iv  <- 'fatheduc'
cof_controls <- 'exper + expersq'
cof_firststage  <- paste0(cof_x, ' ~ ', cof_iv)


# are the family educ correlated with education, inc. the controls
form_firststage  <- paste(cof_x %+% ' ~', cof_iv, cof_controls, sep=' + ') %>% as.formula()
mod_firststage <- feols(form_firststage, ff1, vcov = 'hetero') 

wald(mod_firststage, 'educ')
# > yes, the instruments are correlated with education


# and what about the instruments themselves? do they explain wage?
form_test1  <- paste(cof_y_dv, cof_iv, cof_controls, sep=' + ') %>% as.formula()
mod_test1 <- feols(form_test1, ff1, vcov = 'hetero') 
# > no, they do not
wald(mod_test1, '.educ')
# > no, they do not - not even jointly. This is very strange.

## naive ols 
# create naive formula
# form_naive_ols <- paste(cof_y_dv %+% ' + ', cof_controls, collapse = ' + ') %>%  as.formula()
form_naive_ols <- cof_y_dv %+% ' + ' %+%  cof_controls %>%  as.formula()
# run naive regression
mod_naive_ols <- feols(form_naive_ols, ff1, vcov = 'hetero')



# IV using feols:
# The syntax is: Dep_var ~ Exo_vars | Fixed-effects | Endo_vars ~ Instruments.
# firststage <- paste('nearc4 ~', cof_controls, collapse = ' + ')
form_iv <- formula(
	paste(cof_y %+% ' ~ ', cof_controls,  ' | ', cof_firststage)
	)
feols(form_iv, ff1, vcov = 'hetero')

# manual iv
# 1. First stage 

# estimate first stage by hand form_firststage <- paste0(cof_firststage, ' + ', cof_controls) %>% as.formula()
mod_firststage <- feols(form_firststage, ff1) 

# include first stage output as prediction in second stage
ff1[, x_pred := predict(mod_firststage) ]
ff1[, x_res := resid(mod_firststage) ]

form_reducedf <- paste0(cof_y %+% ' ~ ', 'x_pred',  ' + ', cof_controls) %>% as.formula()

# compare the manual (first two lines) and automatic (3rd line) - they are the same (f-test also the same)
feols(form_reducedf, ff1, vcov = 'hetero')
wald(mod_firststage, 'educ')
mod_iv  <- feols(form_iv, ff1, vcov = 'hetero')

# test using residuals - the residuals are NOT significant, meaning:
form_endo_test  <- paste0(cof_y_dv, '+ x_res +', cof_controls) %>% as.formula()
feols(form_endo_test, ff1, vcov = 'hetero')

# overidentification test (can only be done with >= 2 instruments)
ff1[, iv_res := resid(mod_iv)]
ff1$iv_res %>% summary()

mod_overid  <- feols('iv_res ~' %+% cof_iv %+% ' + ' %+% cof_controls %>% formula(), ff1 )


oid <- r2(mod_overid)[['r2']] * nrow(ff1) 

# not getting the right chisq value, not sure why 
pchisq(oid, df= 1)
pchisq(0.03, df= 1)



map(1:20, ~ qchisq(oid, df= .x) )






pchisq(15, df=2, lower.tail=FALSE)




##############  #section ################
# mixtape example
#########################################

library(AER)
library(haven)
library(tidyverse)

read_data <- function(df) {
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

card <- read_dta("https://raw.github.com/scunning1975/mixtape/master/card.dta") %>% data.table()

#Define variable 
#(Y1 = Dependent Variable, Y2 = endogenous variable, X1 = exogenous variable, X2 = Instrument)


Y1 <- card$lwage
Y2 <- card$educ
X1 <- cbind(card$exper, card$black, card$south, card$married, card$smsa)
X2 <- card$nearc4



#OLS
ols_reg <- lm(Y1 ~ Y2 + X1)
summary(ols_reg)

#2SLS
iv_reg = ivreg(Y1 ~ Y2 + X1 | X1 + X2)
summary(iv_reg)



all_thecols <- qc(lwage,educ,exper,black,south,married,smsa)
dtdesc(card[, ..all_thecols])


# vars of interest
cof_y_dv  <- 'lwage ~ educ +'
cof_y  <- 'lwage ~ '


# controls
cof_controls_bin <- dtdesc(card[, ..all_thecols])[class == 'numeric' & unique_vals == 2, var] 
cof_controls_bin <- paste0('factor(', cof_controls_bin, ')') %>% paste(collapse = ' + ' )
# cof_controls_bin  <- paste(cof_controls_bin, collapse = ' + ' )
cof_controls_num  <- '+ married + exper'
cof_controls <- paste(cof_controls_bin, cof_controls_num, collapse = ' + ')
cof_controls


# create naive formula
cof_naive_ols <- paste(cof_y_dv, cof_controls, collapse = ' + ')
form_naive_ols  <-  as.formula(cof_naive_ols)
form_naive_ols


# IV formula
# firststage <- paste('nearc4 ~', cof_controls, collapse = ' + ')

cof_firststage <- paste('educ ~ nearc4', collapse = ' + ')
cof_firststage
form_iv <- formula(
	paste(cof_y, cof_controls,  ' | ', cof_firststage)
)
form_iv 


	# paste(gsub(' educ \\+', '', cof_naive_ols), ' | ', cof_firststage)


# The syntax is: Dep_var ~ Exo_vars (| Fixed-effects) | Endo_vars ~ Instruments.

# estimation
feols(form_naive_ols, card) %>% etable(vcov = 'hetero')
feols(form_iv, card) %>% etable(vcov = 'hetero')
feols(form_iv, card)


# estimate first stage by hand, get the effect of nearc4 on educ
form_firststage <- paste0(cof_firststage, ' + ', cof_controls) %>% as.formula()
feols(form_firststage, card) 


