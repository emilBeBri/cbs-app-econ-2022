# Setup
setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/test-exam-2020')
source('/home/emil/Dropbox/data-science/R/.Rprofile')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtcor.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/narv_omit.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtdesc.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dt_ttest.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/narv_omit.R')
library(haven)
library(Hmisc)
library(GGally)
library(plotly)
library(fixest)
library(Hmisc)
library(modelsummary)
library(marginaleffects)


# Question 1

f1 <- read_dta('caresdata.dta')  %>% data.table()



fepois(perday ~ female + age45 + so_addicted + want_quit, f1, vcov='hetero')

f1[, ln_perday := log(perday)]

mod_q1d <- feols(ln_perday ~ female + so_addicted + age45 + want_quit, f1, vcov='hetero')

# mod_q1e <- lm(ln_perday ~ female*so_addicted + age45 + want_quit, f1)
mod_q1e <- feols(ln_perday ~ female*so_addicted + age45 + want_quit, f1, vcov='hetero')
marg_q1e <- marginaleffects(mod_q1e)




# Question 1

# a:  want_quit will be upward biased, if there is a tendency for people who have more willpower to  blah blah

# b: 

# IV using feols:
# The syntax is: Dep_var ~ Exo_vars | Fixed-effects | Endo_vars ~ Instruments.
# firststage <- paste('nearc4 ~', cof_controls, collapse = ' + ')

form_iv_q1b  <- ln_perday ~ female + so_addicted + age45 | want_quit ~ quit_6mos + avoid
mod_iv_q1b <- feols(form_iv_q1b, f1, vcov = 'hetero')



etable(mod_q1e, mod_iv_q1b)
mod_iv_q1b 
mod_q1e

dtdesc(f1[, .(want_quit)])
f1[, .(want_quit)]

dtdesc(f1)


form_firststage  <-  want_quit ~ quit_6mos + avoid + female + so_addicted + age45
feglm(form_firststage, f1, vcov = 'hetero', family = 'binomial')




# overidentification test (can only be done with >= 2 instruments)
f1[, iv_res := resid(mod_iv_q1b)]
f1$iv_res %>% summary()

mod_overid  <- feols('iv_res ~' %+% cof_iv %+% ' + ' %+% cof_controls %>% formula(), f1 )


oid <- r2(mod_overid)[['r2']] * nrow(f1) 

# not getting the right chisq value, not sure why 
pchisq(oid, df= 1)
pchisq(0.03, df= 1)






