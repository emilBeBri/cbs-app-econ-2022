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
# install.packages('PSweight')
# install_github("thuizhou/PSweight")
library(PSweight)

a1 <- read_dta('input/datamatching.dta') %>% data.table()

# numeric binary
des1 <- dtdesc(a1)
# if character: keep the vars strings as labels
a1[, data_id  := gsub('^D.*e$', 'DW', data_id)]

thecols <- des1[unique_vals == 2 & class == 'character', var] 
walk(thecols, ~ a1[, (.x) := factor(get(.x), levels = unique(a1[[.x]])),] )


# the different relevant groups, overview:
a1[, .N, .(training, treat_experiment, experim_sample, data_id)]


##############  #section #################
# estimation
#########################################

# first see diff in the eksperimental sample, e.g. the actual difference

# test balancing condition w. t-test
thecols_all  <- qc(education, nodegree, age, married, black, hispanic, re74, re75, re78)
dt_ttest(a1[experim_sample == 1], 'training', thecols_all)
# no difference, meaning: the random assigninment is working - except for the post-treatment wages, as expected!
control_result  <- a1[experim_sample == 1, .(mean=mean(re78)), training]
control_result  <- rbind(control_result, data.table(training='difference', mean=0))
control_result$mean[3]  <-  control_result$mean[1] - control_result$mean[2]  


# drop the actual control group from the experimental sample, e.g. we'll use the survey as control grp
a2  <- a1[(training == 1 & experim_sample == 1) | (training == 0 & experim_sample == 0) ]
a2[, .N, .(training, treat_experiment, experim_sample, data_id)]

# difference btw survey as control - huge, except in hispanic background
dt_ttest(a2, 'training', thecols_all) 
# wages for nontreatet are much much better - because of diff in background

# strongly negative effect of training
form_naive_ols <- re78 ~ training +  factor(nodegree) + age + factor(married) + factor(black) + factor(hispanic)
feols(form_naive_ols, a1) %>% etable(vcov='hetero')
 
# logit model for selection into training
form_logit <-  training ~  factor(nodegree) + age + factor(married) + factor(black) + factor(hispanic)
lo1 <- feglm(form_logit, a2, family='logit') 


# a violation of independence also implies that covariates will be unbalanced across the propensity score—something we call the balancing property.


# get predicted/fitted values in there, e.g. propensity score
a2[, pscore2 := predict(lo1)]


# histogram
ggplot(a2, aes(pscore, fill=data_id)) +
	geom_histogram() + facet_wrap(~ data_id) + scale_fill_manual(values=ecolor_data[grp=='xmen'][3:4, hex])

ggplot(a2, aes(pscore, fill=data_id)) +
	geom_density() + facet_wrap(~ data_id) + scale_fill_manual(values=ecolor_data[grp=='xmen'][3:4, hex])

ggplot(a2, aes(pscore, fill=data_id)) +
	geom_density(alpha=0.5) +  scale_fill_manual(values=ecolor_data[grp=='xmen'][3:4, hex])


balance_any <- SumStat(ps.formula = form_logit, data = a2,  weight = c("IPW", "overlap", "treated"))

# the same as the ones I did above
plot(balance_any, type='density')
plot(balance_any, type='hist') 
plot(balance_any)


# default is overlap, which is the OW version, I think
ps_methods <- qc(IPW, overlap, treated)
pso  <- map(set_names(ps_methods), ~ PSweight(ps.formula = form_logit, yname = "re78", data = a2,  weight= .x))
#pso  <- PSweight(ps.formula = form_logit, yname = "re78", data = a2,  weight= "IPW")
# pso  <- PSweight(ps.formula = form_logit, yname = "re78", data = a2,  weight= "overlap")
map(pso, summary)
pso

form_final_ols <- re78 ~  factor(nodegree) + age + factor(married) + factor(black) + factor(hispanic)


# input the selection-into treatment-control, then the 
out1 <- map(set_names(ps_methods), ~ PSweight(ps.formula = form_logit, yname = "re78", data = a2, weight= .x ,augmentation = TRUE, out.formula = form_final_ols) )
out1

map(out1, summary)
summary(out1$IPW)

out1$IPW
out1$IPW %>% names()

out1$IPW$muhat
out1$IPW$trtgrp
out1$IPW$propensity



