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

f1 <- read_dta('07-paneldata/wagecps.dta') %>% data.table()


# descriptives by year 
d1 <- map_dfr(su(f1$t), .id='t', ~ dtdesc(f1[t == .x]))[order(var)]
d1[var %in% qc(lwage, ed, exp, wks)]


# d2 <- d1[var %in% qc(lwage), .(mean, sd, t, var)]
# d2 %>% ggplot(., aes(t, var)) + geom_()



ggplot(f1, aes(lwage, t)) + geom_point()



# sample random indiviuals, look at heterogenity
ggplot(sample_long(f1, id, 20), aes(t, lwage, group=id, fill=id, color=factor(id))) + geom_line(alpha=.4, size=2)



##############  #section #################
#  estimation
#########################################


cof_x <- 'ed'
cof_y <- 'lwage'
cof_y_dv  <- cof_y %+% ' ~ ' %+% cof_x
cof_controls <- 'exp + exp2 + wks'


cof_naive_ols <- paste(cof_y_dv %+% ' + ', cof_controls, collapse = ' + ') 
form_naive_ols <- cof_naive_ols %>%  as.formula()
mod_naive_ols  <- feols(form_naive_ols, f1)


# manual FD/First Difference estimator
# get the means by unit
thecolls_all<- qc(lwage, ed, exp, exp2, wks)
f1[, ('avg_' %+% thecolls_all) := lapply(.SD, mean), by=id, .SDcols=thecolls_all]
for(.x in thecolls_all) f1[, ('avg_diff_' %+% .x) := get(.x) - get('avg_' %+% .x) ]
form_manual_fd <- gsub('\\b(\\w+)\\b', 'avg_diff_\\1', cof_naive_ols) %>%  as.formula()
mod_manual_fd  <- feols(form_manual_fd, f1)
# > no change in education, so it cancels out

form_fe <- paste(cof_y_dv %+% ' + ', cof_controls, ' | id', collapse = ' + ') %>%  as.formula()
mod_fe  <- feols(form_fe, f1)



mod_manual_fd
mod_fe
mod_naive_ols
