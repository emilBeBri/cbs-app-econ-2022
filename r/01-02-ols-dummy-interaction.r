# Setup
setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course')
source('r/data-preprocessing-workshop-first-ex-v01.r')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtcor.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/narv_omit.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtdesc.R')

##############  #section #################
#  descriptive statS
#########################################

# fix issues becauses of inf values - should be handled in the function #todo 
dttools_desc(ba4[, .( prodnew, extsource, rdint, inconst, lempl00)])
dttools_desc(ba4)

dtcor(ba4[, .( prodnew, extsource, rdint, inconst, lempl00)])



# could be better - also, why does patapply gives an
ggpairs(
ba4[, .( prodnew, extsource, rdint, market, prodinov)]  ,
upper = list(continuous = "density", combo = "box_no_facet"),
lower = list(continuous = "points", combo = "dot_no_facet")) + theme_bw() + theme_bb()
ggpairs(ba4)






dtdesc(ba4[, .(prodnew)])
dtdesc(ba4[, .(prodnew, rdint, rdintpct)])



#########################################
# linear reg OLs
#########################################

# get the sample
ols1 <- ba4[, .(prodnew, extsource, rdint, rdintpct, inconst, lempl00)] %>% na.omit()

o1 <- feols(prodnew ~ extsource + rdint + inconst + lempl00, ols1) 
# add robust std errors - but you don't have to do that in the estimation process, you can do it post ante
o1 %>% etable(vcov='hetero')
# and without
o1 %>% etable()



# I dont get why rdintpct should be lower in pct than in shares - shouldn-t it be the other way around?
feols(prodnew ~ extsource + rdintpct + inconst + lempl00, ols1) %>% etable(vcov='hetero')
feols(prodnew ~ extsource + rdint + inconst + lempl00, ols1) %>% etable(vcov='hetero')
o1 %>% etable(vcov='hetero')


# f-test, with regex matching
wald(o1, 'rdint')

# more params
wald(o1, 'rdint|inconst')


# plot it (you can put more than plot)
coefplot(o1) 
coefplot(list(o1,o1, o1)) 

#########################################
# interactions & marginal effects 
#########################################


# with interactions
mod_ols <- feols(prodnew ~ extsource*rdintpct + inconst + lempl00, ols1, vcov= 'hetero')

# AME average marginal effects AME 
mfx <- marginaleffects(mod_ols)
summary(mfx)
# gives the  marginal effects.. for what? how to interpret this?

# at representative values
r1 <- predictions(mod_ols, newdata = datagrid(rdintpct = seq(0,70,2), extsource=1.7)) %>% data.table; r1
r1 <- predictions(mod_ols, newdata = datagrid(rdintpct = seq(0,70,2))) %>% data.table; r1


r1 <- predictions(mod_ols, variables=list(rdintpct=1)) %>% data.table; r1
r1[, .(predicted, rdintpct, extsource)]


View(r1)
View(r2)
r2 <- predictions(mod_ols) %>% data.table; r2



ggplot(r1, aes(predicted, rdintpct)) + geom_point() + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + theme_bw() + theme_bb() + coord_flip()


# MEM: marginal effect at the mean- coefficients set to mean. What does that even mean?
ma <- marginaleffects(g1, newdata = 'mean')



# not sure why this is different?
j1 <- marginaleffects(g1, newdata = datagrid(rdintpct = seq(0,70,2)))
j1 %>% modelplot()









feols(prodnew ~ avg_extsource*avg_rdintpct + inconst + lempl00, ols1) %>% etable(vcov='hetero')


lo1 <- na.omit(ba4[, .(prodinov , extsource , rdintpct , inconst , lempl00)])
# g1 <- feglm(prodinov ~ extsource + rdintpct + inconst + lempl00, family='logit', vcov='hetero', lo1)
g1 <- feglm(prodinov ~ extsource + rdintpct + inconst + lempl00, family='probit', vcov='hetero', lo1)

# AME average marginal effects AME 
mfx <- marginaleffects(g1)
summary(mfx)
# trying to replicate manually
head(mfx)
mfx %>% as.data.table() %>% .[ !is.na(statistic)]
mfx %>% as.data.table()
m1 <- mfx %>% as.data.table()
m1[, .(mean(dydx)), .(term)]
m1   [, .(mean(dydx), sd(dydx)), .(term)]

# MEM: marginal effect at the mean- coefficients set to mean. What does that even mean?
ma <- marginaleffects(g1, newdata = 'mean')

# at representative values
r1 <- predictions(g1, newdata = datagrid(rdintpct = seq(0,70,2)))
ggplot(r1, aes(predicted, rdintpct)) + geom_point() + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + theme_bw() + theme_bb() + coord_flip()


# not sure why this is different?
j1 <- marginaleffects(g1, newdata = datagrid(rdintpct = seq(0,70,2)))
j1 %>% modelplot()






library(modelsummary)




seq(0,1,70)

# average predicted outcome 
predictions(g1, newdata = datagrid())





marginalmeans(g1)




dtdesc



uniqueN(m1, by='rowid')
unique(m1, by='rowid') %>% .[, .(mean(dydx)), .(term)]







# get the AME (average marginal effect) 
summary(mfx)


#  We can calculate average marinal effects (average partial effects in wooldridge), conditional average marginal effects, and the predicated probabilities 
#  For linear regression (OLS), addigng at means does not change the margianl effects estimated. 
#  However, in a non linear model (such as logit) it matters!
#  Average marginal effects - we interpret:
#  One unit (sd) increase in external source, holding all other variables constant, increases the probability for product innovation by 0.080732 
# margins, dydx(*)
#  conditional marginal effects - we interpret:
#  Holding all other variables constant at thier mean, the increase of probability is now 0.0885114 
# margins, dydx(*) atmeans 
# marginsplot



# these two are the same. so you're taking the mean of the combination of these
m1[, .(mean(dydx)), .(contrast, term)]
# the std error is not the same, because they're taking something called 'the averaged jacobian' 
m1[, .(mean(dydx), mean(std.error)), .(contrast, term)]
# thsese are the numbers in each group - it's the same, because there is a prediction for each predictor (and for the factor: each level)
m1[, .N, .(contrast, term)]


# Base R reads CSVs too, but we'll use data.table here
dat = data.table::fread('https://raw.githubusercontent.com/stata2r/stata2r.github.io/main/data/cps_long.csv')


feols(wage ~ i(treat, age), dat) 

est1 = feols(wage ~ i(hisp, age), dat) 
est1 = feols(wage ~ i(factor(hisp), age), dat) 


# Show how effect differs by group
iplot(est1)


# Show predictive margins with an interaction
# This requires plot_cap from the marginaleffects package
library(marginaleffects)
plot_cap(est1, condition = c('age','hisp')) + labs(x = 'Age', y = 'Wage', fill = 'Hispanic')


# Aside: i() is a fixest-specific shortcut that also 
# has synergies with some other fixest functions. But 
# base R interaction operators all still work, e.g. 
feols(wage ~ factor(treat):age, dat) 
feols(wage ~ factor(treat)/age, dat) 
feols(wage ~ factor(treat)*age, dat)



#########################################
# logit
#########################################


form_logit <- prodinov ~ extsource + rdint + inconst + lempl00

lo1 <- feglm(form_logit, ba4, family='logit') 
lo1 %>% etable(vcov='hetero')


mfx <- marginaleffects(lo1)
# One unit (sd) increase in external source, holding all other variables constant, increases the probability for product innovation by 0.080732 ///
summary(mfx)
m1 <- mfx %>% as.data.table()



mfx[, mean(predicted)]



m1[ !is.nan(statistic)]
m1[ is.nan(statistic)]




# marginaleffects(mod[ie the model object], newdata = "mean"[this holds all non-specified variables at the mean, i.e. you're calculating the MEM rather than the AME. For the AME leave this null to calculate the ME for everyone in the dataset], varlables = vars[i.e. a list of variables you want to calculate the ME for]) 



# change in predictions
predictions(lo1, newdata = datagrid())
marginaleffects(lo1, newdata = 'mean')
m1 <-  marginaleffects(lo1)
m1





datagrid(model=lo1)
lo1






# predictions in a table, for all the observations
predictions(lo1) %>% head()
predictions(lo1) %>% data.table() %>% .[ !is.na(predicted)]



predictions(lo1, newdata = datagrid())
predictions(lo1, newdata = datagrid()) %>% transpose()
predictions(lo1, newdata = datagrid()) %>% t()
predictions(lo1, newdata = datagrid()) 






datagrid(model=lo1)
predictions(lo1, datagrid())



predictions(lo1, newdata = datagrid(am = 0, wt = seq(2, 3, .2)))



library(marginaleffects)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
dtdesc(dat)

mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species,
           data = dat, family = binomial)
mod

mfx <- marginaleffects(mod)
head(mfx)
mfx %>% as.data.table() %>% .[ !is.na(statistic)]
mfx %>% as.data.table()
m1 <- mfx %>% as.data.table()



# get the AME (average marginal effect) 
summary(mfx)
# these two are the same. so you're taking the mean of the combination of these
m1[, .(mean(dydx)), .(contrast, term)]
# the std error is not the same, because they're taking something called 'the averaged jacobian' 
m1[, .(mean(dydx), mean(std.error)), .(contrast, term)]
# thsese are the numbers in each group - it's the same, because there is a prediction for each predictor (and for the factor: each level)
m1[, .N, .(contrast, term)]



# get the g-AME (group average marginal effect)
# you can do this manually  
mfx2 <- marginaleffects(mod, by = "species", variables = "bill_length_mm")
m2 <- mfx2 %>% as.data.table()
summary(mfx2)
m1[ term == 'bill_length_mm'   , .(mean(dydx)), .(contrast, term, species)]
m1[ term == 'bill_length_mm'   , .(mean(dydx)), .(contrast, term)]
m1[ , .(mean(dydx)), .(contrast, term)]


fsetequal(m1, m2)
thecols <- setscn(m1, m2)[[1]]
fsetequal(m1[, ..thecols], m2[, ..thecols])
all.equal(m1[, ..thecols], m2[, ..thecols])









m

1
m2







m1[rowid == 1]
m2[rowid == 1]
m2
summary(mfx2)



m1


nrow(mfx)
nrow(dat)
uniqueN(mfx, by='rowid')




##############  #section #################
# center around mean - not sure what this does
#########################################

# center data around mean - how to interpret this? #todo 
ols1[, `:=` ( avg_rdintpct = rdintpct - mean(rdintpct),  avg_extsource = extsource - mean(extsource))]
