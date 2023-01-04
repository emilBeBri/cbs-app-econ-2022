# Setup
setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course')
source('/home/emil/Dropbox/data-science/R/.Rprofile')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtcor.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/narv_omit.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtdesc.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/narv_omit.R')
source('r/data-preprocessing-workshop-first-ex-v01.r')



# estimate basic logit model



mod_log_basic <- feglm(prodinov ~  extsource + rdintpct + inconst + lempl00, family='logit', data=ba4, vcov='hetero')
marg_log_basic <- marginaleffects(mod_log_basic)

m1 <- marginaleffects(mod_log_basic) %>% data.table()
m1[, .(extsource=mean())]


# get the AME (average marginal effect) 
summary(mfx)
# these two are the same. so you're taking the mean of the combination of these
# m1[, .(mean(dydx)), .(contrast, term)]
m1[, .(mean(dydx)), .(term)]
m1[, .(mean(predicted)), .(term)]
# the std error is not the same, because they're taking something called 'the averaged jacobian' 
m1[, .(mean(dydx), mean(std.error)), .(term)]








#  Marginal Effects 
#  We can calculate average marinal effects (average partial effects in wooldridge), conditional average marginal effects, and the predicated probabilities 
#  For linear regression (OLS), addigng at means does not change the margianl effects estimated. 
#  However, in a non linear model (such as logit) it matters!
#  Average marginal effects - we interpret:
#  One unit (sd) increase in external source, holding all other variables constant, increases the probability for product innovation by 0.080732 
summary(marg_log_basic)
# conditional marginal effects - we interpret:
# Holding all other variables constant at thier mean, the increase of probability is now 0.0885114 //
marg_log_atmeans <- marginaleffects(mod_log_basic, newdata = 'mean')
ggplot(marg_log_atmeans, aes(term, dydx)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + theme_bw() + theme_bb() 


predict(mod_log_basic) %>% mean()

ba4[, .(prodinov ,  extsource , rdintpct , inconst , lempl00)] %>% na.omit() %>% dtdesc() %>% .[, .(var, mean)]
ba4[, .(prodinov ,  extsource , rdintpct , inconst , lempl00)] %>% narv_omit() %>% dtdesc() %>% .[, .(var, mean)]


r1 <- predictions(mod_log_basic, newdata = datagrid(rdintpct = seq(0,70,2)))
View(r1)


mean(ba4$rdintpct)

ba4


datagrid(rdintpct = seq(0,70,2), model=mod_log_basic)




ggplot(r1, aes(predicted, rdintpct)) + geom_point() + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + theme_bw() + theme_bb() + coord_flip()




marg_log_basic



margins,  predict() at(rdintpct = (0(2)70))






# /// One unit (sd) increase in external source, holding all other variables constant, increases the probability for product innovation by 0.080732 ///







remotes::install_github('vincentarelbundock/marginaleffects')



library(data.table)
library(marginaleffects)
library(magrittr)
library(fixest)



fails <- structure(list(prodinov = structure(c(0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0)), lempl00 = structure(c(-Inf, 2.99573227355399, 7.02908756414966, 3.58351893845611, 6.09356977004514, 3.04452243772342, 6.3456363608286, 2.30258509299405, 5.77455154554441, 6.06378520868761, 1.79175946922805, 7.64108424917491, 4.06044301054642, 6.8596149036542, 5.59842195899838, 3.3322045101752, 3.63758615972639, 5.63478960316925, 7.53583046279837, 5.13579843705026, 4.36944785246702) ), rdintpct = structure(c(NA, NA, NA, NA, 0, 0, 0.793854033290653, NA, NA, NA, 0, 0, NA, NA, 0.0396589331746976, NA, NA, 0, 0.677960960368859, NA, NA))), row.names = c(NA, -21L), class = c("data.table", "data.frame"))


# here  the variable lempl00 that has an infinite value disappears (is actually set to 0, wrongly, it seems like, and therefore is removed from summary)
feglm(prodinov ~   rdintpct + lempl00, family='binomial', data=fails) %>% marginaleffects() %>% summary()

# this works
glm(prodinov ~   rdintpct + lempl00, family='binomial', data=fails) %>% marginaleffects() %>% summary()

# if you remove the infinite value from the data BEFORE estimation, then the feglm works like glm does
feglm(prodinov ~   rdintpct + lempl00, family='binomial', data=fails[!is.infinite(lempl00)] ) %>% marginaleffects() %>% summary()





