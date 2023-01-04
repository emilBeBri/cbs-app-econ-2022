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




##############  #section #################
# lecture do-files reconf
#########################################

# https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#422_Simple_difference-in-differences_(TWFE)


f1 <- read_dta('08-diff-in-diff/ck1994-modificed-by-script.dta') %>% data.table()


# w/o controls
feols('fte ~ treated + post + i(treated, post)' %>% formula(), data = f1, vcov='hetero')
# with controls
feols(fte ~ treated + post + i(treated, post) + i(chain) , data = f1, vcov='hetero')




# prison example
j1 <- read_dta('08-diff-in-diff/prison-modificed-by-script.dta') %>% data.table()

p0  <- j1[, .(pris=mean(pris)), .(treated, year)]
ggplot(p0, aes(x = year, y = pris, color = factor(treated))) + geom_line() + geom_point() + theme_bw()


# and now with controls, you need to use marginaleffects to get the clean effect. But first redo the plot above for verification

# at representative values

# clustered std error by state 
mod_pri  <- feols(pris ~ factor(treated)*factor(year), j1)  
etable(mod_pri, cluster=~state)

# we set the clustering by state in the object so marginaleffects can use it
mod_pri  <- feols(pris ~ factor(treated)*factor(year), j1, cluster=~state)

r1 <- predictions(mod_pri, newdata = datagrid(year = unique(j1$year), treated = 0:1)) %>% data.table()

ggplot(r1, aes(predicted, year, fill=factor(treated))) + geom_point(aes(color=factor(treated))) +  theme_bw() + theme_bb() + coord_flip() + scale_y_continuous(breaks=unique(j1$year)) + geom_hline(yintercept=86, linetype='dashed') +  geom_ribbon(aes(xmin=conf.low, xmax=conf.high), alpha=0.1) 



ggplot(r1, aes(predicted, year,  fill=factor(treated))) + geom_point(size=2.5, aes(color=factor(treated))) + geom_ribbon(aes(xmin=conf.low, xmax=conf.high), alpha=0.1) + theme_bw() + theme_bb() + coord_flip() + scale_y_continuous(breaks=unique(j1$year)) + geom_hline(yintercept=86, linetype='dashed')

# or - my own idea: You could plot the DIFFERENCE btw them:
# the difference rises each year. Not good. However, the diff is not significant, as you can see on the conf intervals - they only become differencent in 91. So what kind of indication is this? #?	
r2 <- r1[treated == 1, .(year, predicted_treated=predicted)]  %>% 
	.[ r1[treated == 0, .(year, predicted_ctrl=predicted)  ], on='year', nomatch=0 ]
r2[, diff := predicted_ctrl - predicted_treated ]
ggplot(r2, aes(diff, year)) + geom_point(size=2.5) + theme_bw() + theme_bb() + coord_flip() + scale_y_continuous(breaks=unique(j1$year)) + geom_hline(yintercept=86, linetype='dashed')


# jeg forstaar ikke det her prison example. Baade pre og post er det jo insignificant? Hvad kan man bruge det til?  er det fordi ref er 1980? vi vil self gerne have det med 1986 som ref kategori.

# setting ref category to 86 means we get the interaction effect we're looking for (not doing that but easy to replicate from the stata script - you could probably set the factor level in R so that the first level is  )
feols(pris ~ factor(treated)*factor(year, levels=c(86, setdiff(80:93, 86))), j1[]) %>% etable()


feols(pris ~ year + treated + i(treated, i.year), j1)  %>% etable() 

factor(j1$year, levels=c(86, setdiff(80:93, 86)))
factor(j1$year )



##############  #section #################
# workshop questions
#########################################
f1 <- read_dta('08-diff-in-diff/vodkadata.dta') %>% data.table()

# e1.A
t.test(logvodka ~ y2011, data = f1, alternative = "two.sided", var.equal = FALSE)
# diff NOT significant

# e1.B
t.test(logvodka ~ hdrinker50, data = f1, alternative = "two.sided", var.equal = FALSE)
t.test(logvodka ~ hdrinker50, data = f1, alternative = "less", var.equal = FALSE)
# > yes the difference is significant




# e1.C

f1[ hdrinker50 == 1 & y2011 == 0, mean(logvodka)]
f1[ hdrinker50 == 1 & y2011 == 1, mean(logvodka)]
f1[ hdrinker50 == 0 & y2011 == 0, mean(logvodka)]
f1[ hdrinker50 == 0 & y2011 == 1, mean(logvodka)]



# 1 pre and post: significant differences btw the two groups
map(0:1, ~ t.test(f1[ hdrinker50 == 1 & y2011 == .x, logvodka], f1[ hdrinker50 == 0 & y2011 == .x, logvodka], alternative = "two.sided", var.equal = FALSE))

# 2 pre and post within groups
map(0:1, ~ t.test(f1[ hdrinker50 == .x & y2011 == 0, logvodka], f1[ hdrinker50 == .x & y2011 == 1, logvodka], alternative = "two.sided", var.equal = FALSE))


t.test(f1[ hdrinker50 == 1 & y2011 == 1, logvodka], f1[ hdrinker50 == 0 & y2011 == 1, logvodka], alternative = "two.sided", var.equal = FALSE)
# > yes the difference is significant


# so: the heavy drinkers are more likely to drink vodka in 2011 than in 2009, while the non-heavy drinkers are not. 

# e1.D
f1[, emean(logprice_vodka_real), y2011]



# e.2.A & B

p0 <- f1[, .(logvodka=emean(logvodka)), .(year, hdrinker50)] %>% na.omit()




ggplot(p0, aes(x = year, y = logvodka, color = factor(hdrinker50))) + 
  geom_vline(xintercept=2011, linetype='dashed') + 
  geom_point(size=3) + 
  geom_line(size=1.5) + 
  scale_color_manual(values = ecolor_data[grp == 'paul tol 2'][1:2, hex]) + 
  theme_bw() + 
  theme(legend.position = "bottom") 




cof_x <- 'y2011'
cof_y <- 'logvodka'
cof_y_dv  <- cof_y %+% ' ~ ' %+% cof_x
cof_controls <- 'logincome + age + age_2 + smokes + married + college + curwrk + stroke'

cof_simple <- cof_y_dv  %+% '+ hdrinker50 + logvodka*hdrinker50'  %>% formula()
mod_simple <- feols(cof_simple, data = f1,  cluster = 'idind')
feols(logvodka ~ i(y2011, hdrinker50), data = f1,  cluster = 'idind')
feols(logvodka ~ y2011 +  hdrinker50 + y2011*hdrinker50, data = f1,  cluster = 'idind')



cof_simple_controls <- cof_y_dv  %+% '+ hdrinker50 + logvodka*hdrinker50 + ' %+% cof_controls %>% formula()








