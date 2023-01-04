
form_naive_ols <- re78 ~ factor(training) +  education + factor(nodegree) + age + factor(married) + factor(black) + factor(hispanic)
feols(form_naive_ols, a1) %>% etable(vcov='hetero')


form_logit <-  training ~  education + factor(nodegree) + age + factor(married) + factor(black) + factor(hispanic)

form_pop_esti <- factor(training) ~  education + factor(nodegree) + age + factor(married) + factor(black) + factor(hispanic)












ate.any.aug <- PSweight(ps.formula = form_logit, yname = "re78", data = a2,  weight= "IPW", augmentation = TRUE, out.formula = form_target_ols)
form_target_ols
a2




d1 <- NCDS %>% data.table()
d1
ps.any <- Dany ~ white + maemp + as.factor(scht) + as.factor(qmab) + as.factor(qmab2) + as.factor(qvab) + as.factor(qvab2) + paed_u + maed_u + agepa + agema + sib_u + paed_u * agepa + maed_u * agema 
NCDS
ate.any <- PSweight(ps.formula = ps.any, yname = "wage", data = NCDS,  weight= "IPW") 
ate.any <- PSweight(ps.formula = ps.any, yname = "wage", data = NCDS,  weight= "OW") 



dtdesc(a2)
library(tidyverse)
library(haven)

#continuation
N <- nrow(nsw_dw_cpscontrol)
#- Manual with non-normalized weights using all data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)


nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)

#- Manual with normalized weights
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/N),
         norm = y1 - y0)

nsw_dw_cpscontrol %>% 
  pull(ht) %>% 
  mean()

nsw_dw_cpscontrol %>% 
  pull(norm) %>% 
  mean()

#-- trimming propensity score
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  select(-d1, -d0, -y1, -y0, -ht, -norm) %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

N <- nrow(nsw_dw_cpscontrol)

#- Manual with non-normalized weights using trimmed data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)

#- Manual with normalized weights with trimmed data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/N),
         norm = y1 - y0)

nsw_dw_cpscontrol %>% 
  pull(ht) %>% 
  mean()

nsw_dw_cpscontrol %>% 
  pull(norm) %>% 
  mean()
















	a1[, shapiro.test(as.numeric(age)), treat_experiment]	
	a1[, shapiro.test(as.numeric(education)), treat_experiment]	




map_dfr(set_names(thecols), .id='var', ~ {
	a1[, shapiro.test(as.numeric(get(.x))), treat_experiment]	
}) 


t.test(education ~ treat_experiment, data = a2, var.equal = FALSE)  %>% str()
t.test(education ~ treat_experiment, data = a2, var.equal = FALSE)  
t2 <- t.test(education ~ treat_experiment, data = a2, var.equal = FALSE)  %>% names()
t <- t.test(education ~ treat_experiment, data = a2, var.equal = FALSE)  
map(t2, ~ t[[.x]])



t.test(education ~ treat_experiment, data = a2, var.equal = FALSE)[[c(1, 3, j)]]

t.test(education ~ treat_experiment, data = a2, var.equal = FALSE)  







dtx[order(match(x, qc(b,a)))]

names(t.test(1:2))

iris[1:2, 1:2]

dtx <- data.table(x=qc(a,b))

dtx[[ order()  ]]

order(     )

map_int(qc(b,a), ~ which (dtx$x   ==) )







g1 <- vector('a'=)


t3[]






which(p.value, statistic, conf.int, )


t.test(1:2) %>% names()


[  # add names
   ,.(ttname,y,z)]    # put names in first column   



map(set_names(thecols), .id='var', ~ {
t.test(get(.x) ~ treat_experiment, data = a2, var.equal = FALSE)
})


