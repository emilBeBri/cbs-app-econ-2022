setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/exam-real')
source('r/pre-processing.r')

##############  #section #################
# 
#########################################

j1  <- f1[edgrp == '4']



# q3.a

# q3.b

q3b_naive <- feols(dmfwg28 ~ durmf1980 + nondmf1980 + pcemp_m_c2000,  data = j1, vcov='hetero')
q3b <- feols(dmfwg28 ~ durmf1980 + nondmf1980 | pcemp_m_c2000 ~ lncs1980 , data = j1, vcov='hetero')
q3b  %>% etable()
etable(q3b_naive, q3b)



# q3.c


j1[, .(pcemp_m_c2000, lncs1980, dmfwg28)] %>% dtcor() 
feols(pcemp_m_c2000 ~ lncs1980 + durmf1980 + nondmf1980, data = j1, vcov='hetero') %>% etable()
q3b  



# q3.d

q3b_naive <- feols(dmfwg28 ~ durmf1980 + nondmf1980 + pcemp_m_c2000,  data = j1, vcov='hetero')
q3b <- feols(dmfwg28 ~ durmf1980 + nondmf1980 | pcemp_m_c2000 ~ lncs1980 , data = j1, vcov='hetero')
q3b  %>% etable()
etable(q3b_naive, q3b)

# q3.e


q3e <- feols(dmfwg28 ~ durmf1980 + nondmf1980 | pcemp_m_c2000 ~ lncs1980 +  lnlf1980 + mexish1980 , data = j1, vcov='hetero')
q3e  %>% etable()
etable(q3b_naive, q3b, q3e)



j1[, .(pcemp_m_c2000, lncs1980, dmfwg28, lncs1980 ,  lnlf1980 , mexish1980 )] %>% dtcor()
feols(pcemp_m_c2000 ~  lncs1980 +  lnlf1980 + mexish1980  + durmf1980 + nondmf1980, data = j1, vcov='hetero') %>% etable()


q3e


# overidentification test (can only be done with >= 2 instruments)
j1[, iv_res := resid(q3e)]

mod_overid  <- feols(iv_res ~  lncs1980 +  lnlf1980 + mexish1980 + durmf1980 + nondmf1980   , j1 )

oid <- r2(mod_overid)[['r2']] * nrow(j1) 

# not getting the right chisq value, not sure why 
pchisq(oid, df= 1)




# q3.f


# q3.g

# q3.h







