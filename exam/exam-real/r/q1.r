# names(f1)
#
# "cmsa99"             "edgrp"              "pctbl1980"          "erate_f1980lo"      "erate_f1980hi"      "urate1980"          "mexish1980"         "nondmf1980"        
#   "durmf1980"          "lnlf1980"           "lncs1980"           "pcemp_m_c2000"      "dmfwg28"            "drtc28"             "nondmf1980lo"       "durmf1980lo"       
#  "dlncs28"            "hig_hpcemp_m_c2000"
#
# cmsa99             edgrp              pctbl1980          erate_f1980lo      erate_f1980hi      urate1980          mexish1980         nondmf1980        
#   durmf1980          lnlf1980           lncs1980           pcemp_m_c2000      dmfwg28            drtc28             nondmf1980lo       durmf1980lo       
#  dlncs28            hig_hpcemp_m_c2000
#
#

setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/exam-real')
source('r/pre-processing.r')

##############  #section #################
# 
#########################################


# q1.a



# dt_ttest(f1, 'edgrp', 'dmfwg28')
t.test(f1[edgrp == 2, dmfwg28], f1[edgrp == 4, dmfwg28], alternative = 'two.sided', var.equal = TRUE, conf.level = 0.95) 


# f1[edgrp == 2, mean(dmfwg28)] 
# f1[edgrp == 4, mean(dmfwg28)]




# q1.b


f1[urate1980 <= median(urate1980) , above_p50_urate1980 :=  FALSE ]
f1[urate1980 > median(urate1980) , above_p50_urate1980 :=  TRUE ]
# f1[, .N, above_p50_urate1980]

q1b <- unique(f1, by=qc(cmsa99, urate1980, above_p50_urate1980)) 
# dt_ttest(f1, 'above_p50_urate1980', 'pcemp_m_c2000')
t.test(q1b[above_p50_urate1980 == FALSE, pcemp_m_c2000], q1b[above_p50_urate1980 == TRUE, pcemp_m_c2000], alternative = 'two.sided', var.equal = TRUE, conf.level = 0.95) 

# q1b[, .N, above_p50_urate1980]


# f1[, .N]


# q1.c

# map(unique(f1$edgrp), ~ dtcor(f1[edgrp == .x, .(dmfwg28, pcemp_m_c2000)],) )	

# dtcor(f1[edgrp == 2, .(dmfwg28, pcemp_m_c2000)])

# map(unique(f1$edgrp), ~ f1[edgrp == .x, .(dmfwg28, pcemp_m_c2000)] %>% dtcor()) 
map_dfr(unique(f1$edgrp), .id='edgrp', ~ f1[edgrp == .x, .(dmfwg28, pcemp_m_c2000)] %>% dtcor()) 
dtcor(f1[, .(pcemp_m_c2000, drtc28)])



# q1.d


mod_naive_ols <- feols(dmfwg28 ~ edgrp + pcemp_m_c2000, data = f1, vcov='hetero')
mod_naive_ols  %>% etable()



# q1.e

q1e  <- unique(f1[, .(cmsa99, drtc28, pcemp_m_c2000)] ) 
mod_q1e <- feols(q1e, drtc28 ~ pcemp_m_c2000, vcov='hetero')
mod_q1e  %>% etable()



# q1.f



mod_q1f <- feols(dmfwg28 ~ edgrp * pcemp_m_c2000, data = f1, vcov='hetero')
etable(mod_q1f, mod_naive_ols)


# #vendtilbage
marg_q1f <- mod_q1f %>% marginaleffects()


wald(mod_q1f, 'edgrp.*pcemp')



# q1.g 


# can you even do this? 




mod_q1g <- feols(dmfwg28 ~ edgrp + pcemp_m_c2000 + durmf1980 + nondmf1980 + durmf1980lo + nondmf1980lo, data = f1, vcov='hetero')
etable(mod_q1g, mod_naive_ols)
wald(mod_q1g, 'durm|nondm')

f1[1:15, .(cmsa99, edgrp , pcemp_m_c2000 , durmf1980 , nondmf1980 , durmf1980lo , nondmf1980lo) ]
# View(f1[1:15, .(cmsa99, edgrp , pcemp_m_c2000 , durmf1980 , nondmf1980 , durmf1980lo , nondmf1980lo) ])



