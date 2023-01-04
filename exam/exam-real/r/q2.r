setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/exam-real')
source('r/pre-processing.r')

##############  #section #################
# 
#########################################


# q2.a




q2a <- unique(f1, by=qc(cmsa99, hig_hpcemp_m_c2000 , urate1980 , pctbl1980 , lnlf1980 , erate_f1980lo , erate_f1980hi) )
# thecols <- qc(cmsa99, hig_hpcemp_m_c2000 , urate1980 , pctbl1980 , lnlf1980 , erate_f1980lo , erate_f1980hi)
# View(f1[1:15, ..thecols])



mod_q2a <- feglm(hig_hpcemp_m_c2000 ~ urate1980 + pctbl1980 + lnlf1980 + erate_f1980lo + erate_f1980hi, q2a, family='logit', vcov='hetero')
etable(mod_q2a)



# q2.b




# q2.c

# q2.d

# q2.e

# q2.f


# q2.g

# q2.h







