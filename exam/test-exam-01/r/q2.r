
setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/test-exam-01/')
source('r/pre-processing.r')




# q2.a

f1

feglm(us_hq ~ n_founders + prof + n_ssfp + cleantech + comms +  internet + life_science + medical_dev + semicond + misc, f1, family='probit')










