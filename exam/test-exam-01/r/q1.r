setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/test-exam-01/')
source('r/pre-processing.r')

##############  #section #################
# q1  
#########################################



# q1.a
f1[,.N, .(us_hq)][, `:=` (pct = round(100*(N/sum(N)),1))][order(N)] 


# q1.b
f1[ total_raised  > 0, .N  ]
f1[ total_raised  > 0, mean(total_raised)  ]


# q1.c 
ggplot(f1, aes(total_raised)) + geom_histogram(binwidth=5) + theme_bb()

# q1.d
# A> they add one so that the one with zero vc raised can included in the log transformation, log(0) can't be defined.
# The log transformation normalizes the distribution, so that it is more symmetric and less skewed.
ggplot(f1, aes(ln_raised)) + geom_histogram() + theme_bb()
ggplot(f1, aes(ln_raised)) + geom_histogram(binwidth=0.2) + theme_bb()


# dtdesc(f1[,.(total_raised, ln_raised)])

# q1.e
t.test(total_raised ~ us_hq, data = f1, alternative = "two.sided", var.equal = FALSE)

# q1.f

feols(ln_raised ~ us_hq + n_founders + prof + n_ssfp + cleantech + comms +  internet + life_science + medical_dev + semicond + misc, data = f1, vcov='hetero') 


# q1.g

# don’t get the question.  is this an interaction? Or just including them? 

f1[, .N, .(us_hq, ca_hq, ny_hq)]

# q1.h (test that this result is correct w. Someone else)


feols(ln_raised ~ i(us_hq, n_founders) + prof + n_ssfp + cleantech + comms +  internet + life_science + medical_dev + semicond + misc, data = f1, vcov='hetero') 





