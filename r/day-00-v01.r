

# first time
library(haven)

setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course')

e1 <- read_dta('input/exam_data.dta')
setDT(e1)


names(e1)
e1[age > 40, TRUE := over_40]
e1[age <= 40, FALSE := over_40]

e1[, emean(), ][order()]
e1[, .N, hdrinker][order(hdrinker)]
e1[, .N, college][order(college)]

e1[, .N, .(hdrinker, college)][order(hdrinker, college)]
a1 <- e1[,.N, .(hdrinker, college)][, `:=` (pct = round(100*(N/sum(N)),1), and = N/sum(N)),.(hdrinker)][order(N)]
a1[order(-and)]


# t-test
t.test( e1[hdrinker == 1, college ] , e1[hdrinker == 0, college ], alternative = "two.sided", var.equal = FALSE)

e1[, emean(college), hdrinker]


ggplot(e1, aes(age)) + geom_histogram() 

ggplot(e1, aes(age)) + geom_histogram() + facet_wrap(~ hdrinker)








summary(e1)




v2(e1)



e1








