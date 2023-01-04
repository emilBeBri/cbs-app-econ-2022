use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
mlogit prog i.ses write
margins ses, atmeans predict(outcome(1)) 
marginsplot, name(general)
margins ses, atmeans predict(outcome(2))
marginsplot, name(academic)
margins ses, atmeans predict(outcome(3))
marginsplot, name(vocational)
graph combine general academic vocational, ycommon

