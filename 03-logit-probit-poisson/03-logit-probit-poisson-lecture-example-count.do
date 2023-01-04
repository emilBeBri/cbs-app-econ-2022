* poisson
use https://stats.idre.ucla.edu/stat/stata/dae/poisson_sim, clear
sum
tabstat num_awards, by(prog) stats(mean sd n)
histogram num_awards, discrete freq 
poisson num_awards i.prog math
margins prog, atmeans
marginsplot 

* negative binomial
use https://stats.idre.ucla.edu/stat/stata/dae/nb_data, clear
histogram daysabs, discrete freq
tabstat daysabs, by(prog) stats(mean v n)
nbreg daysabs math i.prog
nbreg, irr
margins prog, atmeans
marginsplot 
poisson daysabs math i.prog
poisson daysabs math i.prog, r

* zero-inflated negative binomial
use http://www.stata-press.com/data/r10/fish, clear
sum 
histogram count, discrete freq
zinb count child i.camper, inflate(persons) 


