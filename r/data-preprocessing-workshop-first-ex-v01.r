library(haven)
library(Hmisc)
library(GGally)
library(plotly)
library(fixest)
library(Hmisc)
library(marginaleffects)



ba1a <- read_dta('input/workshop-first-ex-cis3samp.dta') %>% data.table()
ba1b <- read_dta('input/workshop-first-ex-register.dta') %>% data.table()
ba1c <- read_dta('input/workshop-first-ex-exercise3_extra_data.dta') %>% data.table()

# merge onto
ba2  <- ba1a[ba1b, on='id'][ba1c, on='id']


ba3  <- ba2[, .(prodnew, extsource, inconst, xinterm, export00, capex00, employ00, 
		# used for creating new variable, remove after that
			idbrturn,
		# for poisson / logit
		prodinov, patapply, pcoop, market
		)]

# GENERATING KEY VARIABLES
ba3[, `:=` ( rdint = xinterm/idbrturn, expint = export00/idbrturn, capex00 = capex00/idbrturn, lempl00 = log(employ00))]
ba3[, rdintpct := rdint*100]
# remove idbturn
thenotcols <- qc('idbrturn', 'employ00')
ba3 <- ba3[, !..thenotcols]


# remove observations 
ba4 <- ba3[(rdint <= 1 | is.na(rdint) ) & expint <= 1]




# make factors out of vars
ba4[, market := factor(market, levels=1:4, labels=c('local', 'regional', 'domestic', 'international'))]
# ba4[, prodinov := factor(prodinov, levels=0:1, labels=c('no', 'yes')) ]


ba4  <- narv_omit(ba4) 


