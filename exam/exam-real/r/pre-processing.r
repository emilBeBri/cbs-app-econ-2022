setwd('/home/emil/Dropbox/arbejde/cbs/phd/courses/econometrics-course/exam/exam-real/')



##############  #afsnit #################
#  libraries  
#########################################


# supress startup messages
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("clipr"))
suppressPackageStartupMessages(library("fst"))
# suppressPackageStartupMessages(library("bit64")) # problems with the conflicted:: package, see https://github.com/r-lib/conflicted/issues/48
suppressPackageStartupMessages(library("qs"))
# suppressPackageStartupMessages(library("texreg"))
# suppressPackageStartupMessages(library("ergm"))


# load always
  library(conflicted)
  # library(assertthat) # deprecated
  # library(assertive)
  library(devtools)
  library(purrr)
  # library(tinytest)
  # visual
  library(RColorBrewer)
  library(viridis)
  # tools
  library(scales)
  # library(knitr)
  # library(rmarkdown)
  library(languageserver)
  # library(DescTools)
  # read/write files
  library(readxl)
  library(writexl)
  library(qs)
  library(data.table)
  # library(bit64) # problems with the conflicted:: package, see https://github.com/r-lib/conflicted/issues/48
  # library(wrapr)
  # din egen pakke!
  library(dttools) 
  # SNA
# library(igraph)
# library(ggraph)

library(conflicted)
library(purrr)
library(RColorBrewer)
library(viridis)
library(scales)
library(haven)
library(Hmisc)
library(GGally)
library(plotly)
library(fixest)
library(Hmisc)
library(modelsummary)
library(magrittr)
library(knitr)
library(kableExtra)
library(marginaleffects)


#######  #subafsnit #######
# conflicted preferences

conflict_prefer('discard', 'purrr') 
conflict_prefer('%+%', 'dttools')
conflict_prefer('%nin%', 'dttools')
conflict_prefer('%like%', 'data.table')
conflict_prefer('transpose', 'data.table')
conflict_prefer('year', 'data.table')
conflict_prefer('filter', 'stats')
# conflict_prefer('first', 'data.table')


##### subafsnit slut ######

source('/home/emil/Dropbox/data-science/R/.Rprofile')

source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtcor.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/narv_omit.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dtdesc.R')
source('/home/emil/Dropbox/data-science/R/r-package-development/dttools/R/dt_ttest.R')
conflict_prefer('discard', 'purrr') 
conflict_prefer('%+%', 'dttools')
conflict_prefer('%nin%', 'dttools')
conflict_prefer('%like%', 'data.table')
conflict_prefer('transpose', 'data.table')
conflict_prefer('year', 'data.table')
conflict_prefer('filter', 'stats')
# conflict_prefer('first', 'data.table')



f1 <- read_dta('input/stackcity.dta') %>% data.table()
setnames_clean(f1)


f1[, edgrp := factor(edgrp)]

