# TEMPLATE add brief notes here
# TEMPALTE add brief notes here


# Load libraries
install.packages(c("dplyr" , "reshape2", "tidyr", "tidyverse", "knitr", "purrr", "stringdist", "RODBC", "tools", "editData"))
library(dplyr)
library(reshape2)
library(tidyr)
library(tidyverse)
library(knitr)
library(purrr)
library(stringdist)
library(RODBC)
library(tools)
library(editData)
squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")

# See R file below for template queries of ADV data to run through R
# N:/AIS/Yerik/_Template Scripts/_SQL_QUERIES_IN_R.R  
# Remember can only query, not write or update, but is PRD aka real-time
# See R cheats file for basic commands etc at
# N:/AIS/Yerik/R_Cheats.xlsx

# FOR ANY SQL QUERIES RUN - add date to name of R file, so know data are
# Current in ADV as of date...easy to leave one address file sitting for a week