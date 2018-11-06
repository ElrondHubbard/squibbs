# MH needs to know how many people we killed off in ADV via BB dead people append
# Pull out big list of people n match against master list we got from BB, see what happened.

# Load libraries
install.packages(c("dplyr" , "reshape2", "tidyr", "tidyverse", "knitr", "purrr", "stringdist",
  "RODBC", "tools"))
library(dplyr)
library(reshape2)
library(tidyr)
library(tidyverse)
library(knitr)
library(purrr)
library(stringdist)
library(RODBC)
library(tools)
squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")

# Open file with addresses to update, subset out junk columns
base_dead <- read.csv("N:/AIS/Yerik/Data Audits/Deceased Cleanup/FILE_RECIEVED_FROM_BB_DeceasedFinder_Dec2017_RES.csv")
bb_dead <- subset(base_dead, select=c(2,17:19))
  
# Pull like, all the dead people
big_dead_list <- sqlQuery(squibbs, "(
  SELECT DISTINCT 
  e.id_number ,e.first_name ,e.last_name ,e.record_type_code ,e.record_status_code 
  ,e.status_change_date ,e.operator_name  ,e.death_dt ,e.xcomment 
  FROM 
  advance.entity e  )")

bb_dead2 <- left_join(bb_dead, big_dead_list, c("ID_NUMBER" = "ID_NUMBER"))

bb_dead2$death_update_date <- paste(substring(bb_dead2$STATUS_CHANGE_DATE,1,4),substring(bb_dead2$STATUS_CHANGE_DATE,6,7),substring(bb_dead2$STATUS_CHANGE_DATE,9,10),sep='')
bb_dead2$death_update_date <- as.integer(bb_dead2$death_update_date)
bb_dead3 <- subset(bb_dead2, death_update_date >=20180101)

bb_dead3 %>%
  group_by(OPERATOR_NAME) %>%
  summarise(number=n())

#rm(base_dead, bb_dead, big_dead_list, bb_dead2)

# No outfile b/c just need counts as of August 31 2018. Might need more later, if so run n outfile