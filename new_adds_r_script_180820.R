# Need to add attendees to Founders Dinner 2017 event
# Got list of those to add, of course wo IDs
# Get list of IDs from entity table, match by name against list of FD additions
# Use alfred clipboard to mass update

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

all_orgs <- sqlQuery(squibbs, "(
  SELECT e.id_number ,e.PREF_MAIL_NAME
  FROM advance.entity e
  WHERE PERSON_OR_ORG = 'O'
)")

founder_list <- read.csv("N:/AIS/Yerik/Data Audits/founders_dinner_2017/FOUNDER_DINNER_new_ADDS_Intellect_Space_Group2_180908.csv")
all_orgs$PREF_MAIL_NAME <- tolower(all_orgs$PREF_MAIL_NAME)
founder_list$Company <- tolower(founder_list$Company)

founder_list_IDs <- left_join(founder_list, all_orgs,c("Company" = "PREF_MAIL_NAME"))

#write.csv(founder_list_IDs, file="N:/AIS/Yerik/Data Audits/founders_dinner_2017/FOUNDER_DINNER_new_ADDS_w_ID_180820.csv", na="", row.names=TRUE)
