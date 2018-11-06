# Stephanie Kimmel email 8/15 - has list of CFR christmas card
# Make sure data are correct - name, contact, title, address, etc
# Use list she sent connect to DB pull out all possible matches
# Based on org name, ensure the other data are accurate

# B$$ thinks yes for most part but might be missing some

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

# Open data and pull data from SQL

cfr_list <- read.csv("N:/AIS/Yerik/Christmas Card CleanUp/to_printer_loyola_cfr_list_11_6_17.csv")

all_orgs <- sqlQuery(squibbs, "(
  SELECT e.ID_NUMBER ,e.PREF_MAIL_NAME
  FROM advance.entity e
  WHERE e.PERSON_OR_ORG = 'O' )")

all_contacts <- sqlQuery(squibbs, "(
  SELECT c.ID_NUMBER ,c.CONTACT_NAME ,c.TITLE ,c.CONTACT_CODE ,c.START_DT ,c.STOP_DT
  FROM advance.contact c )")

all_addresses <- sqlQuery(squibbs, "(
  SELECT a.ID_NUMBER ,a.ADDR_TYPE_CODE ,a.ADDR_STATUS_CODE ,a.ADDR_PREF_IND 
  ,a.CARE_OF ,a.COMPANY_NAME_1 ,a.COMPANY_NAME_2 ,a.BUSINESS_TITLE ,a.STREET1 
  ,a.STREET2 ,a.STREET3 ,a.FOREIGN_CITYZIP ,a.CITY ,a.STATE_CODE ,a.ZIPCODE
  FROM advance.address a )") 

# Make all data lowercase for better / easier matching
cfr_list_l <- as.data.frame(sapply(cfr_list, tolower))
all_orgs_l <- as.data.frame(sapply(all_orgs, tolower))
all_contacts_l <- as.data.frame(sapply(all_contacts, tolower))
all_addresses_l <- as.data.frame(sapply(all_addresses, tolower))

# Change vartype ID NUMBER to factor
all_orgs_l <- as.data.frame(sapply(all_orgs, tolower))
all_contacts_l$ID_NUMBER <- as.factor(all_contacts_l$ID_NUMBER)
all_addresses_l <- as.data.frame(sapply(all_addresses, tolower))


# Join datasets - there are some dupes in org name but NO dupes w org name + pref_care_of
cfr_list_IDs <- left_join(cfr_list_l, all_orgs_l, c("PREF_MAIL_NAME", "PREF_MAIL_NAME"))
cfr_list_IDs <- within(cfr_list_IDs, count <- ave(rep(1,nrow(cfr_list_IDs)),cfr_list_IDs$PREF_MAIL_NAME, cfr_list_IDs$Pref_Care_of,FUN=cumsum))
cfr_list_IDs <- subset(cfr_list_IDs, count == 1)
cfr_list_IDs2 <- left_join(cfr_list_IDs, all_contacts_l, c("ID_NUMBER" = "ID_NUMBER"))
cfr_list_IDs3 <- left_join(cfr_list_IDs, all_addresses_l, c("ID_NUMBER", "ID_NUMBER"))
cfr_list_IDs3$Pref_Street_1 <- as.character(cfr_list_IDs3$Pref_Street_1)
cfr_list_IDs3$STREET1 <- as.character(cfr_list_IDs3$STREET1)
cfr_list_IDs3$addr_check <- ifelse((cfr_list_IDs3$Pref_Street_1 == cfr_list_IDs3$STREET1),1,0)

# Count the #1 matches
cfr_list_IDs3 %>%
  group_by(addr_check) %>%
  summarise(number=n())

cfr_list_IDs4 <- subset(cfr_list_IDs3, cfr_list_IDs3$addr_check==1, select = c(8,24))
cfr_list_IDs5 <- left_join(cfr_list_IDs, cfr_list_IDs4, c("ID_NUMBER" = "ID_NUMBER"))
cfr_list_IDs5 <- within(cfr_list_IDs5, count <- ave(rep(1,nrow(cfr_list_IDs5)),cfr_list_IDs5$PREF_MAIL_NAME, cfr_list_IDs5$Pref_Care_of,FUN=cumsum))
cfr_list_IDs5 <- subset(cfr_list_IDs5, count == 1)




