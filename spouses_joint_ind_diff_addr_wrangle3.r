# There are over 1000 records that are spouses with Joint INDTesting
# aka they want to get mail at the same place, but their addr are differenTesting
# Need to try and determine how this happened, source, and then, which addr is better

# Stopped midway through - see notes in code b/c realized actual problem
# was one of bad data in address table, such that not sure if married == Y 
# actually means they're married, found data on Cook County divorce to cast doubt

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

# SQL Queries - only rerun if needed, they each take 3-5 min to complete

address_181005 <- sqlQuery(squibbs, "( SELECT DISTINCT 
  a.ID_NUMBER ,a.XSEQUENCE ,a.ADDR_TYPE_CODE ,a.ADDR_STATUS_CODE ,a.ADDR_PREF_IND
  ,a.STREET1 ,a.STREET2 ,a.STREET3 ,a.FOREIGN_CITYZIP ,a.CITY ,a.STATE_CODE ,a.ZIPCODE
  ,a.START_DT as ADDR_START_DT ,a.STOP_DT AS ADDR_STOP_DT ,a.XCOMMENT AS XCOMMENT_ADDR 
  ,a.DATE_ADDED AS ADDR_ADD_DT ,a.DATE_MODIFIED AS ADDR_MOD_DT ,a.OPERATOR_NAME AS ADDR_OPERATOR
  ,a.MAIL_RETURNED_NBR ,a.MAIL_RETURNED_DATE ,a.ADDRESS_MOD_DATE ,a.TRACER_SENT_DATE ,a.ADDR_VERIFIED_CODE  
  ,RANK() OVER (PARTITION BY a.id_number ORDER BY a.date_added DESC, rownum) AS rnk 
  FROM advance.address a  )")
entity_data <- sqlQuery(squibbs, "( SELECT 
  e.ID_NUMBER ,e.FIRST_NAME ,e.MIDDLE_NAME ,e.LAST_NAME ,e.PERSON_OR_ORG 
  ,e.RECORD_TYPE_CODE ,e.RECORD_STATUS_CODE ,e.STATUS_CHANGE_DATE ,e.PREF_ADDR_TYPE_CODE 
  ,e.BIRTH_DT ,e.GENDER_CODE ,e.DATE_ADDED ,e.DATE_MODIFIED ,e.OPERATOR_NAME
  
  ,e.MARITAL_STATUS_CODE ,e.JNT_ADDRESS_IND ,e.JNT_MAILINGS_IND ,e.JNT_GIFTS_IND 
  ,e.JNT_TELEPHONE_IND ,e.PREF_JNT_MAIL_NAME1 ,e.PREF_JNT_MAIL_NAME2 ,e.MARITAL_STATUS_CHG_DT
  ,e.MARITAL_COMMENT ,e.MARITAL_SRC_CODE ,e.SPOUSE_STATUS_CODE ,e.SPOUSE_NAME
  ,e.SPOUSE_ID_NUMBER ,e.SPOUSE_SALUTATION ,e.SPOUSE_NICKNAME ,e.SPOUSE_BIRTH_DT
  ,e.MARRIAGE_PLACE ,e.MARRIAGE_DT ,e.MARRIAGE_ALUM_MAG_DATE ,e.JNT_SALUTATION  
  ,e.MAIL_CHANGE_SOURCE ,e.MARITAL_CHANGE_SOURCE ,e.PRIMARY_SPOUSE_ENTITY_IND  
  FROM advance.entity e )")
name_data <- sqlQuery(squibbs, "( SELECT 
  n.ID_NUMBER ,n.XSEQUENCE ,n.NAME_TYPE_CODE ,n.PREF_NAME ,n.REPORT_NAME ,n.XCOMMENT
  ,n.LAST_NAME ,n.FIRST_NAME ,n.MIDDLE_NAME ,n.PREFIX ,n.PERS_SUFFIX ,n.PROF_SUFFIX
  ,n.DATE_ADDED ,n.DATE_MODIFIED ,n.OPERATOR_NAME
  FROM advance.name n)")

base_diff <- read.csv("N:/AIS/Yerik/Data Audits/spouse_joint_ind_diff_addr/spouses_with_same_home_check_mismatched.csv")
base_diff$spouse_link <- 1:nrow(base_diff)
base_diff1 <- subset(base_diff, select=c(1,2,7,9,13))
colnames(base_diff1)[1:4] <- c("id1", "id1_street", "id2", "id2_street")
base_diff1$id1_street <- as.character(base_diff1$id1_street)
base_diff1$id2_street <- as.character(base_diff1$id2_street)
base_diff1$id1_street <- gsub('.','',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub('Avenue','Ave',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub('Court','Ct',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub('Drive','Dr',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub(',',' ',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub('#','',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub(' Road','Rd',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub('st','St',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- gsub('Street','St',base_diff1$id1_street, fixed = TRUE)
base_diff1$id1_street <- tolower(base_diff1$id1_street)
base_diff1$id2_street <- gsub('.','',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub('Avenue','Ave',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub('Court','Ct',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub('Drive','Dr',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub(',',' ',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub('#','',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub(' Road','Rd',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub('st','St',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- gsub('Street','St',base_diff1$id2_street, fixed = TRUE)
base_diff1$id2_street <- tolower(base_diff1$id2_street)


entity_data2 <- subset(entity_data, select=c(1,2,4))

# Play w name data, order as long by primary 1st, then by date modified
# Mod not add b/c mod will never be older than add date
# Dedupe by id, name, type b/c might have >1 idname diff type no delete MD type
name_data2 <- subset(name_data, (name_data$NAME_TYPE_CODE=='AK' | name_data$NAME_TYPE_CODE=='00' | name_data$NAME_TYPE_CODE=='DV' | name_data$NAME_TYPE_CODE=='MD' | name_data$NAME_TYPE_CODE=='MS'), select=c(1,7,3,6,14,15))
name_data2$DATE_ADDED <- paste(substring(name_data2$DATE_ADDED,1,4), substring(name_data2$DATE_ADDED,6,7), substring(name_data2$DATE_ADDED,9,10),sep='')
name_data2$DATE_MODIFIED <- paste(substring(name_data2$DATE_MODIFIED,1,4), substring(name_data2$DATE_MODIFIED,6,7), substring(name_data2$DATE_MODIFIED,9,10),sep='')
name_data2[,5:6] <- sapply(name_data2[,5:6],as.integer)
name_data2$ntc_levels <- ifelse(name_data2$NAME_TYPE_CODE=='00',0,1)
name_data2 <- name_data2[order(name_data2$ID_NUMBER, name_data2$ntc_levels, -(name_data2$DATE_MODIFIED)),]
name_data3 <- subset(name_data2, select = c(1:6))
name_data3$dupes <- duplicated(name_data3[,(1:3)])
name_data3 <- subset(name_data3, name_data3$dupes==FALSE, select=c(1,3,2,4,6))
name_data3 <- within(name_data3, count <- ave(rep(1,nrow(name_data3)),name_data3$ID_NUMBER,FUN=cumsum))
name_data3 <-reshape(name_data3, direction = "wide", idvar = "ID_NUMBER", timevar="count")

address_data2 <- subset(address_181005, select=c(1:8,10:21))
# redo dates so they are numeric yyyymmdd can be compared easily
address_data2$ADDR_ADD_DT <- paste(substring(address_data2$ADDR_ADD_DT,1,4), substring(address_data2$ADDR_ADD_DT,6,7), substring(address_data2$ADDR_ADD_DT,9,10),sep='')
address_data2$ADDR_MOD_DT <- paste(substring(address_data2$ADDR_MOD_DT,1,4), substring(address_data2$ADDR_MOD_DT,6,7), substring(address_data2$ADDR_MOD_DT,9,10),sep='')
address_data2$MAIL_RETURNED_DATE <- paste(substring(address_data2$MAIL_RETURNED_DATE,1,4), substring(address_data2$MAIL_RETURNED_DATE,6,7), substring(address_data2$MAIL_RETURNED_DATE,9,10),sep='')
address_data2$ADDR_ADD_DT <- as.integer(address_data2$ADDR_ADD_DT)
address_data2$ADDR_MOD_DT <- as.integer(address_data2$ADDR_MOD_DT)
address_data2$MAIL_RETURNED_DATE <- as.integer(address_data2$MAIL_RETURNED_DATE)

# sort addresses by id number with most recent address first
# create var that counts each address for each ID, in this sort order
address_data2 <- address_data2[order(address_data2$ID_NUMBER, address_data2$ADDR_STATUS_CODE, -(address_data2$ADDR_ADD_DT)),]
# Some gsub work to remove periods n standardize addresses, OMFG why.
address_data2$STREET1 <- gsub('.','',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub('Avenue','Ave',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub('Court','Ct',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub('Drive','Dr',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub(',',' ',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub(' Road','Rd',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub('st','St',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub('Street','St',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- gsub('#','',address_data2$STREET1, fixed = TRUE)
address_data2$STREET1 <- tolower(address_data2$STREET1)
# Transform street et al columns to character at once for easy comp later
address_data2[,6:10] <- sapply(address_data2[,6:10],as.character)

# Reorder columns, ID dupes, remove dupes, return columns OG order
# Count iterative addresses for each ID
address_data3 <- address_data2[,c(1,6,2:5,7:19)]
address_data3$dupes <- duplicated(address_data3[,(1:2)])
address_data3 <- subset(address_data3, address_data3$dupes==FALSE)
address_data3 <- address_data3[,c(1,3:6,2,7:19)]
address_data3 <- within(address_data3, count <- ave(rep(1,nrow(address_data3)),address_data3$ID_NUMBER,FUN=cumsum))

#####################
## TURN OFF FOR NOW
#####################
# Subset only IDs n street address data
# address_data4 <- subset(address_data3, select=c(1,3:4,6,20))
# address_data4 <- reshape(address_data4, direction = "wide", idvar = "ID_NUMBER", timevar="count")

# compare round 1 - look at ID1, ID1street, ID2...match on ID2, see if any of ID2 addr show up for ID1 current
address_data3$street <- address_data3$STREET1
comp1 <- left_join(base_diff1, address_data3, c("id1"="ID_NUMBER", "id2_street" = "street"))
comp1$id1_has_id2_addr <- ifelse(comp1$STREET1==comp1$id2_street,1,0)
comp1$id1_has_id2_addr_inact <- ifelse(comp1$id1_has_id2_addr==1 & comp1$ADDR_STATUS_CODE=='I',1,0)
comp2 <- left_join(comp1, address_data3, c("id2"="ID_NUMBER", "id1_street" = "street"))
comp2$id2_has_id1_addr <- ifelse(comp2$STREET1.y==comp2$id1_street,1,0)
comp2$id2_has_id1_addr_inact <- ifelse(comp2$id2_has_id1_addr==1 & comp2$ADDR_STATUS_CODE.y=='I',1,0)
comp2 <- left_join(comp2, entity_data2, c("id1"="ID_NUMBER"))
comp2 <- left_join(comp2, entity_data2, c("id2"="ID_NUMBER"))
colnames(comp2)[48:51] <- c('id1_fname','id1_lname','id2_fname','id2_lname')
comp2$id1_fname <- as.character(comp2$id1_fname)
comp2$id1_lname <- as.character(comp2$id1_lname)
comp2$id2_fname <- as.character(comp2$id2_fname)
comp2$id2_lname <- as.character(comp2$id2_lname)
comp2$id1_fname <- gsub('-',' ',comp2$id1_fname, fixed = TRUE)
comp2$id1_lname <- gsub('-',' ',comp2$id1_lname, fixed = TRUE)
comp2$id2_fname <- gsub('-',' ',comp2$id2_fname, fixed = TRUE)
comp2$id2_lname <- gsub('-',' ',comp2$id2_lname, fixed = TRUE)
comp2$id1_fname <- gsub(',','',comp2$id1_fname, fixed = TRUE)
comp2$id1_lname <- gsub(',','',comp2$id1_lname, fixed = TRUE)
comp2$id2_fname <- gsub(',','',comp2$id2_fname, fixed = TRUE)
comp2$id2_lname <- gsub(',','',comp2$id2_lname, fixed = TRUE)

comp2$share_last_name1 <- ifelse(mapply(str_detect, comp2$id1_lname, comp2$id2_lname)==TRUE,1,0)
comp2$share_last_name2 <- ifelse(mapply(str_detect, comp2$id2_lname, comp2$id1_lname)==TRUE,1,0)
comp2$share_last_name <- 0
comp2$share_last_name [(comp2$share_last_name1==1 | comp2$share_last_name2==1)] <- 1
comp2$id1_addr_curr_same_name <- ifelse((comp2$id1_has_id2_addr_inact==1 & is.na(comp2$id2_has_id1_addr_inact) & comp2$share_last_name==1),1,0)
comp2$seasonal_active_flag <- ifelse((comp2$ADDR_TYPE_CODE.x=='S' & comp2$ADDR_STATUS_CODE.x=='A') | (comp2$ADDR_TYPE_CODE.y=='S' & comp2$ADDR_STATUS_CODE.y=='A'),1,0)

# Group 1 subset - id2 has id1 addr, same last name, and 1 seasonal address issue
colnames(comp1)[10:13] <- c('asc_id2_addr_in_id1_rec', 'add_dt_id2str_on_id1rec', 'mod_dt_id2str_on_id1rec','id2_str_from_id1_rec')
comp2$group1 <- ifelse(comp2$id1_addr_curr_same_name==1,1,0)
group1 <- subset(comp2, comp2$group1==1, select=c(1:5,25,54))
#write.csv(group1, file="N:/AIS/Yerik/Data Audits/spouse_joint_ind_diff_addr/spouses_diff_address_confo_group1.csv", na="", row.names=TRUE)

# Start out with Group2 - everyone not in group1 ~~ 800 ppl
# Reorder var into better grouping, rename for clarity, only 1:32 are useful, rest are junk
# For clarity - if suffix .x it refers to ID1, if .y refers to ID2
comp3 <- subset(comp2, (comp2$group1!=1 | is.na(comp2$group1)))
comp3 <- subset(comp3, select=c(5,1,48:49,2,7,8,10,16,17,19:22,3,50:51,4,28:29,31,37,38,40:43,25:26,46:47,54, 6,9,11:15,18,23:24,27,30,32:36,39,44:45,52:53,55:57))
colnames(comp3)[1:32] <- c('group_id', 'id_num1', 'id1_fname', 'id1_lname', 'id1_current_street', 'atc_id2_st_on_id1', 'asc_id2_st_id1_on_rec', 'str_id2_on_id1', 'start_dt_id2_st_on_id1', 'stop_dt_id2_st_on_id1', 'add_dt_id2_st_on_id1', 'mod_dt_id2_st_on_id1', 'operator_made_id2_addr_inact_id1', 'mail_ret_id2_st_id1', 'id_num2', 'id2_fname', 'id2_lname', 'id2_current_street', 'atc_id1_st_on_id2', 'asc_id1_st_id2_on_rec', 'str_id1_on_id2', 'start_dt_id1_st_on_id2', 'stop_dt_id1_st_on_id2', 'add_dt_id1_st_on_id2', 'mod_dt_id1_st_on_id2', 'operator_made_id1_addr_inact_id2', 'mail_ret_id1_st_id2', 'id1_has_id2_addr', 'id1_has_id2_addr_inact', 'id2_has_id1_addr', 'id2_has_id1_addr_inact', 'share_last_name_id1_id2')
address_data_stub <- subset(address_data3, select=c(1,6,15:17))
comp3 <- left_join(comp3, address_data_stub, c("id_num1" = "ID_NUMBER", "id1_current_street"="STREET1"))
colnames(comp3)[58:60] <- c('add_dt_cur_addr1','mod_dt_cur_addr1','oper_cur_addr1')
comp3 <- left_join(comp3, address_data_stub, c("id_num2" = "ID_NUMBER", "id2_current_street"="STREET1"))
colnames(comp3)[61:63] <- c('add_dt_cur_addr2','mod_dt_cur_addr2','oper_cur_addr2')

##############################################
############ UPDATE ON OCTOBER 04 ############
##############################################
# MH is 100% not interested in solving the problem
# Is OK w barfing data that is known to be wrong
# But eh nbd b/c omg metrics
# /edit editorialization/
# K fine we'll just slam in a bunch of shitty data that will make this WORSE
# Because hey, enough lipstick on a pig n you got yourself a lady

# Subset comp3 for only a few pieces make comparison easy
comp_lol <- subset(comp3, select=c(1:5,58:60,15:18,61:63))
base_diff2 <- subset(base_diff, select=c(13,5,11))
comp_lol <- left_join(comp_lol, base_diff2, c("group_id" = "spouse_link"))
comp_lol$LOWER_ID_COMMENT <- as.character(comp_lol$LOWER_ID_COMMENT)
comp_lol$HIGHER_ID_COMMENT <- as.character(comp_lol$HIGHER_ID_COMMENT)
comp_lol$id1_mod_cln_adr <- ifelse(mapply(str_detect, comp_lol$LOWER_ID_COMMENT, "Clean Address")==TRUE,1,0)
comp_lol$id2_mod_cln_adr <- ifelse(mapply(str_detect, comp_lol$HIGHER_ID_COMMENT, "Clean Address")==TRUE,1,0)
comp_lol$check <- str_detect("Clean Address", comp_lol$LOWER_ID_COMMENT)
comp_lol$check2 <- str_detect("Clean Address", comp_lol$HIGHER_ID_COMMENT)

# ADDR_MOD_DATE in Adv...differs from DATE_MODIFIED...OMG OMG OMG


#save(address_181003, file="address_181003.Rda")
#comp4 <- subset(comp3, select=c(1:5,58:60,6:18,61:63,19:32))
#write.csv(address_181003, file="N:/AIS/Yerik/address_181003.csv", na="", row.names=TRUE)
#write.csv(entity_data, file="N:/AIS/Yerik/entity_data.csv", na="", row.names=TRUE)
#write.csv(name_data, file="N:/AIS/Yerik/name_data.csv", na="", row.names=TRUE)
#write.csv(phone_data, file="N:/AIS/Yerik/phone_data.csv", na="", row.names=TRUE)

#other_group <- subset(comp4, select=c(1:5,18:21))

#rm(base_diff, base_diff1, comp1, comp2, comp3, comp4, name_data2, name_data3, address_data2, address_data3, address_data_stub, entity_data2, group1)

rm(marital_status, check, other_group, comp4_inact, comp4_noshare, comp5, count)
#######################
######## NOTES ########
#######################

# As of Wednesday October 3rd, tabling and leaving off for now
# There is not a way to cleanly compare these w the data we have
# The data we have literally does not allow a meaningful comparison
# Conflicting dates for add/stop/start/mod 
# Shared names are divorced via Cook County, in 2013 in some cases
# Dates of address differ Advance vs. AlumniFinder vs NCOA vs EDQ

# Need to take step back and address (pun) underlying data issues
# IE do xyz steps to ensure that our data are good as they stand

# All below is therefore decent code but not helpful
# / END

# Try looking at last names of those no share names
# Do we have record of person having same last name in past
# RESULT - ID'd like a dozen, not enough
####################################
#comp4_noshare <- subset(comp4, comp4$share_last_name_id1_id2==0)
#comp4_noshare <- left_join(comp4_noshare, name_data3, c("id_num1" = "ID_NUMBER"))
#comp4_noshare <- left_join(comp4_noshare, name_data3, c("id_num2" = "ID_NUMBER"))

# Subset for all those where one/both address is inactive on other record
# Not helpful b/c addr added or changed wo regard to if they were present
####################################
#comp4_inact <- subset(comp4, (comp4$id2_has_id1_addr_inact==1 | comp4$id1_has_id2_addr_inact==1))
#comp4_inact <- comp4_inact[order(comp4_inact$share_last_name_id1_id2, comp4_inact$id1_has_id2_addr_inact, comp4_inact$id2_has_id1_addr_inact, decreasing=TRUE ),]

# Looking at iModules flag to see if applies b/c indicates manual update
# Nope b/c yeah but only exists on a dozen or so records
####################################
#comp4$imodules_flag1 <- ifelse(mapply(str_detect, (comp4$operator_made_id2_addr_inact_id1), 'iModule')==TRUE,1,0)
#comp4$imodules_flag2 <- ifelse(mapply(str_detect, (comp4$operator_made_id1_addr_inact_id2), 'iModule')==TRUE,1,0)

# Checked marital status via entity, see if got shoved in back side
####################################
# marital_status <- subset(entity_data, select=c(1,15,22))
# comp4 <- left_join(comp4, marital_status, c("id_num1"="ID_NUMBER"))
# comp4 <- left_join(comp4, marital_status, c("id_num2"="ID_NUMBER"))

# Generic counting function
comp4 %>%
  group_by(share_last_name_id1_id2) %>%
  summarise(number=n())

comp5 <- subset(comp4, comp4$share_last_name_id1_id2==1)

# merged_var <- merged4[1:2,]

small_addr <- address_181005[1:2000,]
write.csv(small_addr, file="N:/AIS/Yerik/Data Audits/Address_et_al/small_addr_pull_181005.csv", na="", row.names=TRUE)




