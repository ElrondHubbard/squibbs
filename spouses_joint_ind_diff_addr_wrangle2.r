# There are over 1000 records that are spouses with Joint INDTesting
# aka they want to get mail at the same place, but their addr are differenTesting
# Need to try and determine how this happened, source, and then, which addr is better

# V2 goes beyond V1 

# Load libraries
install.packages(c("dplyr" , "reshape2", "tidyr", "tidyverse", "knitr", "purrr", "stringdist",
  "RODBC", "tools", "editData"))
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

base_diff <- read.csv("N:/AIS/Yerik/Data Audits/spouse_joint_ind_diff_addr/spouses_with_same_home_check_mismatched.csv")
base_diff1 <- subset(base_diff, select=c(1,2,7,9))
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
base_diff1$spouse_link <- 1:nrow(base_diff1)

address_data <- sqlQuery(squibbs, "( SELECT DISTINCT 
  a.ID_NUMBER ,a.XSEQUENCE ,a.ADDR_TYPE_CODE ,a.ADDR_STATUS_CODE ,a.ADDR_PREF_IND
  ,a.STREET1 ,a.STREET2 ,a.STREET3 ,a.FOREIGN_CITYZIP ,a.CITY ,a.STATE_CODE ,a.ZIPCODE
  ,a.START_DT as ADDR_START_DT ,a.STOP_DT AS ADDR_STOP_DT ,a.XCOMMENT AS XCOMMENT_ADDR 
  ,a.DATE_ADDED AS ADDR_ADD_DT ,a.DATE_MODIFIED AS ADDR_MOD_DT ,a.OPERATOR_NAME AS ADDR_OPERATOR
  ,a.MAIL_RETURNED_NBR ,a.MAIL_RETURNED_DATE  
  ,RANK() OVER (PARTITION BY a.id_number ORDER BY a.date_added DESC, rownum) AS rnk 
  FROM advance.address a  )")

entity_data <- sqlQuery(squibbs, "( SELECT 
  e.ID_NUMBER ,e.FIRST_NAME ,e.MIDDLE_NAME ,e.LAST_NAME ,e.PERSON_OR_ORG 
  ,e.RECORD_TYPE_CODE ,e.RECORD_STATUS_CODE ,e.STATUS_CHANGE_DATE ,e.PREF_ADDR_TYPE_CODE 
  ,e.BIRTH_DT ,e.GENDER_CODE ,e.DATE_ADDED ,e.DATE_MODIFIED ,e.OPERATOR_NAME
  FROM advance.entity e )")
entity_data2 <- subset(entity_data, select=c(1,2,4))

name_data <- sqlQuery(squibbs, "( SELECT 
  n.ID_NUMBER ,n.XSEQUENCE ,n.NAME_TYPE_CODE ,n.PREF_NAME ,n.REPORT_NAME ,n.XCOMMENT
  ,n.LAST_NAME ,n.FIRST_NAME ,n.MIDDLE_NAME ,n.PREFIX ,n.PERS_SUFFIX ,n.PROF_SUFFIX
  FROM advance.name n)")
name_data2 <- subset(name_data, 
  (name_data$NAME_TYPE_CODE=='00' | name_data$NAME_TYPE_CODE=='AK' | name_data$NAME_TYPE_CODE=='00'
  | name_data$NAME_TYPE_CODE=='DV' | name_data$NAME_TYPE_CODE=='MD' | name_data$NAME_TYPE_CODE=='MS'),
  select=c(1,3,6:8))
name_data2 <- within(name_data2, count <- ave(rep(1,nrow(name_data2)),name_data2$ID_NUMBER,FUN=cumsum))
name_data3 <-reshape(name_data2, direction = "wide", idvar = "ID_NUMBER", timevar="count")

address_data2 <- subset(address_data, select=c(1:8,10:21))
# redo dates so they are numeric yyyymmdd can be compared easily
address_data2$ADDR_ADD_DT <- paste(
  substring(address_data2$ADDR_ADD_DT,1,4),
  substring(address_data2$ADDR_ADD_DT,6,7),
  substring(address_data2$ADDR_ADD_DT,9,10),sep='')
address_data2$ADDR_MOD_DT <- paste(
  substring(address_data2$ADDR_MOD_DT,1,4),
  substring(address_data2$ADDR_MOD_DT,6,7),
  substring(address_data2$ADDR_MOD_DT,9,10),sep='')
address_data2$MAIL_RETURNED_DATE <- paste(
  substring(address_data2$MAIL_RETURNED_DATE,1,4),
  substring(address_data2$MAIL_RETURNED_DATE,6,7),
  substring(address_data2$MAIL_RETURNED_DATE,9,10),sep='')
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
comp2$group1 <- ifelse((comp2$id1_addr_curr_same_name==1 | comp2$seasonal_active_flag==1),1,0)


## Start out with Group2 - everyone not in group1 ~~ 800 ppl
comp3 <- subset(comp2, (comp2$group1!=1 | is.na(comp2$group1)))
rm(address_data4, address2, base_diff, base_diff_hrz, comp, comp1)









id1_has_id2_subset <- subset(comp2, (!is.na(comp2$id1_has_id2_addr_inact) | !is.na(comp2$id2_has_id1_addr_inact)))
id1_has_id2_subset$flag <- ifelse( !is.na(id1_has_id2_subset$id2_has_id1_addr) & !is.na(id1_has_id2_subset$id1_has_id2_addr),1,0)


comp

comp1 %>%
  group_by(id1_has_id2_addr_inact) %>%
  summarise(number=n())
comp2 %>%
  group_by(group1) %>%
  summarise(number=n())
comp3 %>%
  #group_by(seasonal_active_flag) %>%
  summarise(number=n())


id1_has_id2_addr




colnames(comp1)[10:13] <- c('asc_id2_addr_in_id1_rec', 'add_dt_id2str_on_id1rec', 'mod_dt_id2str_on_id1rec','id2_str_from_id1_rec')


range(address_data3$count)
# K so way too many dupes here, remove them count again UNIQUE addr

comp1$check <- ifelse(comp1$id1_street==comp1$STREET1.1, 1,0)




addr_col_list <- 
# list of the columns or varnames of all addr var
    c( )
comp1$match <- filter(tmp_dupe_wk1, !EMAIL_ADDRESS %in% exclude_list)




comp1 %>%
  group_by(check) %>%
  summarise(number=n())

address_data2$sort_index <- 1:nrow(address_data2)
address_data2 <- address_data2[which(duplicated(address_data2[,c('ID_NUMBER','STREET1')])==T),]


address_data2$dupes <- ifelse(mapply(duplicated, address_data2$ID_NUMBER, address_data2$STREET1)==TRUE,1,0)

address_data2 <- subset(address_data2, select=c(1:20))

address_data3

address_data2$dupes2 <- duplicated(address_data2[,(1,6)])
address_data2$STREET1 <- as.character(address_data2$STREET1)
address_data2$dupes <- duplicated(address_data2$ID_NUMBER + address_data2$STREET1)

  

# dupes by id, street, and address status
#address_data2$dupes1 <- duplicated(address_data2[,1:3])
# dupes by id and street IE if rec has multiple of same addr, will label as dupes
# subset only unique from dupe2 ID var
address_data2 <- subset(address_data2, dupes2==FALSE, select=c(1:5))
# duplicate street address so can visual compare
address_data2$street <- address_data2$ADV_STREET

# comparison dataset
comp <- base_diff1
comp$id1_street <- as.character(comp$id1_street)
comp$id2_street <- as.character(comp$id2_street)
comp$id1_street <- tolower(comp$id1_street)
comp$id2_street <- tolower(comp$id2_street)
comp <- left_join(comp, address_data2, c("id1"="ID_NUMBER", "id1_street" = "ADV_STREET"))
comp <- left_join(comp, address_data2, c("id2"="ID_NUMBER", "id2_street" = "ADV_STREET"))
comp <- comp[,c(5,1,2,7,8,3,4,11,12)]
colnames(comp)[4:5] <- c('id1_add_dt', 'id1_mod_dt')
colnames(comp)[8:9] <- c('id2_add_dt', 'id2_mod_dt')

# joins to enable comparison
# check to see if for each id, is current addr in other id as old inactive
# IE did one addr get updated when other did not
# Also check last names cursory check for divorce, changed name
comp <- left_join(comp, address_data2, c("id1"="ID_NUMBER", "id2_street" = "STREET1"))
colnames(comp)[10:13] <- c('asc_id2_addr_in_id1_rec', 'add_dt_id2str_on_id1rec', 'mod_dt_id2str_on_id1rec','id2_str_from_id1_rec')
comp <- left_join(comp, address_data2, c("id2"="ID_NUMBER", "id1_street" = "ADV_STREET"))
colnames(comp)[14:17] <- c('asc_id1_addr_in_id2_rec', 'add_dt_id1str_on_id2rec', 'mod_dt_id1str_on_id2rec','id1_str_from_id2_rec')
comp <- left_join(comp, entity_data, c("id1"="ID_NUMBER"))
comp <- left_join(comp, entity_data, c("id2"="ID_NUMBER"))
colnames(comp)[18:21] <- c('id1_fname','id1_lname','id2_fname','id2_lname')
comp <- subset(comp, select=c(1:2,18:19,3:6,20:21,7:17))
comp$id1_fname <- as.character(comp$id1_fname)
comp$id1_lname <- as.character(comp$id1_lname)
comp$id2_fname <- as.character(comp$id2_fname)
comp$id2_lname <- as.character(comp$id2_lname)

comp$id1_fname <- gsub('-',' ',comp$id1_fname, fixed = TRUE)
comp$id1_lname <- gsub('-',' ',comp$id1_lname, fixed = TRUE)
comp$id2_fname <- gsub('-',' ',comp$id2_fname, fixed = TRUE)
comp$id2_lname <- gsub('-',' ',comp$id2_lname, fixed = TRUE)
comp$id1_fname <- gsub(',','',comp$id1_fname, fixed = TRUE)
comp$id1_lname <- gsub(',','',comp$id1_lname, fixed = TRUE)
comp$id2_fname <- gsub(',','',comp$id2_fname, fixed = TRUE)
comp$id2_lname <- gsub(',','',comp$id2_lname, fixed = TRUE)

comp$share_last_name1 <- ifelse(mapply(str_detect, comp$id1_lname, comp$id2_lname)==TRUE,1,0)
comp$share_last_name2 <- ifelse(mapply(str_detect, comp$id2_lname, comp$id1_lname)==TRUE,1,0)
comp$share_last_name <- 0
comp$share_last_name [(comp$share_last_name1==1 | comp$share_last_name2==1)] <- 1
comp <- subset(comp, select=c(1:21,24))
comp$id1_addr_curr_same_name <- ifelse((comp$id2_street == comp$id2_str_from_id1_rec & comp$asc_id2_addr_in_id1_rec== 'I' & is.na(comp$id1_str_from_id2_rec ==1 & comp$share_last_name==1)),1,0)

# Subset 1 - id2 addr is inactive id1 addr, same last name
id1_addr_curr_same_name <- subset(comp, comp$id2_street == comp$id2_str_from_id1_rec & comp$asc_id2_addr_in_id1_rec== 'I' & is.na(comp$id1_str_from_id2_rec) & comp$share_last_name==1)

# Remove subset1 from comp dataset, keep working
comp2 <- subset(comp, comp$id1_addr_curr_same_name ==0 | is.na(comp$id1_addr_curr_same_name))

# In comp2, flag if lname is same n lev dist < 5 or so, try n cut down more, 

# do lev distance, see if append apt, missing st direction
comp2$lev_dist <- mapply(adist, comp2$id1_street, comp2$id2_street)

comp3 <- subset(comp2, comp2$lev_dist < 5, select=c(2,5:7,8,11:13,22,24))

comp2 <- comp2[order(-(comp2$share_last_name)),]






comp2 %>%
  group_by(share_last_name & lev_dist < 5) %>%
  summarise(number=n())


comp2$check <- ifelse(mapply(str_detect,comp2$id1_lname, comp2$id2_lname)==TRUE,1,0)
comp2$check2 <- mapply(str_detect,comp2$id2_lname, comp2$id1_lname)




comp$id1_addr_curr_same_name <- NULL
# Subset 1 - Address on ID1 also shows up on ID2 as inactive
id1_addr_is_current <- subset(comp, comp$id1_addr_is_current==1)
comp2 <- subset(comp, comp$id1_addr_is_current != 1 | is.na(comp$id1_addr_is_current))

comp <- subset(comp, select=c(1:13))


comp3$id2_check1 <- ifelse((comp3$id1_street == comp3$id1_str_from_id2_rec),1,0)
comp3$id2_check2 <- ifelse((comp3$asc_id1_addr_in_id2_rec== 'I'),1,0)








comp %>%
  group_by(id1_addr_is_current) %>%
  summarise(number=n())
#write.csv(base_diff_vrt_head, file="N:/AIS/Yerik/Data Audits/spouse_joint_ind_diff_addr/top_50_spouse_diff_addr.csv", na="", row.names=TRUE)



id1_id2_inact_ma <- subset(test3, test3$id2_str_asc_id1_rec=='I', select=c(1:5,7))


address_data2 %>%
group_by(dupes) %>%
summarise(number=n())




base_diff1 <- subset(base_diff, select=c(1:6,13))
colnames(base_diff1)[1:6] <- c("id_number", "street1", "addr_type", "operator", "comment", "modified")
base_diff2 <- subset(base_diff, select=c(7:13))
colnames(base_diff2)[1:6] <- c("id_number", "addr_type", "street1","operator", "comment", "modified")
base_diff_vrt <- base_diff1
base_diff_vrt <-  bind_rows(base_diff_vrt, base_diff2)
base_diff_vrt$dupes <- duplicated(base_diff_vrt$id_number)




kkkkk <- subset(base_diff_vrt_head, select=c(1,2,7))
kkkkk2 <- melt(kkkkk, id=c("spouse_link"))
kkkkk3 <- dcast(kkkkk, spouse_link ~ variable, value.var= "id_number", "street1")



R ~ C


omfg2 <- reshape(transform(base_diff_vrt_head, time=ave(spouse_link,spouse_link,FUN=seq_along)), 
  idvar="spouse_link", direction="wide", sep="")

omfg <- transform(base_diff_vrt_head, time=ave(spouse_link,spouse_link,FUN=seq_along))
