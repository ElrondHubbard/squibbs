# Testing the manual updates and speeding up checking process

# Asked by MH to look at few hundred records b/c of giving, date of gift, etc
# Good number of these do not need address verified in detail
# Address we have is what BB suggested, it is newer than BB suggestion, etc

# Script takes this small subgroup - N = 431 and checks 

# Does BB addr match ADV
# Is ADV addr more current than BB
# Is BB partial match ADV vice versa (adding apt or STE number)
# Is BB suggestion an old, inactive address we have on file in ADV?
# (Should do last part for ALL auto updates n go back n inactive
# if needed but not now, let them run script to update)

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

# base file with advance address data by IDOpen file with addresses to update, subset out junk columns
# Break spouse data into two, spouse1 spouse2, recombine 1 dataset

base <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/spouse_w_diff_address.csv")
base$spouse_link <- 1:nrow(base)
spouse1_base <- subset(base, select=c(1:5,11))
colnames(spouse1_base)[1:5] <- c("spouse_id", "street1", "city", "state", "zip")
spouse2_base <- subset(base, select=c(6:11))
colnames(spouse2_base)[1:5] <- c("spouse_id", "street1", "city", "state", "zip")
spouse_base <- bind_rows(spouse1_base, spouse2_base)
warnings(spouse_base)

# Pull mega addr list from ADV - see if any BB addr is an old inactive addr in ADV
big_address_list <- sqlQuery(squibbs, "(
  SELECT DISTINCT 
  a.id_number ,a.street1 as adv_street ,a.zipcode AS adv_zip ,a.addr_status_code AS adv_addr_status ,a.addr_type_code 
  ,a.date_added AS adv_add_date ,a.date_modified AS adv_mod_date ,a.mail_returned_nbr ,a.start_dt AS adv_addr_start ,a.stop_dt AS adv_addr_stop
  ,RANK() OVER (PARTITION BY a.id_number ORDER BY a.date_added DESC, rownum) AS rnk 
  FROM 
  advance.address a
)")
big_address_list$ADV_STREET <- as.character(big_address_list$ADV_STREET)
big_address_list$ADV_STREET <- gsub('.','',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Avenue','Ave',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Court','Ct',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Drive','Dr',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub(',',' ',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Road','Rd',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('st','St',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Street','St',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- tolower(big_address_list$ADV_STREET)
spouse_base$street1 <- tolower(spouse_base$street1)

base2 <- subset(base2, select=c(2,4,45,46))

spouse_base2 <- left_join(spouse_base, base2, c("spouse_id" = "ID_NUMBER"))
spouse_base2 <- left_join(spouse_base2, big_address_list, c("spouse_id" = "ID_NUMBER"))


###############
###############
# Left off here, need to compare addresses to see how diff they actually are
# Actually give to MY$$ n have him, they are all # vs Unit vs Apt shit
###############
###############

# All below is code from edq manual group script

# Subset manual group == no addr match not dead, subset again, join huge addr list
manual_group2 <- subset(manual_group, manual_group$addr_match ==0 & manual_group$RECORD_STATUS_CODE == "A")
colnames(manual_group2)[17] <- c("adv_curr_add_date")
# Get date of all other ADV addr in comparable form
spouse_base2$adv_add_date <- paste(substring(spouse_base2$ADV_ADD_DATE,1,4),substring(manual_group2$ADV_ADD_DATE,6,7),substring(manual_group2$ADV_ADD_DATE,9,10),sep='')
manual_group2$adv_any_stop_date <- manual_group2$ADV_ADDR_STOP
manual_group2 <- manual_group2[,-c(8:11,18:19,24:26,33:34,36:38)]

# Do partial match fwd and back - IE does edq addr contain adv or does adv contain edq
# Catch when edq adds apt # to adv, and when adv has apt # edq does not etc
# Create 1 var that is pm either direction
manual_group2$curr_adv_active <- manual_group2$adv_current_addr
manual_group2$edq_sugg_addr <- manual_group2$edq_addr_return
manual_group2$any_adv_addr <- manual_group2$ADV_STREET
manual_group2$pm_adv_to_edq <- mapply(charmatch, manual_group2$any_adv_addr, manual_group2$edq_sugg_addr)
manual_group2$pm_edq_to_adv <- mapply(charmatch, manual_group2$edq_sugg_addr, manual_group2$any_adv_addr)
manual_group2$pm_act <- 0
manual_group2$pm_act [(manual_group2$pm_adv_to_edq==1 | manual_group2$pm_edq_to_adv==1) & manual_group2$ADV_ADDR_STATUS=="A"] <- 1
manual_group2$pm_inact <- 0
manual_group2$pm_inact [(manual_group2$pm_adv_to_edq==1 | manual_group2$pm_edq_to_adv==1) & manual_group2$ADV_ADDR_STATUS=="I"] <- 1
manual_group2 <- manual_group2[, -which(names(manual_group2) %in% c("pm_adv_to_edq","pm_edq_to_adv"))]

# Two partial match pm groups - any partial match, partial where match is inactive ADV addr
manual_group_pm1 <- subset(manual_group2, pm_act==1)
manual_group_pm1$dupes <- duplicated(manual_group_pm1$ID_NUMBER)
manual_group_pm1 <- subset(manual_group_pm1, dupes == FALSE, select=c(1,23,25:26,29:30))
colnames(manual_group_pm1)[2:6] <- c("pm_act_type", "pm_act_add","pm_act_stop", "pm_act", "pm_act_flag")
manual_group_pm2 <- subset(manual_group2, pm_inact==1)
manual_group_pm2$dupes <- duplicated(manual_group_pm2$ID_NUMBER)
manual_group_pm2 <- subset(manual_group_pm2, dupes == FALSE, select=c(1,23,25:26,29,31))
colnames(manual_group_pm2)[2:6] <- c("pm_inact_type", "pm_inact_add","pm_inact_stop", "pm_inact", "pm_inact_flag")

# Make copy dataset of no dupes manual group for final flags, already has dead + addr match
manual_group_final <- manual_group

#Bring both pm into final group
manual_group_final <- left_join(manual_group_final, manual_group_pm1, c("ID_NUMBER" = "ID_NUMBER"))
manual_group_final <- left_join(manual_group_final, manual_group_pm2, c("ID_NUMBER" = "ID_NUMBER"))
rm(manual_group_pm1, manual_group_pm2)

# Creates series of flags to YN lookup, creates lookup flag, orders based on priority
manual_group_final$gift_date <- paste(substring(manual_group_final$gift_date,1,4),substring(manual_group_final$gift_date,6,7),substring(manual_group_final$gift_date,9,10),sep='')
manual_group_final$gift_date <- as.integer(manual_group_final$gift_date)
manual_group_final$pm_act_flag <- ifelse(is.na(manual_group_final$pm_act_flag),0,1)
manual_group_final$pm_inact_flag <- ifelse(is.na(manual_group_final$pm_inact_flag),0,1)
manual_group_final$pm_act_flag <- as.integer(manual_group_final$pm_act_flag)
manual_group_final$pm_inact_flag <- as.integer(manual_group_final$pm_inact_flag)
manual_group_final$curr_adv_act <- manual_group_final$adv_current_addr
manual_group_final$edq_sugg_addr <- manual_group_final$edq_addr_return
manual_group_final$pm_act_addr <- manual_group_final$pm_act
manual_group_final$pm_inact_addr <- manual_group_final$pm_inact
manual_group_final <- manual_group_final[, -c(8:10,12,18:19,24:26)]
manual_group_final$curr_addr_post_mar1 <- 0
manual_group_final$curr_addr_post_mar1 [(manual_group_final$adv_add_date2 >= 20180301)]   <- 1
manual_group_final$gift_date_post_may1 <- 0
manual_group_final$gift_date_post_may1 [(manual_group_final$gift_date >= 20180501)]   <- 1

manual_group_final$lookup1 <- 1
manual_group_final$lookup1 [(manual_group_final$deceased==1 | manual_group_final$addr_match==1)] <- 0
manual_group_final$lookup1 [(manual_group_final$curr_addr_post_mar1==1 | manual_group_final$gift_date_post_may1==1)] <- 0

manual_group_final <- manual_group_final[order(manual_group_final$lookup, manual_group_final$ACTIVE_PROSPECT, manual_group_final$pm_inact_flag, -(manual_group_final$gift_date), decreasing=TRUE),]

manual_group_final %>%
  group_by(lookup1) %>%
  summarise(number=n())

#write.csv(spouse_base, file="N:/AIS/Yerik/Data Audits/Address_et_al/spouses_need_review1.csv", na="", row.names=TRUE)

