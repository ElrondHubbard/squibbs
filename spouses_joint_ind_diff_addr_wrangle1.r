# There are over 1000 records that are spouses with Joint INDTesting
# aka they want to get mail at the same place, but their addr are differenTesting
# Need to try and determine how this happened, source, and then, which addr is better

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

# data came, of course, as wide file, >1 ID per row. why people do this i will never
# understand. anyway, open csv, create link variable, break into two datasets, combine 
# back into one. yah. 

base_diff <- read.csv("N:/AIS/Yerik/Data Audits/spouse_joint_ind_diff_addr/spouses_with_same_home_check_mismatched.csv")
base_diff$spouse_link <- 1:nrow(base_diff)
base_diff1 <- subset(base_diff, select=c(1:6,13))
colnames(base_diff1)[1:6] <- c("id_number", "street1", "addr_type", "operator", "comment", "modified")
base_diff2 <- subset(base_diff, select=c(7:13))
colnames(base_diff2)[1:6] <- c("id_number", "addr_type", "street1","operator", "comment", "modified")
base_diff_vrt <- base_diff1
base_diff_vrt <-  bind_rows(base_diff_vrt, base_diff2)
base_diff_vrt$dupes <- duplicated(base_diff_vrt$id_number)

# Sort them to get pairs then subsample to play
base_diff_vrt <- base_diff_vrt[order(base_diff_vrt$spouse_link),]
base_diff_vrt_head <- base_diff_vrt[1:50,]

# big address query we know and love
big_address_list <- sqlQuery(squibbs, "( SELECT DISTINCT 
  a.id_number ,a.street1 as adv_street ,a.zipcode AS adv_zip ,a.addr_status_code AS adv_addr_status ,a.addr_type_code 
  ,a.date_added AS adv_add_date ,a.date_modified AS adv_mod_date ,a.mail_returned_nbr ,a.start_dt AS adv_addr_start ,a.stop_dt AS adv_addr_stop
  ,RANK() OVER (PARTITION BY a.id_number ORDER BY a.date_added DESC, rownum) AS rnk 
  FROM advance.address a  )")

# Theory is that for a lot of these, the is probably an address match b/t
# active for one and past for the other...for some reason, one got changed 
# but not other, like NCOA said change for one so was changed but only one
# of two did it. If can find that, then we go nuts n just update.

base_diff_vrt_head <- left_join(base_diff_vrt_head, big_address_list, c("id_number" = "ID_NUMBER"))
base_diff_vrt_head <- base_diff_vrt_head[,c(7,1,9,11,13,14,16)]
base_diff_vrt_head$ADV_ADD_DATE <- paste(
  substring(base_diff_vrt_head$ADV_ADD_DATE,1,4),
  substring(base_diff_vrt_head$ADV_ADD_DATE,6,7),
  substring(base_diff_vrt_head$ADV_ADD_DATE,9,10),sep='')
base_diff_vrt_head$ADV_MOD_DATE <- paste(
  substring(base_diff_vrt_head$ADV_MOD_DATE,1,4),
  substring(base_diff_vrt_head$ADV_MOD_DATE,6,7),
  substring(base_diff_vrt_head$ADV_MOD_DATE,9,10),sep='')
base_diff_vrt_head$dupes <- duplicated(base_diff_vrt_head[,1:4])

address2 <- subset(big_address_list, select=c(1,2,4))
address2$dupes <- duplicated(address2[,1:3])
address2 <- subset(address2, dupes==FALSE, select=c(1:3))


base_diff_vrt_head2 <- subset(base_diff_vrt_head, select=c(7,1:3))
base_diff_vrt_head2$dupes <- duplicated(base_diff_vrt_head$street1)










test <- base_diff_vrt_head
test <- subset(test, dupes==FALSE)
test <- subset(test, select=c(1:4))
test <- melt(test, id=c("spouse_link", "id_number", 'ADV_STREET', 'ADV_ADDR_STATUS'))





base_diff_wide <- melt(base_diff_vrt_head, id=c("spouse_link", "id_number"))

base_diff_vrt_head <- within(base_diff_vrt_head, count <- ave(rep(1,nrow(base_diff_vrt_head)),base_diff_vrt_head$id_number,FUN=cumsum))
base_diff_vrt_head2 <- subset(base_diff_vrt_head, select=c(1:3,8))
base_diff_wide <- reshape(base_diff_vrt_head2, direction = "wide", idvar="spouse_link", timevar = "count")



rm(base_diff_wide)

# Subset of big list, only active, only home
small_addr_list <- subset(big_address_list, big_address_list$ADV_ADDR_STATUS=='A' & big_address_list$ADDR_TYPE_CODE=='H')

# Join them together
base_diff_vrt <- left_join(base_diff_vrt, small_addr_list, c("id_number"="ID_NUMBER"))

# And of fucking course there are dupes, WHY WHY WHY WHY
# ID them but leave them in for now, diff problem needs address too
base_diff_vrt$dupes <- duplicated(base_diff_vrt$id_number)
#base_diff_vrt <- subset(base_diff_vrt, base_diff_vrt$dupes == FALSE)





# Make fucking VRT -> vrt work 
# Also rename vrt to VRT like it should be god











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

