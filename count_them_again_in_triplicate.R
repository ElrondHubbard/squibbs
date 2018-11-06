# MH needs to know how records in dupe file we went through, ID'd YN, and resolved
# Open MA list of BB dupes, pull sql out to see RTC look for X purgable == resolved
# Need data on # examined too, combine B$, JC, YK, MY, GQ stubs

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
base_dupes <- read.csv("N:/AIS/Yerik/Data Audits/Duplicate Entities BB et al/SANDBOX_LUC_dupes_v1.csv")
bb_dupes <- subset(base_dupes, select=c(1:4,17,48:49))

# Can actually use this list from dead people count for dupe people count
big_dead_list <- sqlQuery(squibbs, "(
  SELECT DISTINCT 
  e.id_number ,e.first_name ,e.last_name ,e.record_type_code ,e.record_status_code 
  ,e.status_change_date ,e.operator_name  ,e.death_dt ,e.xcomment 
  FROM 
  advance.entity e   )")
big_dupe_list <- subset(big_dead_list, select=c(1,4:7,9))
bb_dupes2 <- left_join(bb_dupes, big_dupe_list, c("ID_NUMBER" = "ID_NUMBER"))

# List of LUC donors, flag gifts on both records YN
big_giving_list <- sqlQuery(squibbs, "(
  SELECT DISTINCT 
  e.id_number ,loyola.rsw_utilities.calculate_giving
  ('C','CASH','JOINT','ALL','ALL','LAK','ALL',e.id_number) 
  AS lifetime_luc_giving
  FROM advance.entity e   )")
big_giving_list$gift_flag <- ifelse(big_giving_list$LIFETIME_LUC_GIVING > 0,1,0)
big_giving_list <- subset(big_giving_list, big_giving_list$LIFETIME_LUC_GIVING > 0, select=c(1,3))

bb_dupes2 %>%
  group_by(RECORD_STATUS_CODE == "X") %>%
  summarise(number=n())

# Open most recent files from BW, JC, MYK
dupes_jc <- read.csv("N:/AIS/Yerik/Data Audits/Duplicate Entities BB et al/blackbaud_duplicate_file_v5_JC_asof180523.csv")
dupes_bw <- read.csv("N:/AIS/Yerik/Data Audits/Duplicate Entities BB et al/blackbaud_duplicate_file_v5_BW_asof_180607.csv")
dupes_myk <- read.csv("N:/AIS/Yerik/Data Audits/Duplicate Entities BB et al/blackbaud_duplicate_file_v5_myk.csv")
dupes_category <- subset(dupes_jc, select =c(1,56))
dupes_jc <- subset(dupes_jc, select = c(1,58:59))
colnames(dupes_jc)[2:3] <- c("jc_dupe","jc_result")
dupes_bw <- subset(dupes_bw, select = c(1,57:58))
colnames(dupes_bw)[2:3] <- c("bw_dupe","bw_result")
dupes_myk <- subset(dupes_myk, select = c(1,57:59))
colnames(dupes_myk)[2:4] <- c("myk_dupe","myk_result","myk_x")

bb_dupes3 <- left_join(bb_dupes2,dupes_jc,c("ID_NUMBER"="ID_NUMBER"))
bb_dupes3 <- left_join(bb_dupes3,dupes_bw,c("ID_NUMBER"="ID_NUMBER"))
bb_dupes3 <- left_join(bb_dupes3,dupes_myk,c("ID_NUMBER"="ID_NUMBER"))
bb_dupes3 <- left_join(bb_dupes3, dupes_category,c("ID_NUMBER"="ID_NUMBER"))
bb_dupes3 <- left_join(bb_dupes3, big_giving_list,c("ID_NUMBER"="ID_NUMBER"))

# Combine columns from each of 3 DF into one 
bb_dupes3$dupe_yn <- 0
bb_dupes3$dupe_yn [( bb_dupes3$bw_dupe==1)] <- 1
bb_dupes3$dupe_yn [( bb_dupes3$jc_dupe==1)] <- 1
bb_dupes3$dupe_yn [( bb_dupes3$myk_dupe=="1")] <- 1

bb_dupes3$myk_result <- as.character(bb_dupes3$myk_result)
bb_dupes3$bw_result <- as.character(bb_dupes3$bw_result)
bb_dupes3$jc_result <- as.character(bb_dupes3$jc_result)
bb_dupes3$XCOMMENT <- as.character(bb_dupes3$XCOMMENT)

# One column combines 'action taken' column from 3, for parsing counting
# Subset to only key var, make two more var 
# If one rec in group == D or X, mark both with flag ==1
bb_dupes3$result <- 
  ifelse(bb_dupes3$jc_result !='', bb_dupes3$jc_result,
    ifelse(bb_dupes3$bw_result !='',bb_dupes3$bw_result,
      ifelse(bb_dupes3$myk_result !='', bb_dupes3$myk_result,
        ifelse(mapply(charmatch,"Duplicate",bb_dupes3$XCOMMENT)==1, "Duplicate per xcomment",
          ifelse(mapply(charmatch,"duplicate",bb_dupes3$XCOMMENT)==1, "Duplicate per xcomment",
            '')))))
bb_dupes_final <- subset(bb_dupes3, select=c(1:11,20:23))
bb_dupes_final$rec_purged <- ifelse(ave((bb_dupes_final$RECORD_STATUS_CODE=="X") ,bb_dupes_final$BB_dupe_group_ID,FUN=any)==TRUE,1,0)
bb_dupes_final$rec_dead <- ifelse(ave((bb_dupes_final$RECORD_STATUS_CODE=="D"),bb_dupes_final$BB_dupe_group_ID,FUN=any)==TRUE,1,0)
bb_dupes_final$gifts_mult_rec <- ifelse(ave(bb_dupes_final$gift_flag==1,bb_dupes_final$BB_dupe_group_ID,FUN=sum)>1,1,0)   

rm(base_dupes, bb_dupes, bb_dupes2, bb_dupes3, big_dead_list, big_giving_list, big_dupe_list, dupes_bw, dupes_category, dupes_jc, dupes_myk)
#write.csv(bb_dupes_final, file="N:/AIS/Yerik/Data Audits/Duplicate Entities BB et al/bb_dupes_final_180906_2.csv", na="", row.names=TRUE)

