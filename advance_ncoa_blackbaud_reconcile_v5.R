
# v5 adds INDV flags for types of Y/N criteria, then groups Y/N as well

# Script reconciles results from EDQ file with Advance address data
# Opens huge base ID Adv file, NCOA, EDQ, returned mailings law nurs
# Merges together, runs series of checks, removes all NCOA data, parses down varlist
# Creates file for all records with ERT L in Advance, outfile for update
# Goes into EDQ file results, compares to Advance
# Checks for dead, by date, by mail return, matched addr, addr dates Adv vs EDQ
# Makes use of Levenshtien distance calcs
# Makes large list of NO UPDATE and YES UPDATE
# See notes within script, there are many in detail of what is done

# As of 8/27/18 - final count for updates is 9674, could change if run again due to 
# sql pulled data etc. 

# Load libraries
install.packages("dplyr")
install.packages("reshape2")
install.pacakages("tidyr")
install.packages("tidyverse")
install.packages("knitr")
install.packages("purrr")
install.packages("stringdist")
install.packages("RODBC")
install.packages("tools")
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

## Base dataframe
advance_base <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/EDQ/work_bio_entity_address_ALL_180717.csv")
advance_base$base_flag <- 1

## NCOA results dataframe
ncoa_may2018 <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/NCOA/Loyola Addr Cleaning 2018-05-11_Import.csv")
ncoa_may2018$ncoa_flag <- 1
ncoa_may2018$dupes <- duplicated(ncoa_may2018$ID)
colnames(ncoa_may2018)[1] <- c("ID_NUMBER")
## ID dupes, make subset, remove dupes from main dataframe
ncoa_dupes <- filter(ncoa_may2018, dupes == TRUE)
ncoa_may2018 <- filter(ncoa_may2018, dupes == FALSE)
ncoa_may2018$dupes <- NULL
ncoa_dupes$dupes <- NULL

## BB edq results dataframe
edq_file <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/EDQ/EDQ_v3.csv")
edq_file$edq_flag <- 1
edq_file$flag_dead <- ifelse((edq_file$cattrcat_A == 'Deceased'),1,0)
edq_file$ImportID <- NULL

# Open returned mail as of July 18th data
returned_mail <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/returned_mail_june2018/combined_p14987_p15035_law_asof_180719.csv")
returned_mail$dupes <- duplicated(returned_mail$ID_NUMBER)
returned_mail <- filter(returned_mail, dupes == FALSE)
returned_mail$dupes <- NULL

# Merge 4 together
base <- advance_base
base_ncoa <- left_join(base, ncoa_may2018, c("ID_NUMBER" = "ID_NUMBER"))
base_ncoa_edq <- left_join(base_ncoa, edq_file, c("ID_NUMBER" = "ID_NUMBER"))
base_ncoa_edq_return <- left_join(base_ncoa_edq, returned_mail, c("ID_NUMBER" = "ID_NUMBER"))

# Delete other dataframes
rm(advance_base, base, base_ncoa, base_ncoa_edq, ncoa_dupes, returned_mail, edq_file, ncoa_may2018)

merged4 <- base_ncoa_edq_return
merged4 <- subset(merged4,select=c(1:7, 12,18,34,40,56,62,78,84,100,106,122,128,142, 324, 326:329, 333:334, 343:346, 351:352, 358:361))

# Do EDQ and NCOA addr lines have data
merged4$edq_addr <- ifelse(is.na(merged4$Addrlines_A),0,1)

# Recode to character data for comparison
merged4$PREF_STREET1 <- as.character(merged4$PREF_STREET1)
merged4$HOME_STREET1 <- as.character(merged4$HOME_STREET1)
merged4$BIZ_STREET1 <- as.character(merged4$BIZ_STREET1)
merged4$HOME_2ND_STREET1 <- as.character(merged4$HOME_2ND_STREET1)
merged4$BIZ_2ND_STREET1 <- as.character(merged4$BIZ_2ND_STREET1)
merged4$SSNL_STREET1 <- as.character(merged4$SSNL_STREET1)
merged4$Addrlines_A <- as.character(merged4$Addrlines_A)

# Standardize addresses quick, use gsub to find n replace St. with St, Avenue with Ave, etc
merged4$PREF_STREET1 <- gsub('.','',merged4$PREF_STREET1, fixed = TRUE)
merged4$PREF_STREET1 <- gsub('Avenue','Ave',merged4$PREF_STREET1, fixed = TRUE)
merged4$PREF_STREET1 <- gsub('Court','Ct',merged4$PREF_STREET1, fixed = TRUE)
merged4$PREF_STREET1 <- gsub('Drive','Dr',merged4$PREF_STREET1, fixed = TRUE)
merged4$PREF_STREET1 <- gsub(',','',merged4$PREF_STREET1, fixed = TRUE)
merged4$PREF_STREET1 <- gsub('Road','Rd',merged4$PREF_STREET1, fixed = TRUE)
merged4$PREF_STREET1 <- gsub('st','St',merged4$PREF_STREET1, fixed = TRUE)
merged4$PREF_STREET1 <- gsub('Street','St',merged4$PREF_STREET1, fixed = TRUE)
# Home
merged4$HOME_STREET1 <- gsub('.','',merged4$HOME_STREET1, fixed = TRUE)
merged4$HOME_STREET1 <- gsub('Avenue','Ave',merged4$HOME_STREET1, fixed = TRUE)
merged4$HOME_STREET1 <- gsub('Court','Ct',merged4$HOME_STREET1, fixed = TRUE)
merged4$HOME_STREET1 <- gsub('Drive','Dr',merged4$HOME_STREET1, fixed = TRUE)
merged4$HOME_STREET1 <- gsub(',','',merged4$HOME_STREET1, fixed = TRUE)
merged4$HOME_STREET1 <- gsub('Road','Rd',merged4$HOME_STREET1, fixed = TRUE)
merged4$HOME_STREET1 <- gsub('st','St',merged4$HOME_STREET1, fixed = TRUE)
merged4$HOME_STREET1 <- gsub('Street','St',merged4$HOME_STREET1, fixed = TRUE)
# Biz
merged4$BIZ_STREET1 <- gsub('.','',merged4$BIZ_STREET1, fixed = TRUE)
merged4$BIZ_STREET1 <- gsub('Avenue','Ave',merged4$BIZ_STREET1, fixed = TRUE)
merged4$BIZ_STREET1 <- gsub('Court','Ct',merged4$BIZ_STREET1, fixed = TRUE)
merged4$BIZ_STREET1 <- gsub('Drive','Dr',merged4$BIZ_STREET1, fixed = TRUE)
merged4$BIZ_STREET1 <- gsub(',',' ',merged4$BIZ_STREET1, fixed = TRUE)
merged4$BIZ_STREET1 <- gsub('Road','Rd',merged4$BIZ_STREET1, fixed = TRUE)
merged4$BIZ_STREET1 <- gsub('st','St',merged4$BIZ_STREET1, fixed = TRUE)
merged4$BIZ_STREET1 <- gsub('Street','St',merged4$BIZ_STREET1, fixed = TRUE)
# HOME 2
merged4$HOME_2ND_STREET1 <- gsub('.','',merged4$HOME_2ND_STREET1, fixed = TRUE)
merged4$HOME_2ND_STREET1 <- gsub('Avenue','Ave',merged4$HOME_2ND_STREET1, fixed = TRUE)
merged4$HOME_2ND_STREET1 <- gsub('Court','Ct',merged4$HOME_2ND_STREET1, fixed = TRUE)
merged4$HOME_2ND_STREET1 <- gsub('Drive','Dr',merged4$HOME_2ND_STREET1, fixed = TRUE)
merged4$HOME_2ND_STREET1 <- gsub(',','',merged4$HOME_2ND_STREET1, fixed = TRUE)
merged4$HOME_2ND_STREET1 <- gsub('Road','Rd',merged4$HOME_2ND_STREET1, fixed = TRUE)
merged4$HOME_2ND_STREET1 <- gsub('st','St',merged4$HOME_2ND_STREET1, fixed = TRUE)
merged4$HOME_2ND_STREET1 <- gsub('Street','St',merged4$HOME_2ND_STREET1, fixed = TRUE)
# Biz 2
merged4$BIZ_2ND_STREET1 <- gsub('.','',merged4$BIZ_2ND_STREET1, fixed = TRUE)
merged4$BIZ_2ND_STREET1 <- gsub('Avenue','Ave',merged4$BIZ_2ND_STREET1, fixed = TRUE)
merged4$BIZ_2ND_STREET1 <- gsub('Court','Ct',merged4$BIZ_2ND_STREET1, fixed = TRUE)
merged4$BIZ_2ND_STREET1 <- gsub('Drive','Dr',merged4$BIZ_2ND_STREET1, fixed = TRUE)
merged4$BIZ_2ND_STREET1 <- gsub(',','',merged4$BIZ_2ND_STREET1, fixed = TRUE)
merged4$BIZ_2ND_STREET1 <- gsub('Road','Rd',merged4$BIZ_2ND_STREET1, fixed = TRUE)
merged4$BIZ_2ND_STREET1 <- gsub('st','St',merged4$BIZ_2ND_STREET1, fixed = TRUE)
merged4$BIZ_2ND_STREET1 <- gsub('Street','St',merged4$BIZ_2ND_STREET1, fixed = TRUE)
# Seasonal (very few but count anyway)
merged4$SSNL_STREET1 <- gsub('.','',merged4$SSNL_STREET1, fixed = TRUE)
merged4$SSNL_STREET1 <- gsub('Avenue','Ave',merged4$SSNL_STREET1, fixed = TRUE)
merged4$SSNL_STREET1 <- gsub('Court','Ct',merged4$SSNL_STREET1, fixed = TRUE)
merged4$SSNL_STREET1 <- gsub('Drive','Dr',merged4$SSNL_STREET1, fixed = TRUE)
merged4$SSNL_STREET1 <- gsub(',','',merged4$SSNL_STREET1, fixed = TRUE)
merged4$SSNL_STREET1 <- gsub('Road','Rd',merged4$SSNL_STREET1, fixed = TRUE)
merged4$SSNL_STREET1 <- gsub('st','St',merged4$SSNL_STREET1, fixed = TRUE)
merged4$SSNL_STREET1 <- gsub('Street','St',merged4$SSNL_STREET1, fixed = TRUE)

# Make all address data lowercase for better comparison
merged4$PREF_STREET1 <- tolower(merged4$PREF_STREET1)
merged4$HOME_STREET1 <- tolower(merged4$HOME_STREET1)
merged4$BIZ_STREET1 <- tolower(merged4$BIZ_STREET1)
merged4$HOME_2ND_STREET1 <- tolower(merged4$HOME_2ND_STREET1)
merged4$BIZ_2ND_STREET1 <- tolower(merged4$BIZ_2ND_STREET1)
merged4$SSNL_STREET1 <- tolower(merged4$SSNL_STREET1)
merged4$Addrlines_A <- tolower(merged4$Addrlines_A)

# Flags - has ADV addr YN, ERT is lost YN, dead in ADV or Blackbaud YN
merged4$has_adv_addr <- ifelse((merged4$PREF_STREET1 =='')  & (merged4$HOME_STREET1 =='') & (merged4$BIZ_STREET1 =='')  & (merged4$HOME_2ND_STREET1 =='') & (merged4$BIZ_2ND_STREET1 =='')  & (merged4$SSNL_STREET1 =='' ),0,1)
merged4$lost_addr <- ifelse((merged4$RECORD_STATUS_CODE == 'L' & merged4$edq_addr == 1 & merged4$flag_dead == 0),1,0)
merged4$big_dead_flag <- ifelse((merged4$RECORD_STATUS_CODE == 'D' | merged4$flag_dead == 1),1,0)

# Does EDQ returned addr match any of 5 addr above
# If Y adds that addr to column, if N column is blank
merged4$adv_edq_match <- 
  ifelse((merged4$PREF_STREET1 == merged4$Addrlines_A),merged4$PREF_STREET1,
  ifelse((merged4$HOME_STREET1 == merged4$Addrlines_A),merged4$HOME_STREET1,
  ifelse((merged4$BIZ_STREET1 == merged4$Addrlines_A),merged4$BIZ_STREET1,
  ifelse((merged4$HOME_2ND_STREET1 == merged4$Addrlines_A),merged4$HOME_2ND_STREET1,
  ifelse((merged4$BIZ_2ND_STREET1 == merged4$Addrlines_A),merged4$BIZ_2ND_STREET1,
  ifelse((merged4$SSNL_STREET1 == merged4$Addrlines_A),merged4$SSNL_STREET1,
  ''))))))  

# ID type of Adv addr that matched EDQ, if match = biz2 but we have pref, do not update
merged4$match_addr_type <- 
  ifelse((merged4$adv_edq_match == merged4$PREF_STREET1) ,  "pref",
  ifelse((merged4$adv_edq_match == merged4$HOME_STREET1) , "home1" ,
  ifelse((merged4$adv_edq_match == merged4$BIZ_STREET1) , "biz1" , 
  ifelse((merged4$adv_edq_match == merged4$HOME_2ND_STREET1) , "home2" ,
  ifelse((merged4$adv_edq_match == merged4$BIZ_2ND_STREET1) , "biz2" , 
  ifelse((merged4$adv_edq_match == merged4$SSNL_STREET1) , "ssnl" ,
  ''))))))  

# Recode any without adv address to blank
merged4$match_addr_type [(merged4$has_adv_addr == 0)] <- '' 
merged4$match_addr_type [(merged4$adv_edq_match == '')] <- '' 

# Count for each type of addr that matched
merged4 %>%
  group_by(match_addr_type) %>%
  summarise(number=n())

# Easy see variable names and positions
# merged_var <- merged4[1:2,]

#####################################
#### GROUP 1 READY - LOST in ADV ####
#####################################

# Make small subset of all addr returned where Adv ERT == L
lost_for_update <- subset(merged4, lost_addr ==1)
# write.csv(lost_for_update, file="N:/AIS/Yerik/Data Audits/Address_et_al/lost_entities_addr_update_n1103_180727.csv", na="", row.names=TRUE)
# rm(lost_for_update)

#####################################
#### GROUP 2 - EDQ FILE RESULTS  ####
#####################################

# Smaller parsed EDQ dataset - RSC not Lost, alive per BB, in edq file
# Can still be dead per our records, but removed all BB dead, N = 14,731
edq_file_exp <- filter(merged4, RECORD_STATUS_CODE != 'L' & flag_dead == 0 & edq_flag == 1)

# Column that lists current Adv addr for each record 
edq_file_exp$adv_current_addr <- 
  ifelse(edq_file_exp$PREF_STREET1 != '',edq_file_exp$PREF_STREET1, 
  ifelse(edq_file_exp$HOME_STREET1 != '', edq_file_exp$HOME_STREET1,
  ifelse(edq_file_exp$BIZ_STREET1 != '', edq_file_exp$BIZ_STREET1,
  ifelse(edq_file_exp$HOME_2ND_STREET1 != '', edq_file_exp$HOME_2ND_STREET1, 
  ifelse(edq_file_exp$BIZ_2ND_STREET1 != '', edq_file_exp$BIZ_2ND_STREET1,
  ifelse(edq_file_exp$SSNL_STREET1 != '', edq_file_exp$SSNL_STREET1, 
  "NO ACTIVE ADV ADDR"))))))

# Column that lists type of each current Adv addr
edq_file_exp$adv_curr_addr_type <- 
  ifelse((edq_file_exp$adv_current_addr == edq_file_exp$PREF_STREET1),  "pref", 
  ifelse((edq_file_exp$adv_current_addr == edq_file_exp$HOME_STREET1), "home",
  ifelse((edq_file_exp$adv_current_addr == edq_file_exp$BIZ_STREET1), "biz1",
  ifelse((edq_file_exp$adv_current_addr == edq_file_exp$HOME_2ND_STREET1), "home2", 
  ifelse((edq_file_exp$adv_current_addr == edq_file_exp$BIZ_2ND_STREET1), "biz2",
  ifelse((edq_file_exp$adv_current_addr == edq_file_exp$SSNL_STREET1), "ssnl", 
  ''))))))

# Column is copy of EDQ returned addr so side by side for easy visual comparison
edq_file_exp$edq_addr_return <- edq_file_exp$Addrlines_A

# Calculate Lev Distance b/t MA adv column and BB return column
edq_file_exp$lev_distance <- mapply(adist, edq_file_exp$adv_current_addr, edq_file_exp$edq_addr_return)

# Categorical var for distance - full match, low distance, probable match, not probable
# Distance for each 1 = 0, 2 = less than 5, 3 = 5 to 10, 4 = < 10
# 1 = 100% match, 2 = minor change (cap, st dir), 3 = mostly apt add, 4 ofen 100% diff addr
edq_file_exp$lev_dist_grp <- 
  ifelse(edq_file_exp$adv_edq_match == edq_file_exp$edq_addr_return, "full match" ,
  ifelse(edq_file_exp$lev_distance <= 4, "< 5" , 
  ifelse(edq_file_exp$lev_distance <= 10, "5 to 10" , "> 10")))

# R charmatch checks for matching string of data within two cells 
# cell1 = 123 Main St 
# cell2 = 123 Main St Apt 1 
# Result = 1 b/c cell2 contains (among other things) 100% of cell1 data 
# Also copied BB field for description so if BB just appended apt # will state such
edq_file_exp$adv_addr_partial_edq <- mapply(charmatch, edq_file_exp$adv_current_addr, edq_file_exp$edq_addr_return)
edq_file_exp$adv_addr_partial_edq[is.na(edq_file_exp$adv_addr_partial_edq)] <- 0
edq_file_exp$addr_edit_type <- edq_file_exp$cattrdesc_A

# Subset n outfile lev group 2 & 3 MYK analyze case by case
edq_file_prolly_match <- subset(edq_file_exp, lev_dist_grp == '5 to 10' | lev_dist_grp == '< 5')
edq_file_prolly_match <- edq_file_prolly_match[order(edq_file_prolly_match$lev_distance_grp),]
edq_file_prolly_match <- arrange(edq_file_prolly_match, lev_dist_grp, lev_distance, -adv_addr_partial_edq)
#write.csv(edq_file_prolly_match, file="N:/AIS/Yerik/Data Audits/Address_et_al/adv_edq_addr_comp_lev23_180731.csv", na="", row.names=TRUE)

# Read back in csv list of all group 2 3 that YK n MY went through
edq_file_gr23_results <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/adv_edq_addr_comp_lev23_180731_DONE.csv")
edq_file_gr23_results <- subset(edq_file_gr23_results, select = c(2,54))

# Connect to SQL Squibbs TRN environs n pull out EVERY address for match date added etc.
big_address_list <- sqlQuery(squibbs, "(
  SELECT DISTINCT 
  a.id_number ,a.street1 as adv_street ,a.zipcode AS adv_zip ,a.addr_status_code AS adv_addr_status 
  ,a.date_added AS adv_add_date ,a.date_modified AS adv_mod_date ,a.mail_returned_nbr ,a.start_dt AS adv_addr_start ,a.stop_dt AS adv_addr_stop
  ,RANK() OVER (PARTITION BY a.id_number ORDER BY a.date_added DESC, rownum) AS rnk 
  FROM 
  advance.address a
)")
big_address_list$ADV_STREET <- as.character(big_address_list$ADV_STREET)

# Connect to SQL Squibbs TRN and pull out records that have XYZ we care about
# RE giving, date of last gift, active prospect rec YN
life_date_prosrec <- sqlQuery(squibbs, "(
  SELECT e.id_number
  ,loyola.rsw_utilities_4.get_last_gift_luc(e.id_number,'DATE_1') AS last_gift_date
  ,loyola.rsw_utilities.calculate_giving('C','CASH','JOINT','ALL','ALL','LAK','ALL',e.id_number) AS lifetime_luc_giving
  ,CASE WHEN EXISTS
  (SELECT pe.id_number FROM advance.prospect_entity pe 
    JOIN advance.prospect p ON pe.prospect_id = p.prospect_id
    WHERE pe.id_number = e.id_number AND pe.primary_ind = 'Y'
    AND p.active_ind = 'Y' AND p.PROSPECT_GROUP_CODE = 'LUC'
    ) THEN 'Yes' ELSE 'No' END AS active_prospect
  ,case when exists
  (select a.id_number from advance.address a where a.addr_type_code = 'H' and a.addr_status_code = 'A' and a.id_number = e.id_number) THEN 'Yes' ELSE 'No' END AS has_active_home_address
  FROM advance.entity e )")

# Change date to workable form for subset
life_date_prosrec$gift_date <- as.Date(life_date_prosrec$LAST_GIFT_DATE, format = "%m/%d/%y")
life_date_prosrec$manual <- ifelse((life_date_prosrec$LIFETIME_LUC_GIVING >=25000 | life_date_prosrec$ACTIVE_PROSPECT=="Yes" | life_date_prosrec$gift_date >= "2016-07-01"),1,0)
life_date_prosrec2 <- subset(life_date_prosrec, life_date_prosrec$manual == 1)

# standardize format for addr fields again like above
big_address_list$ADV_STREET <- gsub('.','',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Avenue','Ave',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Court','Ct',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Drive','Dr',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub(',',' ',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Road','Rd',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('st','St',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- gsub('Street','St',big_address_list$ADV_STREET, fixed = TRUE)
big_address_list$ADV_STREET <- tolower(big_address_list$ADV_STREET)

# Join results from MYK group 2 & 3 analysis with huge addr list, remove any dupes
edq_file_exp2 <- edq_file_exp
edq_file_exp2 <- left_join(edq_file_exp2, edq_file_gr23_results, c("ID_NUMBER" = "ID_NUMBER"))
edq_file_exp2 <- left_join(edq_file_exp2, big_address_list, c("ID_NUMBER" = "ID_NUMBER", "adv_current_addr" = "ADV_STREET"))
edq_file_exp2$dupes <- duplicated(edq_file_exp2$ID_NUMBER)
edq_file_exp2 <- subset(edq_file_exp2, dupes == FALSE)

# Extract address concat in form for easy compare see newer addr date EDQ or ADV
edq_file_exp2$edq_addr_yyyy <- substring(edq_file_exp2$cattrcom_A,7,10)
edq_file_exp2$edq_addr_mm <- substring(edq_file_exp2$cattrcom_A,1,2)
edq_file_exp2$edq_addr_dd <- substring(edq_file_exp2$cattrcom_A,4,5)
edq_file_exp2$edq_result_date <- paste(edq_file_exp2$edq_addr_yyyy, edq_file_exp2$edq_addr_mm, edq_file_exp2$edq_addr_dd, sep='')
edq_file_exp2$edq_result_date <- as.integer(edq_file_exp2$edq_result_date)
edq_file_exp2$adv_add_yyyy <- substring(edq_file_exp2$ADV_ADD_DATE,1,4)
edq_file_exp2$adv_add_mm <- substring(edq_file_exp2$ADV_ADD_DATE,6,7)
edq_file_exp2$adv_add_dd <- substring(edq_file_exp2$ADV_ADD_DATE,9,10)
edq_file_exp2$adv_add_date2 <- paste(edq_file_exp2$adv_add_yyyy, edq_file_exp2$adv_add_mm, edq_file_exp2$adv_add_dd, sep='')
edq_file_exp2$adv_add_date2 <- as.integer(edq_file_exp2$adv_add_date2)
edq_file_exp2 <- edq_file_exp2[,-c(61:63,65:67)]
edq_file_exp2$edq_date_newer <- ifelse(((edq_file_exp2$edq_result_date - edq_file_exp2$adv_add_date2) > 0),1,0)

####################################
#### INDV FLAGS FOR Y/N UPDATE #####
####################################

# The individual flags for EDQ updating
# Start at N = 14731 n whittle down
# Part I = no update group
# Flag those I know we do not update ==1 b/c:
# Record is dead per Advance
# ADV and EDQ addresses are perfect match
# Weird N=14 group - BB says Apt Append but regular address is Inactive
# Any addr w Adv add date after 03/01/2018 - is most recent date for all BB data aka anything in Adv later is newer than their newest
# MYK reviewed n found that 'correction' was BB adding county rd # as apt number WTF
# Record is marked as purgable in Adv
# One final flag that combines all the INDV flags, can filter as group but see logic for any single decision
edq_file_exp2$no_addr_update_dead <- 0 
edq_file_exp2$no_addr_update_dead [(edq_file_exp2$big_dead_flag == 1)] <- 1 
edq_file_exp2$no_addr_update_match <- 0 
edq_file_exp2$no_addr_update_match [(edq_file_exp2$lev_dist_grp == "full match")] <- 1 
edq_file_exp2$no_update_weird14 <- 0 
edq_file_exp2$no_update_weird14 [((edq_file_exp2$lev_dist_grp == "< 5" | edq_file_exp2$lev_dist_grp == "5 to 10") & edq_file_exp2$ADV_ADDR_STATUS == "I" & edq_file_exp2$adv_addr_partial_edq == 1)] <- 1
edq_file_exp2$no_update_date <- 0
edq_file_exp2$no_update_date [(edq_file_exp2$adv_add_date2 > 20180301 | edq_file_exp2$edq_date_newer==0)] <- 1
edq_file_exp2$no_update_myk_review <- 0
edq_file_exp2$no_update_myk_review [(edq_file_exp2$addr_OK == 0 & (edq_file_exp2$addr_edit_type == "Address Correction" | edq_file_exp2$addr_edit_type == "APT Append"))] <- 1
edq_file_exp2$no_update_purgable <- 0
edq_file_exp2$no_update_purgable [(edq_file_exp2$RECORD_STATUS_CODE == "X")] <- 1
edq_file_exp2$no_update_group <- 0
edq_file_exp2$no_update_group [(edq_file_exp2$no_addr_update_dead==1 | edq_file_exp2$no_addr_update_match==1 | edq_file_exp2$no_update_date==1 | edq_file_exp2$no_update_weird14==1 | edq_file_exp2$no_update_myk_review==1 | edq_file_exp2$no_update_purgable==1)] <- 1 

###############################
###### NOW IN REVERSE #########
###############################

# Left with 10,196 that could be updated
# Make INDV flags for YES update
# MYK reviewed n said yes update
# EDQ date is newer than ADV date
# no active address in ADV
# returned mail N > 0
# addr is currently inactive in adv

# Group of IDs that were checked by hand and determined to be added to update group 
manual_update_list <- c( "11287", "24278", "25596", "44449", "57842", "62623", 
  "70813", "75420", "75600", "106202", "114997", "119231", "123207", "126528", 
  "136114", "156827", "165631", "168908", "173082", "215068", "216071", "216501", 
  "231530", "294756", "317994", "322608", "335143", "347497", "370132", "387014", 
  "387061", "397808", "399656", "440570", "444886", "534858", "558264", "558457")

# Add flag for manual review per above life_dategive_blah SQL pull
edq_file_exp2 <- left_join(edq_file_exp2, life_date_prosrec2, c("ID_NUMBER" = "ID_NUMBER"))

edq_file_exp2$yes_update_myk_review <- 0
edq_file_exp2$yes_update_myk_review [(edq_file_exp2$addr_OK==1 )] <- 1 
edq_file_exp2$yes_update_edq_date_newer <- 0
edq_file_exp2$yes_update_edq_date_newer [(edq_file_exp2$edq_date_newer==1)] <- 1 
edq_file_exp2$yes_update_return_mail <- 0
edq_file_exp2$yes_update_return_mail [(edq_file_exp2$MAIL_RETURNED_NBR > 0)] <- 1 
edq_file_exp2$yes_update_no_adv_addr <- 0
edq_file_exp2$yes_update_no_adv_addr [(edq_file_exp2$adv_current_addr == "NO ACTIVE ADV ADDR")] <- 1 
edq_file_exp2$yes_update_adv_addr_inact <- 0
edq_file_exp2$yes_update_adv_addr_inact [(edq_file_exp2$ADV_ADDR_STATUS == "I")] <- 1 
edq_file_exp2$yes_update_manual_yes <- 0
edq_file_exp2$yes_update_manual_yes [(edq_file_exp2$ID_NUMBER %in% manual_update_list)] <- 1 

edq_file_exp2$yes_update_group <-0
edq_file_exp2$yes_update_group [(edq_file_exp2$yes_update_myk_review==1
  | edq_file_exp2$yes_update_edq_date_newer==1 | edq_file_exp2$yes_update_return_mail==1
  | edq_file_exp2$yes_update_no_adv_addr==1 | edq_file_exp2$yes_update_adv_addr_inact==1
  | edq_file_exp2$yes_update_manual_yes==1 )] <- 1

###############
# FINAL COUNT #
# no update ==0
# yes update ==1
# not manual lookup
###############

edq_file_exp2$final_update_flag <-0
edq_file_exp2$final_update_flag [( edq_file_exp2$yes_update_group == 1
  & edq_file_exp2$no_update_group == 0 & is.na(edq_file_exp2$manual) )] <- 1

edq_file_exp2$final_manual_flag <- 0
edq_file_exp2$final_manual_flag [(edq_file_exp2$final_update_flag==0
  & edq_file_exp2$no_update_group==0 & is.na(edq_file_exp2$manual_mh))
  | (edq_file_exp2$final_update_flag==0 & edq_file_exp2$no_update_group==1 
    & edq_file_exp2$manual_mh==1) ] <- 1

colnames(edq_file_exp2)[76] <- c("manual_mh")

edq_file_exp2 %>%
  group_by(final_update_flag) %>%
  summarise(number=n())
edq_file_exp2 %>%
  group_by(final_manual_flag) %>%
  summarise(number=n())

#write.csv(edq_file_exp2, file="N:/AIS/Yerik/Data Audits/Address_et_al/edq_update_yn_FINAL_180827.csv", na="", row.names=TRUE)



