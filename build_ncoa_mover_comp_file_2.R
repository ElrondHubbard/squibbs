# Script below takes last 5 NCOA updates
# May 2018, Jan 2018, Sept 2017, May 2017, Jan 2017
# Finds all those who moved, pulls dates of move, type code, etc
# So can compare against funny dates for new movers etc

# Look through ALL NCOA data and find all move date codes.
# Pull them out n get them in a list.

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

# May 2018
ncoa_may18 <- read.csv("N:/AIS/NCOA/Loyola_Addr_Cleaning_2018_05_11/Loyola Addr Cleaning 2018-05-11_Import.csv")
ncoa_may18$ncoa_dt <- "may18"
ncoa_may18$FIN_ADDR <- gsub('.','',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub('Avenue','Ave',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub('Court','Ct',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub('Drive','Dr',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub(',',' ',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub('#','',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub(' Road','Rd',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub('st','St',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- gsub('Street','St',ncoa_may18$FIN_ADDR, fixed = TRUE)
ncoa_may18$FIN_ADDR <- tolower(ncoa_may18$FIN_ADDR)
ncoa_may18$addr_is_update <- ifelse(ncoa_may18$NCO_ADDR=="",0,1)
ncoa_may18_movers <- subset(ncoa_may18, ncoa_may18$NCO_MOVEDT > 0, select=c(1,158,170:172,175,132,68,67,69,183:184))
colnames(ncoa_may18_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# January 2018
ncoa_jan18 <- read.csv("N:/AIS/NCOA/Loyola Addr Cleaning 2018-01-09_Appended.csv")
ncoa_jan18$ncoa_dt <- "jan18"
ncoa_jan18$FIN_ADDR <- gsub('.','',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub('Avenue','Ave',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub('Court','Ct',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub('Drive','Dr',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub(',',' ',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub('#','',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub(' Road','Rd',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub('st','St',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- gsub('Street','St',ncoa_jan18$FIN_ADDR, fixed = TRUE)
ncoa_jan18$FIN_ADDR <- tolower(ncoa_jan18$FIN_ADDR)
ncoa_jan18$addr_is_update <- ifelse(ncoa_jan18$NCO_ADDR=="",0,1)
ncoa_jan18_movers <- subset(ncoa_jan18, ncoa_jan18$NCO_MOVEDT > 0, select=c(1,158,170:172,175,132,68,67,69,183:184))
colnames(ncoa_jan18_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# Sept 2017
ncoa_sept17 <- read.csv("N:/AIS/NCOA/Loyola_Addr_cleaning_2017_9_22_Appended/Loyola_Addr_Cleaning_2017_09_22_Appended.csv")
ncoa_sept17$ncoa_dt <- "sept17"
ncoa_sept17$STREET1 <- as.character(ncoa_sept17$STREET1)
ncoa_sept17$STREET1 <- gsub('.','',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub('Avenue','Ave',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub('Court','Ct',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub('Drive','Dr',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub(',',' ',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub('#','',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub(' Road','Rd',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub('st','St',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- gsub('Street','St',ncoa_sept17$STREET1, fixed = TRUE)
ncoa_sept17$STREET1 <- tolower(ncoa_sept17$STREET1)
ncoa_sept17$Address_NCOA <- as.character(ncoa_sept17$Address_NCOA)
ncoa_sept17$Address_NCOA <- gsub('.','',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub('Avenue','Ave',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub('Court','Ct',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub('Drive','Dr',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub(',',' ',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub('#','',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub(' Road','Rd',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub('st','St',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- gsub('Street','St',ncoa_sept17$Address_NCOA, fixed = TRUE)
ncoa_sept17$Address_NCOA <- tolower(ncoa_sept17$Address_NCOA)
ncoa_sept17$addr_is_update <- ifelse(ncoa_sept17$Address_NCOA == ncoa_sept17$STREET1,0,1)
ncoa_sept17_movers <- subset(ncoa_sept17, ncoa_sept17$MoveDate_NCOA > 0, select=c(1,24:34))
colnames(ncoa_sept17_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# May 2017
ncoa_may17 <- read.csv("N:/AIS/NCOA/Loyola_Addr_cleaning_2017_05_31_Appended/Loyola_Addr_Cleaning_2017_05_31_Appended.csv")
ncoa_may17$ncoa_dt <- "may17"
ncoa_may17$STREET1 <- as.character(ncoa_may17$STREET1)
ncoa_may17$STREET1 <- gsub('.','',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub('Avenue','Ave',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub('Court','Ct',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub('Drive','Dr',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub(',',' ',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub('#','',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub(' Road','Rd',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub('st','St',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- gsub('Street','St',ncoa_may17$STREET1, fixed = TRUE)
ncoa_may17$STREET1 <- tolower(ncoa_may17$STREET1)
ncoa_may17$Address_NCOA <- as.character(ncoa_may17$Address_NCOA)
ncoa_may17$Address_NCOA <- gsub('.','',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub('Avenue','Ave',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub('Court','Ct',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub('Drive','Dr',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub(',',' ',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub('#','',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub(' Road','Rd',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub('st','St',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- gsub('Street','St',ncoa_may17$Address_NCOA, fixed = TRUE)
ncoa_may17$Address_NCOA <- tolower(ncoa_may17$Address_NCOA)
ncoa_may17$addr_is_update <- ifelse(ncoa_may17$Address_NCOA == ncoa_may17$STREET1,0,1)
ncoa_may17_movers <- subset(ncoa_may17, ncoa_may17$MoveDate_NCOA > 0, select=c(1,24:34))
colnames(ncoa_may17_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# Jan 2017
ncoa_jan17 <- read.csv("N:/AIS/NCOA/Loyola_Addr_cleaning_2017_01_23_Appended/Loyola_Addr_Cleaning_2017_01_23_Appended.csv")
ncoa_jan17$ncoa_dt <- "jan17"
ncoa_jan17$STREET1 <- as.character(ncoa_jan17$STREET1)
ncoa_jan17$STREET1 <- gsub('.','',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub('Avenue','Ave',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub('Court','Ct',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub('Drive','Dr',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub(',',' ',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub('#','',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub(' Road','Rd',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub('st','St',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- gsub('Street','St',ncoa_jan17$STREET1, fixed = TRUE)
ncoa_jan17$STREET1 <- tolower(ncoa_jan17$STREET1)
ncoa_jan17$Address_NCOA <- as.character(ncoa_jan17$Address_NCOA)
ncoa_jan17$Address_NCOA <- gsub('.','',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub('Avenue','Ave',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub('Court','Ct',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub('Drive','Dr',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub(',',' ',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub('#','',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub(' Road','Rd',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub('st','St',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- gsub('Street','St',ncoa_jan17$Address_NCOA, fixed = TRUE)
ncoa_jan17$Address_NCOA <- tolower(ncoa_jan17$Address_NCOA)
ncoa_jan17$addr_is_update <- ifelse(ncoa_jan17$Address_NCOA == ncoa_jan17$STREET1,0,1)
ncoa_jan17$addr_is_update <- ifelse(ncoa_jan17$Address_NCOA=="",1,0)
ncoa_jan17_movers <- subset(ncoa_jan17, ncoa_jan17$MoveDate_NCOA > 0, select=c(1,24:34))
colnames(ncoa_jan17_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# Feb 2015 - I think, the "one AIS did in 2015"
ncoa_feb15 <- read.csv("N:/AIS/Address_Cleaning/Addr_list_returned_from_NCOA_2015_2_23.csv")
ncoa_feb15$ncoa_dt <- "feb15"
ncoa_feb15$STREET1 <- as.character(ncoa_feb15$STREET1)
ncoa_feb15$STREET1 <- gsub('.','',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub('Avenue','Ave',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub('Court','Ct',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub('Drive','Dr',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub(',',' ',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub('#','',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub(' Road','Rd',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub('st','St',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- gsub('Street','St',ncoa_feb15$STREET1, fixed = TRUE)
ncoa_feb15$STREET1 <- tolower(ncoa_feb15$STREET1)
ncoa_feb15$Address_NCOA <- as.character(ncoa_feb15$Address_NCOA)
ncoa_feb15$Address_NCOA <- gsub('.','',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub('Avenue','Ave',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub('Court','Ct',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub('Drive','Dr',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub(',',' ',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub('#','',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub(' Road','Rd',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub('st','St',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- gsub('Street','St',ncoa_feb15$Address_NCOA, fixed = TRUE)
ncoa_feb15$Address_NCOA <- tolower(ncoa_feb15$Address_NCOA)
ncoa_feb15$addr_is_update <- ifelse(ncoa_feb15$Address_NCOA == ncoa_feb15$STREET1,0,1)
ncoa_feb15$ID <- substring(ncoa_feb15$ID,1,10)
ncoa_feb15$ID <- as.integer(ncoa_feb15$ID)
ncoa_feb15_movers <- subset(ncoa_feb15, ncoa_feb15$MoveDate_NCOA > 0, select=c(2,25:35))
colnames(ncoa_feb15_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

ncoa_may18_movers$ZIP <- as.character(ncoa_may18_movers$ZIP) 
ncoa_jan18_movers$ZIP <- as.character(ncoa_jan18_movers$ZIP) 
ncoa_sept17_movers$ZIP <- as.character(ncoa_sept17_movers$ZIP) 
ncoa_may17_movers$ZIP <- as.character(ncoa_may17_movers$ZIP) 
ncoa_jan17_movers$ZIP <- as.character(ncoa_jan17_movers$ZIP) 
ncoa_feb15_movers$ZIP <- as.character(ncoa_feb15_movers$ZIP) 

# Merge all separate NCOA mover data together
ncoa_data_feb15_may18 <-  bind_rows(ncoa_may18_movers, ncoa_jan18_movers, ncoa_sept17_movers, ncoa_may17_movers, ncoa_jan17_movers, ncoa_feb15_movers)

# Sort so order is ID asc , addr desc, nixie desc - addr doesn't matter but desc nixie means all A code will be on top - aka if address is 100% by NCOA, it will be on top 
ncoa_data_feb15_may18 <- ncoa_data_feb15_may18[order(-(ncoa_data_feb15_may18$ID), ncoa_data_feb15_may18$ADDR, ncoa_data_feb15_may18$NIXIE, decreasing=TRUE),]

# Subset keep only NIXIE w match A, drop secondary, or only missing zip + 4
nixie_list <- c("91", "92", "A", "19")

# Flag dupes based on ID and address
ncoa_data_feb15_may18$dupes <- duplicated(ncoa_data_feb15_may18[,1:2])

# Subset out all dupes that have NIXIE codes from keep list, keep move date n ID
ncoa_data_f15_m18_dupes <- subset(ncoa_data_feb15_may18, (ncoa_data_feb15_may18$dupes==TRUE & ncoa_data_feb15_may18$NIXIE %in% nixie_list), select=c(1,9,11,12))

# Sort them by ID, most recent move date first
ncoa_data_f15_m18_dupes <- ncoa_data_f15_m18_dupes[order(-(ncoa_data_f15_m18_dupes$ID), ncoa_data_f15_m18_dupes$MOVE_DT, decreasing=TRUE),]

# Flag dupes again, this time only by ID
ncoa_data_f15_m18_dupes$dupes <- duplicated(ncoa_data_f15_m18_dupes$ID)

# Subset dupe list again, remove all dupes. To refresh, the dupes in this list are duplicates of addresses in the main dataset. In this subset addr have been removed but if match along ID, it will map back to main dataset with same address as was originally attached to dates in this subset. Necessary b/c in some NCOA updates, got soft error messages about secondary data being removed, etc. Sorted main set such that A codes were kept, but is possible that there are newer dated addr than that w A code. If it was A code at some point, it's good. So by removing these dupes, merging date back into main dataset, comparing this date to date in main set and keeping most recent...we will have the most recent date for which ID# was associated with that address. 
ncoa_data_f15_m18_dupes <- subset(ncoa_data_f15_m18_dupes, ncoa_data_f15_m18_dupes$dupes==FALSE, select=c(1:4))

# Now..remove all dupes from main dataset, was sorted by ID/addr/A code. So address kept will be one that at is known by NCOA to be good, deliverable, etc.
ncoa_data_feb15_may18 <-subset(ncoa_data_feb15_may18, ncoa_data_feb15_may18$dupes ==FALSE, select=c(1:12))

# Join subset of dupe dates into main set of addresses, rename columns
ncoa_data_feb15_may18 <- left_join(ncoa_data_feb15_may18, ncoa_data_f15_m18_dupes, c("ID"="ID"))
colnames(ncoa_data_feb15_may18)[9:15] <- c("MOVE_DT1", "NIXIE1", "group1" , "addr_is_update1" ,"MOVE_DT2", "group2", "addr_is_update2")

# compare move date 2 if exists to move date 1, keep whichever is newest as 'most recent'
ncoa_data_feb15_may18$MOVE_DT2 <- ifelse(is.na(ncoa_data_feb15_may18$MOVE_DT2),0,ncoa_data_feb15_may18$MOVE_DT2)
ncoa_data_feb15_may18$addr_newest_dt <- ifelse(ncoa_data_feb15_may18$MOVE_DT1 > ncoa_data_feb15_may18$MOVE_DT2, ncoa_data_feb15_may18$MOVE_DT1,ncoa_data_feb15_may18$MOVE_DT2)
ncoa_data_feb15_may18$addr_newest_group <- ifelse(ncoa_data_feb15_may18$MOVE_DT1 > ncoa_data_feb15_may18$MOVE_DT2, ncoa_data_feb15_may18$group1, ncoa_data_feb15_may18$group2)


# Now have, for all NCOA updates Feb 15 to May 18, a complete dataset of ALL moves as indicated by NCOA move dates, where person moved from somewhere to an address that is deemed GOOD by NCOA. Most recent date field says that per NCOA guidelines n USPS...that date is most recent 'as of date' the person was known to be at that address. 

# Check dupes just by ID , dupes == 5660 / 48010 or ~ 10%
ncoa_data_feb15_may18$dupes <- duplicated(ncoa_data_feb15_may18$ID)
ncoa_data_feb15_may18 <- within(ncoa_data_feb15_may18, count <- ave(rep(1,nrow(ncoa_data_feb15_may18)),ncoa_data_feb15_may18$ID,FUN=cumsum))
# Dupe addr count 5/3 , 4/62, 3/689, 2/6859, 1/44319
# reshape
ncoa_data_feb15_may18_HRZ <- subset(ncoa_data_feb15_may18, select=c(1,2,16,17,19))
ncoa_data_feb15_may18_HRZ <- reshape(ncoa_data_feb15_may18_HRZ, direction = "wide", idvar = "ID", timevar="count")

# Remove all the extraneous datesets.
#rm(ncoa_data_f15_m18_dupes, ncoa_feb15, ncoa_feb15_movers, ncoa_jan17, ncoa_jan17_movers, ncoa_jan18, ncoa_jan18_movers, ncoa_may17, ncoa_may17_movers, ncoa_may18, ncoa_may18_movers, ncoa_sept17, ncoa_sept17_movers)

#################################################
####### Now the actual spouse mix up data #######
#################################################

diff_exp <- read.csv("N:/AIS/Yerik/Data Audits/spouse_joint_ind_diff_addr/spouses_with_same_home_check_mismatched.csv")
diff_exp$spouse_link <- 1:nrow(diff_exp)
colnames(diff_exp)[1:12] <- c(
  "id1", "id1_street", "id1_addr_type" ,"id1_operator" ,"id1_comment" ,"id1_mod_date" ,
  "id2", "id2_addr_type", "id2_street" ,"id2_operator" ,"id2_comment" ,"id2_mod_date" )
diff_exp$id1_street <- as.character(diff_exp$id1_street)
diff_exp$id2_street <- as.character(diff_exp$id2_street)
diff_exp$id1_street <- gsub('.','',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Avenue','Ave',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Court','Ct',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Drive','Dr',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub(',',' ',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('#','',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub(' Road','Rd',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('st','St',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Street','St',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- tolower(diff_exp$id1_street)
diff_exp$id2_street <- gsub('.','',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Avenue','Ave',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Court','Ct',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Drive','Dr',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub(',',' ',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('#','',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub(' Road','Rd',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('st','St',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Street','St',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- tolower(diff_exp$id2_street)
diff_exp <- subset(diff_exp, select=c(13,1:12))

# Fix awful add/mod dates
diff_exp$id1_mod_date2 <- as.character(substring(diff_exp$id1_mod_date,1,9))
diff_exp$id1_mod_date2 <- as.Date(diff_exp$id1_mod_date, format = "%d-%B-%y")
diff_exp$id2_mod_date2 <- as.character(substring(diff_exp$id2_mod_date,1,9))
diff_exp$id2_mod_date2 <- as.Date(diff_exp$id2_mod_date, format = "%d-%B-%y")

months <- data.frame(month=c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), num=sprintf('%0.2d',1:12))
diff_exp$id1_mod_date <- paste(substring(diff_exp$id1_mod_date,1,4), substring(diff_exp$id1_mod_date,6,7), substring(diff_exp$id1_mod_date,9,10),sep='')
diff_exp$id2_mod_date <- paste(substring(diff_exp$id2_mod_date,1,4), substring(diff_exp$id2_mod_date,6,7), substring(diff_exp$id2_mod_date,9,10),sep='')
diff_exp$id1_mod_date3 <- paste(substring(diff_exp$id1_mod_date2,1,4), substring(diff_exp$id1_mod_date2,6,7), substring(diff_exp$id1_mod_date2,9,10),sep='')
diff_exp$id2_mod_date3 <- paste(substring(diff_exp$id2_mod_date2,1,4), substring(diff_exp$id2_mod_date2,6,7), substring(diff_exp$id2_mod_date2,9,10),sep='')
diff_exp$id1_mod_date3 <- as.integer(diff_exp$id1_mod_date3)
diff_exp$id2_mod_date3 <- as.integer(diff_exp$id2_mod_date3)
diff_exp$id1_mod_date <- diff_exp$id1_mod_date3
diff_exp$id2_mod_date <- diff_exp$id2_mod_date3
diff_exp <- subset(diff_exp, select =(1:13))

###############################################
######  Now combine NCOA master w spouse ######
###############################################

# Match past NCOA against spouse data, diff_exp
spouse_diff_ncoa <- left_join(diff_exp, ncoa_data_feb15_may18_HRZ, c("id1" = "ID"))
spouse_diff_ncoa$id1_street <- trimws(spouse_diff_ncoa$id1_street, which = c("right"))

# Check see if any addr match id1 addr, if Y == addr, do same for date and group
spouse_diff_ncoa$id1_addr_match <- 
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.1), spouse_diff_ncoa$ADDR.1,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.2), spouse_diff_ncoa$ADDR.2,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.3), spouse_diff_ncoa$ADDR.3,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.4), spouse_diff_ncoa$ADDR.4,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.5), spouse_diff_ncoa$ADDR.5,    
  "NO MATCH")))))

spouse_diff_ncoa$id1_addr_date <- 
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.1), spouse_diff_ncoa$addr_newest_dt.1,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.2), spouse_diff_ncoa$addr_newest_dt.2,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.3), spouse_diff_ncoa$addr_newest_dt.3,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.4), spouse_diff_ncoa$addr_newest_dt.4,
  ifelse((spouse_diff_ncoa$id1_street==spouse_diff_ncoa$ADDR.5), spouse_diff_ncoa$addr_newest_dt.5,    "NO MATCH")))))

# Remove all ADDR1 - ADDR 5 data, do again for id2
spouse_diff_ncoa <- subset(spouse_diff_ncoa, select=c(1:14,31:32))

spouse_diff_ncoa <- left_join(diff_exp, ncoa_data_feb15_may18_HRZ, c("id2" = "ID"))
spouse_diff_ncoa$id2_street <- trimws(spouse_diff_ncoa$id2_street, which = c("right"))

# Check see if any addr match id1 addr, if Y == addr, do same for date and group
spouse_diff_ncoa$id2_addr_match <- 
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.1), spouse_diff_ncoa$ADDR.1,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.2), spouse_diff_ncoa$ADDR.2,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.3), spouse_diff_ncoa$ADDR.3,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.4), spouse_diff_ncoa$ADDR.4,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.5), spouse_diff_ncoa$ADDR.5,    
  "NO MATCH")))))

spouse_diff_ncoa$id2_addr_date <- 
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.1), spouse_diff_ncoa$addr_newest_dt.1,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.2), spouse_diff_ncoa$addr_newest_dt.2,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.3), spouse_diff_ncoa$addr_newest_dt.3,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.4), spouse_diff_ncoa$addr_newest_dt.4,
  ifelse((spouse_diff_ncoa$id2_street==spouse_diff_ncoa$ADDR.5), spouse_diff_ncoa$addr_newest_dt.5,    "NO MATCH")))))





ncoa_data_feb15_may18 %>%
  group_by(count) %>%
  summarise(number=n())

