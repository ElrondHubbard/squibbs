# Do these individually AF and AS b/c input addresses are different for both

install.packages(c("dplyr" , "reshape2", "tidyr", "tidyverse", "knitr", "purrr", "stringdist", "RODBC", "tools", "editData", "arsenal"))
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
library(arsenal)
squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")

# Check results from AL Sync vs Advance / BB 

t_alsync <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/vendor_test_files/FinalResults_AlumniSync_TEST_loyola/FINAL_loyola_combined_Address Date Example.csv", header=TRUE, na.strings="")
t_test_addr <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/vendor_test_files/vendor_test_file_all_obs_all_var.csv", header=TRUE, na.strings="")

# Keep only the updated data from alsync n alfind
t_alsync$final_zip <- paste(t_alsync$Final.Zip, t_alsync$Final.Zip...4,sep='-')
t_alsync2 <- subset(t_alsync, select=c(2:4,24:27,40,30:32,38:39))
colnames(t_alsync2)[4:13] <- c("addr_as", "addr2_as", "city_as", "state_as", "zip_as", "addr_date_as", "addr_result_as", "addr_src_as", "start_dt_as","asof_dt_as")

# Join original file sent to vendors, AS result, AF result
comp1 <- left_join(t_test_addr, t_alsync2, c("ID_NUMBER2" = "ID_NUMBER")) 
colnames(comp1)[2] <- c("test_id")
comp1$ADV_STREET <- as.character(comp1$ADV_STREET)
comp1$EDQ_STREET <- as.character(comp1$EDQ_STREET)
comp1$addr_as <- as.character(comp1$addr_as)
comp1$addr2_as <- as.character(comp1$addr2_as)
comp1$addr_as <- ifelse(is.na(comp1$addr2_as), comp1$addr_as, paste(comp1$addr_as, comp1$addr2_as, sep= ' '))

# Add addr type code for ADV addr
address_data <- sqlQuery(squibbs, "( SELECT DISTINCT 
  a.ID_NUMBER ,a.XSEQUENCE ,a.ADDR_TYPE_CODE ,a.ADDR_STATUS_CODE ,a.ADDR_PREF_IND
  ,a.STREET1 ,a.STREET2 ,a.STREET3 ,a.FOREIGN_CITYZIP ,a.CITY ,a.STATE_CODE ,a.ZIPCODE
  ,a.START_DT as ADDR_START_DT ,a.STOP_DT AS ADDR_STOP_DT ,a.XCOMMENT AS XCOMMENT_ADDR 
  ,a.DATE_ADDED AS ADDR_ADD_DT ,a.DATE_MODIFIED AS ADDR_MOD_DT ,a.OPERATOR_NAME AS ADDR_OPERATOR
  ,a.MAIL_RETURNED_NBR ,a.MAIL_RETURNED_DATE ,a.ADDRESS_MOD_DATE ,a.TRACER_SENT_DATE ,a.ADDR_VERIFIED_CODE 
  ,RANK() OVER (PARTITION BY a.id_number ORDER BY a.date_added DESC, rownum) AS rnk 
  FROM advance.address a  )")
address_181025 <- address_data
rm(address_data)
address_sm <- subset(address_181005, select = c(1,3:6))


# Subset DF to only street address columns, standardize them same way en masse
comp2 <- subset(comp1, select=c(3,7,12,18,24))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub(' Avenue',' Ave',comp2, fixed =  TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub( ' Court' , ' Ct' , comp2, fixed = TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub( ' Drive' , ' Dr' , comp2, fixed = TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub( ' Road' , ' Rd' ,comp2, fixed = TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub( ' Street' , ' St' ,comp2, fixed = TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub( '#' , '' ,comp2, fixed = TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub( '.' , '' ,comp2, fixed = TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) gsub( ',' , '' ,comp2, fixed = TRUE)))
comp2 <- as.data.frame(apply(comp2,2,function(comp2) tolower(comp2)))
comp2$ADV_STREET <- as.character(comp2$ADV_STREET)
comp2$EDQ_STREET <- as.character(comp2$EDQ_STREET)
comp2$addr_af <- as.character(comp2$addr_af)
comp2$addr_as <- as.character(comp2$addr_as)
comp2$ID_NUMBER <- as.integer(as.character(comp2$ID_NUMBER))

# subset comp1 to remove all of comp2, join comp2 addr var to comp, reorder so is in OG order
comp <- subset(comp1, select=c(1:6,8:11,13:17,19:23,25:33))
comp <- left_join(comp, comp2, c("ID_NUMBER" = "ID_NUMBER"))
comp <- subset(comp, select=c(1:6,30,7:10,31,11:15,32,16:20,33,21:29))
rm(comp1, comp2, alfind, alfind2, t_alsync, t_alsync2, t_test_addr)

# Now for each vendor (BB, AF, AS) run same and diff
# With ADV as base, what is same about each vendor and adv addr, n what is different
# For better or worse, use EDQ return as some type of metric

# ADV to EDQ
comp$adv_edq_same <- mapply(vintersect, strsplit(comp$ADV_STREET, split=" "), strsplit(comp$EDQ_STREET, split=" "), SIMPLIFY = TRUE)
comp$edq_adv_same <- mapply(vintersect, strsplit(comp$EDQ_STREET, split=" "), strsplit(comp$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp$adv_edq_same <- lapply(comp$adv_edq_same, paste, collapse = ' ')
comp$edq_adv_same <- lapply(comp$edq_adv_same, paste, collapse = ' ')
comp$adv_edq_same <- as.character(comp$adv_edq_same)
comp$edq_adv_same <- as.character(comp$edq_adv_same)
comp$adv_edq_diff <- mapply(vsetdiff, strsplit(comp$ADV_STREET, split=" "), strsplit(comp$EDQ_STREET, split=" "), SIMPLIFY = TRUE)
comp$edq_adv_diff <- mapply(vsetdiff, strsplit(comp$EDQ_STREET, split=" "), strsplit(comp$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp$adv_edq_diff <- lapply(comp$adv_edq_diff, paste, collapse = ' ')
comp$edq_adv_diff <- lapply(comp$edq_adv_diff, paste, collapse = ' ')
comp$adv_edq_diff <- as.character(comp$adv_edq_diff)
comp$edq_adv_diff <- as.character(comp$edq_adv_diff)
#comp$adv_edq_same <- ifelse(is.na(comp$EDQ_STREET),"NO_EDQ_DATA",comp$adv_edq_same)
#comp$edq_adv_same <- ifelse(is.na(comp$EDQ_STREET),"NO_EDQ_DATA",comp$edq_adv_same)
#comp$adv_edq_diff <- ifelse(is.na(comp$EDQ_STREET),"NO_EDQ_DATA",comp$adv_edq_diff)
#comp$edq_adv_diff <- ifelse(is.na(comp$EDQ_STREET),"NO_EDQ_DATA",comp$edq_adv_diff)

# ADV to AF
comp$addr_af <- ifelse((comp$addr_af == ''), NA, comp$addr_af)
comp$adv_af_same <- mapply(vintersect, strsplit(comp$ADV_STREET, split=" "), strsplit(comp$addr_af, split=" "), SIMPLIFY = TRUE)
comp$af_adv_same <- mapply(vintersect, strsplit(comp$addr_af, split=" "), strsplit(comp$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp$adv_af_same <- lapply(comp$adv_af_same, paste, collapse = ' ')
comp$af_adv_same <- lapply(comp$af_adv_same, paste, collapse = ' ')
comp$adv_af_same <- as.character(comp$adv_af_same)
comp$af_adv_same <- as.character(comp$af_adv_same)
comp$adv_af_diff <- mapply(vsetdiff, strsplit(comp$ADV_STREET, split=" "), strsplit(comp$addr_af, split=" "), SIMPLIFY = TRUE)
comp$af_adv_diff <- mapply(vsetdiff, strsplit(comp$addr_af, split=" "), strsplit(comp$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp$adv_af_diff <- lapply(comp$adv_af_diff, paste, collapse = ' ')
comp$af_adv_diff <- lapply(comp$af_adv_diff, paste, collapse = ' ')
comp$adv_af_diff <- as.character(comp$adv_af_diff)
comp$af_adv_diff <- as.character(comp$af_adv_diff)
comp$adv_af_same <- ifelse(is.na(comp$addr_af),"NO_AF_DATA",comp$adv_af_same)
comp$af_adv_same <- ifelse(is.na(comp$addr_af),"NO_AF_DATA",comp$af_adv_same)
comp$adv_af_diff <- ifelse(is.na(comp$addr_af),"NO_AF_DATA",comp$adv_af_diff)
comp$af_adv_diff <- ifelse(is.na(comp$addr_af),"NO_AF_DATA",comp$af_adv_diff)

# ADV to AS
comp$adv_as_same <- mapply(vintersect, strsplit(comp$ADV_STREET, split=" "), strsplit(comp$addr_as, split=" "), SIMPLIFY = TRUE)
comp$as_adv_same <- mapply(vintersect, strsplit(comp$addr_as, split=" "), strsplit(comp$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp$adv_as_same <- lapply(comp$adv_as_same, paste, collapse = ' ')
comp$as_adv_same <- lapply(comp$as_adv_same, paste, collapse = ' ')
comp$adv_as_same <- as.character(comp$adv_as_same)
comp$as_adv_same <- as.character(comp$as_adv_same)
comp$adv_as_diff <- mapply(vsetdiff, strsplit(comp$ADV_STREET, split=" "), strsplit(comp$addr_as, split=" "), SIMPLIFY = TRUE)
comp$as_adv_diff <- mapply(vsetdiff, strsplit(comp$addr_as, split=" "), strsplit(comp$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp$adv_as_diff <- lapply(comp$adv_as_diff, paste, collapse = ' ')
comp$as_adv_diff <- lapply(comp$as_adv_diff, paste, collapse = ' ')
comp$adv_as_diff <- as.character(comp$adv_as_diff)
comp$as_adv_diff <- as.character(comp$as_adv_diff)
comp$adv_as_same <- ifelse(is.na(comp$addr_result_as),"NO_AS_DATA",comp$adv_as_same)
comp$as_adv_same <- ifelse(is.na(comp$addr_result_as),"NO_AS_DATA",comp$as_adv_same)
comp$adv_as_diff <- ifelse(is.na(comp$addr_result_as),"NO_AS_DATA",comp$adv_as_diff)
comp$as_adv_diff <- ifelse(is.na(comp$addr_as),"NO_AS_DATA",comp$as_adv_diff)
comp$len_as <- nchar(comp$adv_edq_same, type="chars", allowNA=FALSE,keepNA=TRUE)

comp2 <- comp
comp <- subset(comp, select=c(1,17,7,11:12,16,24,30,32:37,42:46))


comp$group_as <- NA
comp$group_as [is.na(comp$group_as) & (comp$adv_as_same == "NO_AS_DATA")] <- 0
comp$group_as [is.na(comp$group_as) & (comp$Group != 6 & comp$Group != 2) & (comp$adv_edq_same == comp$adv_as_same)] <- 1
comp$group_as [is.na(comp$group_as) & comp$Group == 2 & (comp$adv_edq_same == comp$adv_as_same)] <- 2
comp$group_as [is.na(comp$group_as) & comp$Group == 4 & (comp$addr_result_as == "Deceased" | is.na(comp$addr_result_as))] <- 3
comp$group_as [is.na(comp$group_as) & comp$Group == 6 & (comp$adv_as_same == comp$as_adv_same) & nchar(comp$adv_as_same, type = "chars") > 9 ] <- 4
comp$group_as [is.na(comp$group_as) & comp$Group == 6 & (comp$adv_as_diff != comp$as_adv_diff)] <- 5
comp$group_as [is.na(comp$group_as) & (comp$adv_edq_same == comp$edq_adv_same) & (comp$adv_edq_same != comp$as_adv_diff)] <- 6

comp %>%
  group_by(group_as) %>%
  summarise(number=n())

comp$group_af <- NA
comp$group_af [is.na(comp$group_af) & (comp$adv_af_same == "NO_AF_DATA")] <- 0
comp$group_af [is.na(comp$group_af) & (comp$adv_edq_same == comp$adv_af_same)] <- 1
comp$group_af [is.na(comp$group_af) & (comp$adv_edq_same == comp$adv_af_same) & comp$group_af==2] <- 2
comp$group_af [is.na(comp$group_af) & (comp$addr_result_af == "Deceafed" | is.na(comp$addr_result_af)) & comp$group_af==4] <- 3
comp$group_af [is.na(comp$group_af) & (comp$adv_af_same == comp$af_adv_same) & nchar(comp$adv_af_same, type = "chars") > 9 & comp$group_af==6 ] <- 4
comp$group_af [is.na(comp$group_af) & (comp$adv_af_diff != comp$af_adv_diff) & comp$group_af==6 ] <- 5
comp$group_af [is.na(comp$group_af) & (comp$adv_edq_same == comp$edq_adv_same) & (comp$adv_edq_same != comp$af_adv_diff)] <- 6

comp %>%
  group_by(group_as) %>%
  summarise(number=n())


# ADV compared to AF subset
comp_adv_af <- subset(comp, select=c(1,17,3:5,7,11:12,16,18,22,34:41))

# ADV compared to AS subset
comp_adv_as <- subset(comp, select=c(1,17,3:5,7,11:12,16,24,30,32:37,42:45))

# Groups
# 0 = AS did not provide data back, aka cannot compare
# 1 = adv / edq / as data are essentially same, less secondary info
# 2 = as returned data to use that we already have and know to be old and inactive
# 3 = count of x/25 that we have listed as dead that AS did not return results or indicated was dead
# 4 = count of x/200 are lost where AS provided addr we do not have
# 5 = count of x/200 are lost where AS provided addr is same we have
# 6 = count of all others who aren't caught in 1-5 w adv == edq but as != adv/edq
comp_adv_as$group <- NA
comp_adv_as$group [is.na(comp_adv_as$group) & (comp_adv_as$adv_as_same == "NO_AS_DATA")] <- 0
comp_adv_as$group [is.na(comp_adv_as$group) & (comp_adv_as$adv_edq_same == comp_adv_as$adv_as_same)] <- 1
comp_adv_as$group [is.na(comp_adv_as$group) & (comp_adv_as$adv_edq_same == comp_adv_as$adv_as_same) & comp_adv_as$Group==2] <- 2
comp_adv_as$group [is.na(comp_adv_as$group) & (comp_adv_as$addr_result_as == "Deceased" | is.na(comp_adv_as$addr_result_as)) & comp_adv_as$Group==4] <- 3
comp_adv_as$group [is.na(comp_adv_as$group) & (comp_adv_as$adv_as_same == comp_adv_as$as_adv_same) & nchar(comp_adv_as$adv_as_same, type = "chars") > 9 & comp_adv_as$Group==6 ] <- 4
comp_adv_as$group [is.na(comp_adv_as$group) & (comp_adv_as$adv_as_diff != comp_adv_as$as_adv_diff) & comp_adv_as$Group==6 ] <- 5
comp_adv_as$group [is.na(comp_adv_as$group) & (comp_adv_as$adv_edq_same == comp_adv_as$edq_adv_same) & (comp_adv_as$adv_edq_same != comp_adv_as$as_adv_diff)] <- 6


comp_adv_as$date <- as.character(comp_adv_as$start_dt_as)
comp_adv_as$date <- as.Date(comp_adv_as$start_dt_as, format = "%m/%d/%Y")
comp_adv_as$date <- as.integer(as.character(paste(substring(comp_adv_as$date,1,4), substring(comp_adv_as$date,6,7), substring(comp_adv_as$date,9,10), sep='')))

comp_adv_as$group <- ifelse(is.na(comp_adv_as$group) & (comp_adv_as$adv_edq_same == comp_adv_as$edq_adv_same) & (comp_adv_as$adv_edq_same != comp_adv_as$as_adv_diff),6,NA)

comp_adv_as %>%
  group_by(group) %>%
  summarise(number=n())

comp_adv_as_NA <- subset(comp_adv_as, is.na(comp_adv_as$group), select = c(1,2,6:13,3:5,14:22))


comp2$share_last_name <- 0
comp2$share_last_name [(comp2$share_last_name1==1 | comp2$share_last_name2==1)] <- 1


# Use for missing addr update
comp_lil <- subset(comp, select=c(1,17,3:5,7,11:12,16,18,24:25,30:39))


# Remove anything populated by NA values being in one or both parts
comp_lil$adv_edq_diff <- 
  ifelse((is.na(comp_lil$EDQ_STREET)),"EDQ_NO_DATA",
    ifelse((comp_lil$ADV_STREET==comp_lil$EDQ_STREET),"MATCH",  comp_lil$adv_edq_diff))
comp_lil$adv_af_diff <- 
  ifelse((is.na(comp_lil$addr_af)),"AF_NO_DATA",
    ifelse((comp_lil$ADV_STREET==comp_lil$addr_af),"MATCH",  comp_lil$adv_af_diff))
comp_lil$adv_as_diff <- 
  ifelse((is.na(comp_lil$addr_as)),"AS_NO_DATA",
    ifelse((comp_lil$ADV_STREET==comp_lil$addr_as),"MATCH",  comp_lil$adv_as_diff))




comp_lil$len <- nchar(comp_lil$edq_adv_diff, type="chars")
comp_lil$adv_af_diff <- 
  ifelse((is.na(comp_lil$addr_af)),"AF_NO_DATA",
    ifelse((comp_lil$ADV_STREET==comp_lil$addr_af),"MATCH",  comp_lil$adv_af_diff))



comp_lil$af_adv_diff <- ifelse(is.na(comp_lil$addr_af),"AF_NO_DATA",comp_lil$af_adv_diff)










# Need to code in NA for all missing data in vendor addr 
# Make smaller DF with only street stuff.
comp2 <- subset(comp, select=c(1:5,7,11:12,16:18,24,32:33))
comp2$af_len <- nchar(comp2$addr_af, type="chars", allowNA=FALSE, keepNA=FALSE)
comp2$addr_af <- ifelse(comp2$af_len==0, NULL, comp2$addr_af)
comp2$addr_af[comp2$af_len==0] <- NA

comp$af_addr2 <- comp$addr_af
comp$af_addr2 <- ifelse((comp$af_addr2 == ''), NA, comp$af_addr2)


comp$length <- nchar(comp$addr_af, type="chars", allowNA=FALSE, keepNA=FALSE)

comp$af_addr2 <- comp$addr_af
comp$af_addr2 <- ifelse((comp$af_addr2 == ''), NA, comp$af_addr2)

comp$af_addr2 <- ifelse(mapply(nchar, comp$af_addr == 1)==TRUE, NA, comp$af_addr2)

comp$comp_adv_af <- mapply(vsetdiff, strsplit(comp$ADV_STREET, split=""), strsplit(comp$addr_AF, split=""), SIMPLIFY = TRUE)

eg_data$addr_comp2_1 <- mapply(vsetdiff, strsplit(eg_data$addr2, split=""), strsplit(eg_data$addr1, split=""), SIMPLIFY = TRUE)
eg_data$addr_comp2_2 <- lapply(eg_data$addr_comp2_1, paste, collapse = '')
eg_data$addr_comp2_3 <- as.character(eg_data$addr_comp2_2)


eg_data$addr_comp2_1 <- mapply(vsetdiff, strsplit(eg_data$addr2, split=""), strsplit(eg_data$addr1, split=""), SIMPLIFY = TRUE)

address_181005_stub <- subset(address_181005, select=c(1,6,13))
test_addr2 <- left_join(test_addr2, address_181005_stub, c("ID_NUMBER" = "ID_NUMBER", "ADV_STREET" = "STREET1"))
test_addr2$ADV_DATE <- test_addr2$ADDR_START_DT
test_addr2$dupes <- duplicated(test_addr2$ID_NUMBER2)
test_addr2 <- subset(test_addr2, test_addr2$dupes == FALSE, select=c(1:11))
test_addr <- subset(test_addr, test_addr$ID_NUMBER2<201)
test_addr <-  bind_rows(test_addr, test_addr2)
rm(test_addr2)

alsync2 <- subset(alsync, select=c(2,10:34))

adv_al_comp <- left_join(test_addr, alsync2, c("ID_NUMBER2"="ID_NUMBER"))


#write.csv(adv_al_comp, file="N:/AIS/Yerik/Data Audits/Address_et_al/adv_alsync_comp.csv", na="", row.names=TRUE)





