
# >1 pref email audit resolution 
# MY YK took dataset of rec w >1 pref email type, determined 'best' one to use
# Have list of those, combine here to give more detail for TF to update

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

# Slightly mod'd SQL query
email_181015 <- sqlQuery(squibbs, "( SELECT
  em.ID_NUMBER ,em.XSEQUENCE ,em.EMAIL_TYPE_CODE as sq_type ,em.EMAIL_STATUS_CODE as        sq_status
  ,em.EMAIL_ADDRESS ,em.PREFERRED_IND as sq_pref ,em.STATUS_CHANGE_DATE ,em.XCOMMENT 
  ,em.START_DT ,em.STOP_DT ,em.DATE_ADDED ,em.DATE_MODIFIED
  ,em.IMODULES_IND ,em.NBR_BOUNCEBACKS ,em.OPERATOR_NAME 
  --,em.USER_GROUP ,em.LOCATION_ID ,em.EMAIL_CODE ,em.EMAIL_FORMAT_CODE 
  --,em.FORWARDS_TO_EMAIL_ADDRESS ,em.ORIGINAL_SOURCE_CODE ,em.CHANGE_SOURCE_CODE 
  --,em.CHANGE_SOURCE_DATE ,em.FAILED_REASON_CODE 
  FROM advance.email em )")

# Read in MY final dataset
dupe_emails <- read.csv("N:/AIS/Yerik/Data Audits/RECURRING_AUDITS/mult_pref_email_one_rec/multiple_emails_updated_MY.csv")
dupe_emails <- subset(dupe_emails, select=c(2:5))
dupe_emails$EMAIL_ADDRESS <- as.character(dupe_emails$EMAIL_ADDRESS)
email_181015$EMAIL_ADDRESS <- as.character(email_181015$EMAIL_ADDRESS)
email_181015$EMAIL_ADDRESS <- tolower(email_181015$EMAIL_ADDRESS)
dupe_emails$EMAIL_ADDRESS <- tolower(dupe_emails$EMAIL_ADDRESS)

dupe_emails <- left_join(dupe_emails, email_181015, c("ID_NUMBER" = "ID_NUMBER", "EMAIL_ADDRESS" = "EMAIL_ADDRESS"))
dupe_emails <- subset(dupe_emails, select=c(1,4,2:3,5:17))
dupe_emails$dupes <- duplicated(dupe_emails[,(1:3)])
dupe_emails <- subset(dupe_emails, dupe_emails$dupes==FALSE, select=c(1:17))
dupe_emails <- subset(dupe_emails, dupe_emails$final_EMAIL_TYPE_CODE != 'E')
dupe_emails$ST_ADD_DATE <- ifelse((dupe_emails$START_DT > 0), dupe_emails$START_DT, paste(substring(dupe_emails$DATE_ADDED,1,4), substring(dupe_emails$DATE_ADDED,6,7), substring(dupe_emails$DATE_ADDED,9,10),sep=''))
dupe_emails_final <- subset(dupe_emails, select = c(1:3,6:8,11,18))
dupe_emails_final$final_EMAIL_TYPE_CODE <- as.character(dupe_emails_final$final_EMAIL_TYPE_CODE)
dupe_emails_final$final_EMAIL_TYPE_CODE <- ifelse((dupe_emails_final$final_EMAIL_TYPE_CODE=='DEL'), 'DELETE', dupe_emails_final$final_EMAIL_TYPE_CODE)
dupe_emails_final$new_email_status_code <- dupe_emails_final$SQ_STATUS
dupe_emails_final$new_pref_ind <- 'N'
dupe_emails_final$new_start_dt <- dupe_emails_final$ST_ADD_DATE
colnames(dupe_emails_final)[3:7] <- c("new_email_type_code" , "cur_em_type" ,"cur_em_status", "curr_em_pref_ind","cur_start_dt")
dupe_emails_final <- subset(dupe_emails_final, select = c(1,2,4:8,3,9:11))

# end DF is ID, email, current type/status/pref/start, a master date var, then 'new' for each
# the 'new start date' is either start date if it existed, or date added if not

#write.csv(dupe_emails_final, file="N:/AIS/Yerik/Data Audits/RECURRING_AUDITS/mult_pref_email_one_rec/multiple_pref_emails_list_for_update.csv", na="", row.names=TRUE)



