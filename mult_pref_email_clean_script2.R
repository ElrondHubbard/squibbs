
#################################
####### DATALOADER EMAIL  #######
#################################

# Dataloader is messing w email - pulling in @luc email 
# making it primary and l type, wo changing current primary 
# to type O - problem b/c doesnt change and often current primary
# is better than @luc anyway

# Need to ID cases where this has happened and fix them
# Pull email list where operator == LUAutoBatch aka DataLoader
# Dataloader is adding @luc email addresses where it shouldn't
# and is overwriting good, preferred email addresses

# Load libraries
install.packages("dplyr")
install.packages("reshape2")
install.pacakages("tidyr")
install.packages("tidyverse")
install.packages("knitr")
install.packages("purrr")
install.packages("stringdist")
install.packages("RODBC")

library(dplyr)
library(reshape2)
library(tidyr)
library(tidyverse)
library(knitr)
library(purrr)
library(stringdist)
library(RODBC)
squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")

# List of any all email address w operator == dataloader name
big_email_list <- sqlQuery(squibbs, "(
  SELECT *
  FROM advance.email em
  WHERE em.operator_name = 'LUAutoBatch' )")

# Query to find any records w >1 preferred email
gt1_pref_email <- sqlQuery(squibbs, "(
  SELECT 
  e.id_number, e.PREF_NAME_SORT, em.email_type_code, em.email_address, 
  em.email_status_code, em.xsequence, em.status_change_date, em.xcomment, 
  em.date_added, em.date_modified, em.start_dt, em.stop_dt
  FROM advance.email em 
  JOIN advance.entity e 
  ON 
    e.id_number = em.ID_NUMBER 
  WHERE em.id_number IN (
    SELECT id_number
    FROM advance.EMAIL
    WHERE email_type_code = 'E'
    AND email_status_code = 'A'
    GROUP BY ID_NUMBER
    HAVING count(id_number) > 1 ) 
  AND em.email_type_code = 'E' 
  AND em.email_status_code = 'A' 
  AND e.RECORD_STATUS_CODE = 'A' 
  AND e.PERSON_OR_ORG = 'P'
  )")

# Flag in big list - email address contain @luc
# Flag in other list - does email contain @luc n oper == AutoBitch

# Reformat date in big email list
big_email_list$add_year <- substring(big_email_list$DATE_ADDED,1,4)
big_email_list$add_month <- substring(big_email_list$DATE_ADDED,6,7)
big_email_list$add_day <- substring(big_email_list$DATE_ADDED,9,10)
big_email_list$DATE_ADDED <- paste(big_email_list$add_year, big_email_list$add_month, big_email_list$add_day,sep='')
big_email_list$DATE_ADDED <- as.integer(big_email_list$DATE_ADDED)
big_email_list$mod_year <- substring(big_email_list$DATE_MODIFIED,1,4)
big_email_list$mod_month <- substring(big_email_list$DATE_MODIFIED,6,7)
big_email_list$mod_day <- substring(big_email_list$DATE_MODIFIED,9,10)
big_email_list$DATE_MODIFIED <- paste(big_email_list$mod_year, big_email_list$mod_month, big_email_list$mod_day,sep='')
big_email_list$DATE_MODIFIED <- as.integer(big_email_list$DATE_MODIFIED)
# Remove pieces
big_email_list <- big_email_list[, -c(25:30)]
# Now subset big email list b/c only care about LUAutoBitch that came in on/after 3/1/18
dataloader_list <- subset(big_email_list, DATE_ADDED >= 20180301)


