# Pull list of PMS code people with note for LUConnect
# Got list of ppl to update, is N = 283 but see N = 357 people 
# can update, so wtf are there dupes or ppl not in DB or what
# Pull list of N = 357 dedupe I.N. 

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

pms_list <- sqlQuery(squibbs, "(
  SELECT DISTINCT i.id_number ,e.first_name ,e.last_name ,i.xsequence 
    ,i.interest_code ,i.start_dt ,i.stop_dt ,i.comment1 ,i.DATE_ADDED
    ,i.DATE_MODIFIED ,i.OPERATOR_NAME
  FROM 
  advance.interest i
  LEFT JOIN
  advance.entity e
  ON 
  e.id_number = i.id_number
  WHERE
  i.interest_code = 'PMS'
  AND
  i.comment2 = ' of LUConnect' 
  )")
pms_list$dupes <- duplicated(pms_list$ID_NUMBER)
pms_list_no_dupes <- subset(pms_list, dupes == FALSE)
# N = 41 dupes, so 357 - 41 = 316 - 283 = 33 records in this list not in XL sheet. 

# Load in XL data from Andrea Brault

luconnect_brault <- read.csv("N:/AIS/Yerik/Data Audits/mentor_code_quinlan/LUConnect_from_Jan_1_2015_to_May_18_2018_with Advance IDs_interest code edits_v2.csv")

# Change vartype, join
pms_list_no_dupes$ID <- as.factor(pms_list_no_dupes$ID_NUMBER)
merged_list <- full_join(pms_list_no_dupes, luconnect_brault, c("ID" = "ADVANCE_ID"))

# Parse out those that are in Advance but not in A Brault XL list
in_adv_not_brault$ADDED_DATE <- substring()
in_adv_not_brault <- subset(merged_list, select=c(1:3, 5:11), is.na(merged_list$ID_NUMBER.y))

# rename columns for easy viewing
colnames(in_adv_not_brault)[1:3] <- c("ID_NUMBER", "FIRST_NAME", "LAST_NAME")
colnames(in_adv_not_brault)[10] <- c("ADDED_BY")

# Outfile 
#write.csv(in_adv_not_brault, file="N:/AIS/Yerik/Data Audits/mentor_code_quinlan/in_advance_not_a_brault_list.csv", na="", row.names=TRUE)

