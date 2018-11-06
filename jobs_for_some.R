# Need to check on counts for employment updates - updated a lot of jobs Summer 2017
# Need to check # that were added, updated, or verified
# Not sure how to do #3, hopefully will see diff b/t add date and mod date for some
# Pull list of all employment table, get all update data, mash together, try n count

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
job_group2 <- read.csv("N:/AIS/Yerik/Data Audits/employment_updates/2017-08-09 Intellect Space GROUP 2.csv")
job_group3 <- read.csv("N:/AIS/Yerik/Data Audits/employment_updates/2017-08-09 Intellect Space GROUP 3.csv")

big_job_list <- sqlQuery(squibbs, "(
  SELECT e.* 
  FROM advance.employment e )")

# Count variable for big_job_list IE how many x does each ID appear
big_job_list <- big_job_list[order(big_job_list$ID_NUMBER , big_job_list$DATE_ADDED, big_job_list$DATE_MODIFIED),]
big_job_list <- within(big_job_list, count <- ave(rep(1,nrow(big_job_list)),big_job_list$ID_NUMBER,FUN=cumsum)) 
# Subset and fix date so can compare
job_list <- subset(big_job_list, select=c(1,8:10,25:27,37))
job_list$DATE_ADDED <- paste(substring(job_list$DATE_ADDED,1,4),substring(job_list$DATE_ADDED,6,7),substring(job_list$DATE_ADDED,9,10),sep='')
job_list$DATE_MODIFIED <- paste(substring(job_list$DATE_MODIFIED,1,4),substring(job_list$DATE_MODIFIED,6,7),substring(job_list$DATE_MODIFIED,9,10),sep='')
job_list$DATE_ADDED <- as.integer(job_list$DATE_ADDED)
job_list$DATE_MODIFIED <- as.integer(job_list$DATE_MODIFIED)

# Group 3 already had >=1 entry, group 2 had none
# Group 2 count all, might have added another from linked in if found it
# Group 3 count all...if only have one, assume == update to existing,
# if have > 1, assume added one to to employment record, check date quickly

job_group2 <- left_join(job_group2, job_list, c("ID_adv" = "ID_NUMBER"))
job_group3 <- left_join(job_group3, job_list, c("ID_adv" = "ID_NUMBER"))

# For some reason have a few that were not added to ADV but should have been
# Most are self employed owner etc, w no other data, probably why
# Need to add them tho, they are here job_group2_m N = 21
# Group 2 = 21
job_group2_m <- job_group2
job_group2_m$dupes <- duplicated(job_group2_m$ID_adv)
job_group2_m <-subset(job_group2_m, dupes == FALSE & is.na(DATE_ADDED), select=c(1:35))

# Group 3 =  114
job_group3_m <- job_group3
job_group3_m$dupes <- duplicated(job_group3_m$ID_adv)
job_group3_m <-subset(job_group3_m, dupes == FALSE & is.na(DATE_ADDED), select=c(1:35))

# For Groups 2 & 3 - counts 
# Add - were added 7/1/2017 - 10/1/2017
# Update - modified 7/1/2017 - 10/1/2017
# Verify - were in original file but neither added or modded in time window

# Group 2
job_group2 <- job_group2[order(job_group2$ID_adv , job_group2$DATE_ADDED, job_group2$DATE_MODIFIED),]
job_group2 <- subset(job_group2, DATE_ADDED > 0)
job_group2 <- within(job_group2, count <- ave(rep(1,nrow(job_group2)),job_group2$ID_adv,FUN=cumsum)) 
job_group2$added <- 0
job_group2$added [( job_group2$DATE_ADDED > 20170630 & job_group2$DATE_ADDED < 20171002)] <- 1
job_group2$update <- 0
job_group2$update [( job_group2$DATE_MODIFIED > 20170630 & job_group2$DATE_MODIFIED < 20171002 & job_group2$added==0)] <- 1
job_group2$verify <- 0
job_group2$verify [(job_group2$added==0 & job_group2$update==0)] <- 1
job_group2 <- job_group2[order(job_group2$ID_adv , job_group2$added, job_group2$update, job_group2$verify, decreasing = TRUE),]


# Group 2 Add = 1395, Mod = 244, Verify = 

# Group 3 - 
job_group3 <- job_group3[order(job_group3$ID_adv , job_group3$DATE_ADDED, job_group3$DATE_MODIFIED),]
job_group3 <- within(job_group3, count <- ave(rep(1,nrow(job_group3)),job_group3$ID_adv,FUN=cumsum)) 
job_group3 <- subset(job_group3, DATE_ADDED > 0)
job_group3$added <- 0
job_group3$added [( job_group3$DATE_ADDED > 20170630 & job_group3$DATE_ADDED < 20171002)] <- 1
job_group3$update <- 0
job_group3$update [( job_group3$DATE_MODIFIED > 20170630 & job_group3$DATE_MODIFIED < 20171002 & job_group3$added==0)] <- 1
job_group3$verify <- 0
job_group3$verify [(job_group3$added==0 & job_group3$update==0)] <- 1
job_group3 <- job_group3[order(job_group3$ID_adv , job_group3$added, job_group3$update, job_group3$verify, decreasing = TRUE),]

job_group2_d <- job_group2
job_group2_d$dupes <- duplicated(job_group2$ID_adv)
job_group2_d <- subset(job_group2_d, dupes == FALSE)

job_group3_d <- job_group3
job_group3_d$dupes <- duplicated(job_group3$ID_adv)
job_group3_d <- subset(job_group3_d, dupes == FALSE)


# Group 3 Add = 640, Mod = 734, Verify = 

job_group2_d %>%
  group_by(added) %>%
  summarise(number=n())
job_group2_d %>%
  group_by(update) %>%
  summarise(number=n())
job_group2_d %>%
  group_by(verify) %>%
  summarise(number=n())

job_group3_d %>%
  group_by(added) %>%
  summarise(number=n())
job_group3_d %>%
  group_by(update) %>%
  summarise(number=n())
job_group3_d %>%
  group_by(verify) %>%
  summarise(number=n())

rm(big_job_list, job_list, job_group2, job_group2_m, job_group3, job_group3_m)

