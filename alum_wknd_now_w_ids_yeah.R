## Find missing IDs or other details for 
## alum weekend 2017 ppl, some didn't do iModules
## so are missing. Take list of ppl by email addr
## and find ID and any other missing data

## install.packages("RODBC")
## install.packages("dplyr")
## library(RODBC)
## library(dplyr)
## squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")

## Big list of all emails and other stuff
tmp_big_email <- sqlQuery(squibbs, "
  (
  SELECT  
    em.id_number, em.email_address, LOWER(e.first_name) as first_name, 
    LOWER(e.last_name) as last_name 
  FROM 
    advance.email em
  JOIN 
    advance.entity e ON (e.id_number = em.id_number)
  JOIN
    advance.address ad ON (ad.id_number = em.id_number) )
 ")

## List of alum 2017 kids wo IDs
## Convert F and L name to all lower case for easier matching
email_y_id_n <- read.csv("N:/AIS/Yerik/Data Requests/Alumni_wknd_2017/Alumni Weekend 2017_IMP.csv", header = TRUE, sep =",")
levels(email_y_id_n$First_Name) <- tolower(levels(email_y_id_n$First_Name))
levels(email_y_id_n$Last_Name) <- tolower(levels(email_y_id_n$Last_Name))
levels(email_y_id_n$Email_Address) <- tolower(levels(email_y_id_n$Email_Address))

## Merge DF missing IDs to that with IDs and email etc
## Get flag for dupes
## filter out dupes
## remove dupe IDing variable
## outfile as csv file
alum_2017_now_w_IDs <- left_join(email_y_id_n, tmp_big_email, c("Email_Address" = "EMAIL_ADDRESS", "First_Name" = "FIRST_NAME", "Last_Name" = "LAST_NAME"))
alum_2017_now_w_IDs$dupes <- duplicated(alum_2017_now_w_IDs$Email_Address)
alum_2017_now_w_IDs_nd <- filter(alum_2017_now_w_IDs, dupes == "FALSE")
alum_2017_now_w_IDs_nd <- subset(alum_2017_now_w_IDs_nd, select = c(1:11))

##write.csv(alum_2017_now_w_IDs_nd, 'N:/AIS/Yerik/Data Requests/Alumni_wknd_2017/alum_2017_now_w_IDs.csv', na="", row.names=TRUE)
