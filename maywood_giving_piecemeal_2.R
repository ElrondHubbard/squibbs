##      MAYWOOD GIVING BATMAN
##  MH needs to know how many acive allocation types have MAYWOOD as the agency
##  Something with Damen Society and how they get coded and treated
##  Needs to know all active allocations where agency = Maywood
##  $$ Amt given
##  Date of gift
##  FY of gift
##  Cares about FY 14 - Now

##      *** PROBLEM ***
##  Script below didn't produce what MH wanted...it didn't give full amounts
## of contributions, only partial, didn't provide $ amounts that jived with 
## what we saw elsewhere n knew were true. 
## Need to set it up such that ID #52336, his $7395 contribution of stock 
## will show up in both gift/pledge amount, and figure into annual totals.
## Right now it does not figure into annual totals. 

##  Query below is edited, takes big query breaks it into smaller pieces
##  see the 'Maywood_giving' script for the OG query

## install.packages("RODBC")
## install.packages("dplyr")
## install.packages("reshape")
## library(dplyr)
## library(RODBC)
## library(reshape)

## squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")
## rm(email_dupes_adv_client2)
## library(dplyr)
## Tunnel into ADVSCPRD and pull email dataset
## Is basically a copy of dupe email audit from ADV client, w more variables

## UPDATE
## Apparently joining FY14-now datasets requires like 1000 GB of memory
## Below is series of repeated actions by FY, 14 - now

FY14_gift_alloc_stub <- sqlQuery(squibbs, "(
  SELECT DISTINCT  
    g.GIFT_DONOR_ID as ID_NUMBER,
    g.GIFT_ASSOCIATED_ALLOCATION,
    g.GIFT_DATE_OF_RECORD as DATE_OF_RECORD, 
    g.GIFT_RECEIPT_NUMBER as RECEIPT_NUM, 
    g.GIFT_TRANSACTION_TYPE as TRANS_TYPE, 
    g.GIFT_ASSOCIATED_AMOUNT as LEGAL_AMT, 
    g.GIFT_ASSOCIATED_CREDIT_AMT as CREDIT_AMT, 
    g.GIFT_RECEIPT_DATE as RECEIPT_DATE, 
    g.GIFT_ASSOCIATED_CODE as ASSOCIATION, 
    g.GIFT_ACCOUNT as ACCOUNT,
    g.GIFT_APPEAL,
    g.GIFT_YEAR_OF_GIVING as FISCAL_YEAR,
    g.GIFT_PAYMENT_TYPE,
    al.SHORT_NAME as ALLOC_NAME, 
    al.ALLOC_SCHOOL,  
    al.AGENCY as DONOEE_AGENCY, 
    al.ALLOCATION_CODE as ALLOC_CODE,
    al.status_code,
    al.start_date AS alloc_date
  FROM 
    advance.gift g
  JOIN 
    advance.allocation al ON (al.allocation_code = g.gift_associated_allocation)
  WHERE
    g.gift_year_of_giving = '2014') ")

## Allocation data 
## Commented out b/c all these data are included in query above
#FY14_allocation_stub <- sqlQuery(squibbs, "(
#  SELECT DISTINCT
#    al.ALLOCATION_CODE, g.GIFT_ASSOCIATED_ALLOCATION, al.SHORT_NAME, al.AGENCY, 
#    al.status_code, al.start_date AS alloc_date 
#  FROM
#    advance.gift g, advance.allocation al
#  WHERE
#    g.GIFT_ASSOCIATED_ALLOCATION = al.ALLOCATION_CODE
#  AND 
#    g.gift_year_of_giving = '2014') ")

##  Gift data
## Commented out b/c data are included in query above
#FY14_gift_stub_distinct <- sqlQuery(squibbs, "(
#  SELECT DISTINCT
#    g.gift_donor_id, g.GIFT_ASSOCIATED_ALLOCATION, g.gift_appeal, g.gift_year_of_giving, 
#    g.gift_payment_type, g.gift_associated_credit_amt, al.allocation_code
#  FROM 
#    advance.gift g, advance.allocation al
#  WHERE
#    g.GIFT_ASSOCIATED_ALLOCATION = al.ALLOCATION_CODE 
#  AND 
#    g.gift_year_of_giving = '2014')  ")    

## Entity data
FY14_entity_stub <- sqlQuery(squibbs, "(
  SELECT DISTINCT
    e.id_number, g.gift_donor_id, e.first_name, e.last_name, e.record_type_code 
  FROM
    advance.gift g, advance.entity e 
  WHERE
    g.gift_donor_id = e.id_number
  AND 
    g.gift_year_of_giving = '2014') ") 

## Appeals data
FY14_appeals_stub <- sqlQuery(squibbs, "(
  SELECT DISTINCT
    ap.id_number, g.gift_donor_id, ap.appeal_code 
  FROM
    advance.gift g, advance.appeals ap
  WHERE
    g.gift_donor_id = ap.id_number
  AND 
    g.gift_year_of_giving = '2014') ")

## Pledge data
FY14_pledge_stub <- sqlQuery(squibbs, "(
  SELECT DISTINCT
    g.GIFT_ASSOCIATED_ALLOCATION, p.pledge_donor_id, p.pledge_allocation_name, 
    p.PLEDGE_PLEDGE_NUMBER, p.pledge_appeal, p.PLEDGE_PURPOSE, p.PLEDGE_ASSOCIATED_CODE,
    p.PLEDGE_AMOUNT, p.PLEDGE_ASSOCIATED_CREDIT_AMT
  FROM
    advance.gift g, advance.pledge p
  WHERE
    g.GIFT_ASSOCIATED_ALLOCATION = p.pledge_allocation_name   
  AND
    g.gift_year_of_giving = '2014') ")


rm(p1_gift_w_alloc)

##tmp_pledge_from_sql_big <- read.csv("N:/AIS/Yerik/Data Audits/Agency Codes Batman/pledge_stub_from_oracle_big.csv")
rm(tmp_appeals_stub, tmp_entity_stub)
##  Now join them
p1_gift_w_pledge <- left_join(FY14_gift_alloc_stub, FY14_pledge_stub, c("GIFT_ASSOCIATED_ALLOCATION" = "PLEDGE_ALLOCATION_NAME"), na_matches = "never")
##  Had issue - R doubled column gift_assoc_allOc_code, 2nd one is .y
##  Is factor type as is pledge_blah blah join p2 so use .y
p2_p1_w_pledge <- left_join(p1_gift_w_alloc, tmp_pledge_stub, c("ALLOC_CODE" = "PLEDGE_ALLOCATION_NAME"), na_matches = "never")

write.csv(FY14_gift_alloc_stub, file="N:/AIS/Yerik/Data Audits/damen_society/fy14_gift_alloc_stub.csv", na="", row.names=TRUE)
write.csv(FY14_entity_stub, file="N:/AIS/Yerik/Data Audits/damen_society/fy14_entity_stub.csv", na="", row.names=TRUE)
write.csv(FY14_appeals_stub, file="N:/AIS/Yerik/Data Audits/damen_society/fy14_appeals_stub.csv", na="", row.names=TRUE)
write.csv(FY14_pledge_stub, file="N:/AIS/Yerik/Data Audits/damen_society/fy14_pledge_stub.csv", na="", row.names=TRUE)

