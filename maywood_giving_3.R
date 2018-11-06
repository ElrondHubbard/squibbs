#############################################
######    DAMEN SOCIETY WRONG LEVEL    ######
#############################################

## MH asked a very complicated question
## wants to know if a person is in DS, but
## is perhaps at the wrong level
## or isn't in DS but should be

## Due to bandaid script, not always counting
## all gifts it should be counting when calc'ing
## DS membership and levels within

## Need to go look for donations, legal and credit amt
## if they differ, and why, is possible they differ b/c
## of joint credit, is = on primary but != on 2nd'ary
## 
## Need
## gift data - amt, date, FY, name donor, associated donor if exist
## alloc data - alloc codes, agency, 
##
## Need at allocation and individual entity levels
##
## Do not count - 
## Maywood agency, planned gifts, pledges,
## totals under 500 (1000 is baseline for DS)
## 

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

## UPDATE 1
## Apparently joining FY14-now datasets requires like 1000 GB of memory
## Below is series of repeated actions by FY, 14 - now

## UPDATE 2
## TF in his glorious and infinite wisdom showed YK a 
## faster easier way to do this, it is below
## Pull donor id, legal amt, gift associated credit amt, sum by year
## If two numbers are =, cool, if not, problem

## UPDATE 3
## TF and YK sat down and did the whole thing inside SQL Developer 
## 
## RESULT
## 
## Nothing, no differences found, nothing found that'd imply DS level 
## needs to change, at all. Did this for INDV and ALLOC levels. 
##
## SCRIPTS
##
## Look at N drive, Yerik, damen_society_mar2018 - 
## ds fy14 to now pull 3 and pull ALLOC_ , both are .sql



legal_credit_comp <- sqlQuery(squibbs, "(
  SELECT DISTINCT   
    g.GIFT_DONOR_ID as ID_NUMBER,
    g.GIFT_ASSOCIATED_ALLOCATION,
    g.GIFT_DATE_OF_RECORD as DATE_OF_RECORD, 
    g.GIFT_RECEIPT_NUMBER as RECEIPT_NUM, 
    g.GIFT_TRANSACTION_TYPE as TRANS_TYPE,
    g.GIFT_ASSOCIATED_AMOUNT as legal_gift_amt,
    SUM(g.GIFT_ASSOCIATED_AMOUNT) OVER (PARTITION BY g.gift_donor_id, g.gift_year_of_giving) as sum_legal_by_fy_id, 
    g.GIFT_ASSOCIATED_CREDIT_AMT as credit_gift_amt,
    SUM(g.GIFT_ASSOCIATED_CREDIT_AMT) OVER (PARTITION BY g.gift_donor_id, gift_year_of_giving) as sum_credit_by_fy_id, 
    g.GIFT_RECEIPT_DATE as RECEIPT_DATE, 
    g.GIFT_ASSOCIATED_CODE as ASSOCIATION, 
    g.GIFT_ACCOUNT as ACCOUNT,
    g.GIFT_APPEAL,
    g.GIFT_YEAR_OF_GIVING as FISCAL_YEAR,
    g.GIFT_PAYMENT_TYPE,
    g.GIFT_SEQUENCE,
    al.ALLOCATION_CODE as ALLOC_CODE,
    al.SHORT_NAME as ALLOC_NAME, 
    al.ALLOC_SCHOOL,  
    al.AGENCY as DONOEE_AGENCY, 
    al.status_code, 
    al.start_date AS alloc_date
  FROM
    advance.gift g
  JOIN
    advance.allocation al ON (al.ALLOCATION_CODE = g.GIFT_ASSOCIATED_ALLOCATION)
  WHERE
    g.gift_year_of_giving > 2013

  )")

##  Solve for diff in INDV level - donor ID, and FY total level - receipt #
##  INDV ID - If legal != credit for in total, keep those IDs incl dupes
##  is probably b/c 2ndary credit, sure
##  use FY totals to determine IDs of interest
##  look at indiv gifts, Var1 = credit$ - legal$
##  look at case where sequence >1 or assoc code not P
##  find for IDx n FYx, is there gift where amt = diff for IDx FYx
credit_legal_no_match <- filter(legal_credit_comp, (LEGAL_GIFT_AMT != CREDIT_GIFT_AMT))
credit_legal_no_match <- credit_legal_no_match[order(credit_legal_no_match$RECEIPT_NUM, credit_legal_no_match$ID_NUMBER),]
credit_legal_no_match$diff <- (credit_legal_no_match$CREDIT_GIFT_AMT - credit_legal_no_match$LEGAL_GIFT_AMT)

## When FY legal sum != FY credit sum, and L or C amt are > 500
## When individual donations L != C, any amt
## sort both 
credit_legal_no_match_sum <- filter(legal_credit_comp, (SUM_LEGAL_BY_FY_ID != SUM_CREDIT_BY_FY_ID), (SUM_LEGAL_BY_FY_ID > 500 | SUM_CREDIT_BY_FY_ID > 500))
credit_legal_no_match <- filter(legal_credit_comp, (LEGAL_GIFT_AMT != CREDIT_GIFT_AMT))
## sort by receipt number and id number
credit_legal_no_match <- credit_legal_no_match[order(credit_legal_no_match$RECEIPT_NUM, credit_legal_no_match$ID_NUMBER),]

##  Get small set from master pull above for merge into no match set directly above
legal_credit_tmp <- subset(legal_credit_comp, select = c("ID_NUMBER", "RECEIPT_NUM", "DATE_OF_RECORD", "LEGAL_GIFT_AMT", "ASSOCIATION", "GIFT_SEQUENCE"))
colnames(legal_credit_tmp)[1:6] <- c("ID_NUMBER_2", "RECEIPT_NUM_2", "DATE_OF_RECORD_2", "LEGAL_GIFT_AMT_2", "ASSOCIATION_2", "GIFT_SEQUENCE_2")
credit_legal_noma_check <- left_join(credit_legal_no_match, legal_credit_tmp, c("RECEIPT_NUM" = "RECEIPT_NUM_2", "DATE_OF_RECORD" = "DATE_OF_RECORD_2", "diff" = "LEGAL_GIFT_AMT_2"))

## Have data in small set above...where legal $ != credit $ for a given FY
## Merge again with OG sql data - 
##  Need to see if can find a donor w same receipt # whose donation $
##  matches the difference observed in the credit legal no match 500 set

legal_credit_comp_tmp <- subset(legal_credit_comp, select = c('ID_NUMBER', 'RECEIPT_NUM', 'DATE_OF_RECORD', 'TRANS_TYPE', 'LEGAL_GIFT_AMT', 'CREDIT_GIFT_AMT', 'ASSOCIATION', 'GIFT_PAYMENT_TYPE', 'GIFT_APPEAL'))
colnames(legal_credit_comp_tmp)[1:9] <- c('ID_NUMBER_2', 'RECEIPT_NUM_2', 'DATE_OF_RECORD_2', 'TRANS_TYPE_2', 'LEGAL_GIFT_AMT_2', 'CREDIT_GIFT_AMT_2', 'ASSOCIATION_2', 'GIFT_PAYMENT_TYPE_2', 'GIFT_APPEAL_2')
credit_legal_noma_solve <- left_join(credit_legal_no_match, legal_credit_comp_tmp, c("RECEIPT_NUM" = "RECEIPT_NUM_2", "DATE_OF_RECORD" = "DATE_OF_RECORD_2", "CREDIT_GIFT_AMT" = "LEGAL_GIFT_AMT_2"), na_matches = "never")

## ID dupes in noma solve dataset
credit_legal_noma_solve$dupes <- duplicated(credit_legal_noma_solve[ c("DATE_OF_RECORD", "RECEIPT_NUM", "ID_NUMBER_2")])

credit_legal_noma_solve <- credit_legal_noma_solve[order(credit_legal_noma_solve$RECEIPT_NUM, credit_legal_noma_solve$dupes),]




duplicated(credit_legal_noma_solve[ c("DATE_OF_RECORD", "RECEIPT_NUM", "ID_NUMBER_2")])

check <- subset(credit_legal_noma_solve, select=c("DATE_OF_RECORD", "RECEIPT_NUM", "ID_NUMBER_2"))
check$dupes <- duplicated(check)

check$long <- paste(check$DATE_OF_RECORD, check$RECEIPT_NUM, check$ID_NUMBER_2, sep="")
check2 <- subset(check, select = c(4:5))

credit_legal_noma_solve$long <- paste(credit_legal_noma_solve$DATE_OF_RECORD, credit_legal_noma_solve$RECEIPT_NUM, credit_legal_noma_solve$ID_NUMBER_2, sep="")
credit_legal_noma_solve$dupes <- duplicated(credit_legal_noma_solve)

check3 <- left_join(credit_legal_noma_solve, check2, c("long" = "long"), )

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

