## Do pull for MH, wants data on transactions and gifts and codes etc
## where gift type = in Honor of, Memory of, or Name of
## FY14 till now, w data below, names are changed to reflect Alfred names
## and are in order per what you'd see in Alfred

## install.packages("RODBC")
## library(RODBC)
## squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")

HMN_gift_trans_data_fy14_now <- sqlQuery(squibbs, "
  (
  SELECT  
    g.GIFT_DONOR_ID as ID_NUMBER, g.GIFT_DATE_OF_RECORD as DATE_OF_RECORD, 
    g.GIFT_RECEIPT_NUMBER as RECEIPT_NUM, g.GIFT_TRANSACTION_TYPE as TRANS_TYPE, 
    g.GIFT_ASSOCIATED_amount as LEGAL_AMT, g.GIFT_ASSOCIATED_CREDIT_AMT as CREDIT_AMT, 
    g.GIFT_RECEIPT_DATE as RECEIPT_DATE, al.ALLOCATION_CODE as ALLOC_CODE,
    g.GIFT_ASSOCIATED_CODE as ASSOCIATION, al.SHORT_NAME as ALLOC_NAME, al.ALLOC_SCHOOL,  
    al.AGENCY as DONOEE_AGENCY, g.GIFT_ACCOUNT as ACCOUNT
  FROM 
    advance.gift g
  JOIN 
    advance.allocation al ON (al.allocation_code = gift_associated_allocation)
  WHERE
    GIFT_ASSOCIATED_CODE in  ('H','I','M')
  AND 
    gift_year_of_giving > 2013) ")

## write.csv(HMN_gift_trans_data_fy14_now,'N:/AIS/Yerik/Data Requests/honor memory name gifts fy14 to now/hmn_gift_trans_data_fy14_now.csv', na="", row.names=TRUE)
## rm(HMN_gift_trans_data_fy14_now)

