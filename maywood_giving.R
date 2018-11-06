## install.packages("RODBC")
## install.packages("dplyr")
## install.packages("reshape")
## library(dplyr)
## library(RODBC)
## library(reshape)

## squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")
## library(dplyr)
## Tunnel into ADVSCPRD and pull email dataset
## Is basically a copy of dupe email audit from ADV client, w more variables

tmp_maywood_agency <- sqlQuery(squibbs, "
(
SELECT DISTINCT
  e.id_number, g.gift_donor_id, p.pledge_donor_id, e.first_name, e.last_name, 
  e.record_type_code, al.ALLOCATION_CODE, p.pledge_allocation_name, 
  g.GIFT_ASSOCIATED_ALLOCATION, p.PLEDGE_PLEDGE_NUMBER, g.gift_appeal, 
  ap.appeal_code, p.pledge_appeal, al.SHORT_NAME, al.AGENCY, al.status_code, 
  al.start_date AS alloc_date, g.gift_year_of_giving, g.gift_payment_type, g.gift_associated_credit_amt
FROM
  advance.gift g ,
  advance.allocation al ,
  advance.pledge p ,
  advance.entity e,
  advance.appeals ap
WHERE
  g.GIFT_ASSOCIATED_ALLOCATION = al.ALLOCATION_CODE 
  --AND al.agency = 'MAY'
AND g.gift_year_of_giving > '2013'
  --AND g.gift_payment_type != 'S'
AND
  al.allocation_code = p.pledge_allocation_name
AND 
  ap.id_number = e.id_number 
AND
  g.gift_donor_id = e.id_number
) ")    

rm(tmp_maywood_agency)         