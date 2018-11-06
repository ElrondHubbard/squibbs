
######################
## ADD STOP DATE HERE TOO FROM THE TOP 
## BE SURE TO CUT OUT ANYONE W A STOP DATE BC MEANS I WENT THROUGH N FIXED THEM
## ADD TO CODE N GET IT ALL
## DF tmp_xx is tmp10 with stop date added
######################

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

## Temp DF0 - Updated and better pull than tmp1
## Go through HRZ smashing realized need other data
## This allows comp of address, phone

tmp_dupe_list <- sqlQuery(squibbs, " (
  SELECT 
      a2.id_number AS id_number, e2.pref_mail_name AS entity, a2.email_address,
      a2.email_type_code, a2.email_status_code, a2.preferred_ind, 
      SUBSTR(a2.date_added, 1, 10) AS date_added, SUBSTR(a2.date_modified, 1, 10) 
      AS date_modified, a2.stop_dt, a2.original_source_code, a2.nbr_bouncebacks, 
      a2.xcomment AS xcomment_email, RANK() OVER (PARTITION BY a2.email_address, a2.id_number 
      ORDER BY a2.email_address ASC, a2.id_number ASC, a2.date_added ASC, rownum) AS rnk  
  FROM 
      advance.email a2,
      advance.entity e2, (
    SELECT x.email email
      FROM( SELECT DISTINCT 
        CASE
          WHEN e.spouse_id_number = ' ' THEN e.id_number
          WHEN e.id_number < e.spouse_id_number THEN e.id_number
            ELSE e.spouse_id_number
              END joint_id, a.email_address email
      FROM advance.entity e, advance.email a
        WHERE e.id_number = a.id_number
        AND a.email_type_code = 'E') x
      GROUP BY x.email
        HAVING COUNT(*) > 1) dup_email_set
        WHERE a2.id_number = e2.id_number
        AND e2.record_status_code <> 'X'
        AND a2.email_address = dup_email_set.email) ")

## Make list of emails to exclude, some have so many relatioships
## it really buggers the reshape process, leading to 1000+ variables
## nobody needs to know every child, in-law, and sibling of the HBF president

exclude_list <- 
  c("rjrojas63@aol.com", "kburke@mullinsfood.com", 
    "mschwartz@harrisonheld.com", "sandy@cct.org", "jimandkayc@aol.com", 
    "hbf_office@yahoo.com", "hbf.chicago@gmail.com", "karen.miller.az@gmail.com", 
    "ekoller707@aol.com", "djggrand0817@hotmail.com", "jfaught@luc.edu", 
    "sgallia@iie.org", "kmcguire@iie.org", "ehammons@luc.edu", 
    "jenrich@iie.org", "jgrobart@luc.edu")

## L1 - tmp dupe work - copy of tmp dupe from above, for working n messing with 
## L2 - adds a T/F var to dupe list T = dupe, F = unique
## L3 - adds 'count' var to count dupes, ie rnk var
## L4 - excl_bad - list with dupes, no email from excluded list
## L5 - em_id_only - list with dupes, only id, email, count (for reshape)
## L6 - tmp_HRZ - dupe list reshaped long to wide, IE for each email, dupe IDs associated removed
## L7 - renames columns from 1 to x1, everything w _x is a base unit

## the lines below that end with '2' are other versions, not used, which exclude a lot more
## see dupe_excl_bad2 for list of additional exclusions

tmp_dupe_wk1 <- filter(tmp_dupe_list, RNK==1)
## tmp_dupe_wk1$dupe_email <- duplicated(tmp_dupe_wk1$EMAIL_ADDRESS)
tmp_dupe_wk1 <- within(tmp_dupe_wk1, count <- ave(rep(1,nrow(tmp_dupe_wk1)),tmp_dupe_wk1$EMAIL_ADDRESS,FUN=cumsum))
tmp_dupe_excl_bad <- filter(tmp_dupe_wk1, !EMAIL_ADDRESS %in% exclude_list)
##tmp_dupe_excl_bad2 <- filter(tmp_dupe_wk1, !EMAIL_ADDRESS %in% exclude_list & STOP_DT == 0 & NBR_BOUNCEBACKS == 0 & EMAIL_STATUS_CODE == "A" & PREFERRED_IND == "Y")

tmp_dupe_em_id_only <- subset(tmp_dupe_excl_bad, select = c('EMAIL_ADDRESS', 'ENTITY', 'ID_NUMBER', 'count'))
##tmp_dupe_em_id_only2 <- subset(tmp_dupe_excl_bad2, select = c('EMAIL_ADDRESS', 'ENTITY', 'ID_NUMBER', 'count'))

tmp_HRZ <- reshape(tmp_dupe_em_id_only, direction = "wide", idvar="EMAIL_ADDRESS", timevar="count")
##tmp_HRZ2 <- reshape(tmp_dupe_em_id_only2, direction = "wide", idvar="EMAIL_ADDRESS", timevar="count")

colnames(tmp_HRZ)[1:9] <- c("EMAIL_ADDRESS_x", "ENTITY_x1", "ID_NUMBER_x1", "ENTITY_x2", "ID_NUMBER_x2", "ENTITY_x3", "ID_NUMBER_x3", "ENTITY_x4", "ID_NUMBER_x4")
##colnames(tmp_HRZ2)[1:9] <- c("EMAIL_ADDRESS_x", "ENTITY_x1", "ID_NUMBER_x1", "ENTITY_x2", "ID_NUMBER_x2", "ENTITY_x3", "ID_NUMBER_x3", "ENTITY_x4", "ID_NUMBER_x4")

##########################
##########################
###     TMP    REL     ###
##########################
##########################
##  Pull relation data  ##
# Merge  based on em/ID  #
#   1=1+2 2=2+3 3=3+4    #
##########################
##########################

tmp_rel <- sqlQuery(squibbs, "
    SELECT 
      e.id_number, et.pref_mail_name AS entity, e.email_address, rl.relation_id_number, 
      n.pref_name AS relation_name, rl.xsequence AS xsequence_relship, rl.relation_type_code, rl.date_added AS date_add_relship
    FROM advance.email e
      LEFT JOIN     advance.relationship rl ON (rl.id_number = e.id_number)
      LEFT JOIN     advance.name n ON (n.id_number = rl.relation_id_number)
      LEFT JOIN     advance.entity et ON (et.id_number = e.id_number)
        ")

## Using left join from dplyr, same as SQL
## test1 is email/ID combo 1 to 2, test2 is combo 2 to 3, test3 is combo 3 to 4
## Use 3 iterations to get full set of relationship detail for each ID associated w 1 email addr
tmp_rel$ID_NUMBER <- as.numeric(as.character(tmp_rel$ID_NUMBER))
test1 <- left_join(tmp_HRZ, tmp_rel, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x1" = "ID_NUMBER", "ID_NUMBER_x2" = "RELATION_ID_NUMBER"))
test1$RELATION_ID_NUMBER <- ifelse(is.na(test1$ENTITY), NA, test1$ID_NUMBER_x2)
test1$dupe_email <- duplicated(test1$EMAIL_ADDRESS)
colnames(test1)[10:16] <- c("ENTITY1to2", "R_NAME1to2", "SEQ_RSHIP1to2", "R_TYPE1to2", "R_DATE_ADD1to2", "R_ID_NUMBER1to2", "dupe_email1to2")

test2 <- left_join(test1, tmp_rel, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x2" = "ID_NUMBER", "ID_NUMBER_x3" = "RELATION_ID_NUMBER"), na_matches = "never")
test2$R_ID_NUMBER2to3 <- ifelse(is.na(test2$ENTITY), NA, test2$ID_NUMBER_x3)
test2$dupe_email2to3 <- duplicated(test2$EMAIL_ADDRESS)
colnames(test2)[17:21] <- c("ENTITY2to3", "R_NAME2to3", "SEQ_RSHIP2to3", "R_TYPE2to3", "R_DATE_ADD2to3")

test3 <- left_join(test2, tmp_rel, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x3" = "ID_NUMBER", "ID_NUMBER_x4" = "RELATION_ID_NUMBER"), na_matches = "never")
test3$R_ID_NUMBER3to4 <- ifelse(is.na(test3$ENTITY), NA, test3$ID_NUMBER_x4)
test3$dupe_email3to4 <- duplicated(test3$EMAIL_ADDRESS)
colnames(test3)[24:28] <- c("ENTITY3to4", "R_NAME3to4", "SEQ_RSHIP3to4", "R_TYPE3to4", "R_DATE_ADD3to4")

### ADDR and phone data
### Pull addr and phone data - use as add'l line of comparison b/t >1 ID associated w 1 email addr

tmp_addr_fon <- sqlQuery(squibbs, "
    SELECT 
        e.id_number, et.pref_mail_name AS entity, e.email_address, ad.addr_type_code,  
        ad.addr_status_code, ad.addr_pref_ind, ad.xsequence, ad.line_1 AS addr_line1, 
        ad.line_2 AS addr_line2, t.area_code, t.telephone_number AS phone_number, t.telephone_type_code AS phone_type,
        t.telephone_status_code AS phone_status, t.preferred_ind AS phone_pref_ind,
        RANK() OVER (PARTITION BY e.id_number, e.email_address 
      ORDER BY ad.addr_status_code ASC, ad.addr_pref_ind DESC, rownum) AS rnk
    FROM advance.email e
      LEFT JOIN     advance.address ad ON (ad.id_number = e.id_number)
      LEFT JOIN     advance.telephone t ON (t.id_number = e.id_number)
      LEFT JOIN     advance.entity et ON (et.id_number = e.id_number)
      WHERE e.id_number != ' '
      ")

rm(tmp_addr_fon, tmp_addr_fon2)
## fucking shit won't work b/c convert factor to number = NA
## really fucking hate R sometimes
write.csv() 
as.numeric(levels(tmp_addr_fon$ID_NUMBER))[tmp_addr_fon$ID_NUMBER]
tmp_addr_fon$ID_NUMBER <- as.numeric(as.character(tmp_addr_fon$ID_NUMBER))

tmp_addr_fon_nd <- subset(tmp_addr_fon, RNK==1)
test4 <- left_join(test3, tmp_addr_fon_nd, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x1" = "ID_NUMBER"))
colnames(test4)[31:43] <- c("entity_x1", "addr_type_x1", "addr_status_x1", "addr_pref_x1", "xsequence_x1", "addr_line1_x1", "addr_line2_x1", "area_code_x1", "pho_num_x1", "pho_type_x1", "pho_status_x1", "pho_pref_x1", "rnk_x1")
test5 <- left_join(test4, tmp_addr_fon_nd, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x2" = "ID_NUMBER"))
colnames(test5)[44:56] <- c("entity_x2", "addr_type_x2", "addr_status_x2", "addr_pref_x2", "xsequence_x2", "addr_line1_x2", "addr_line2_x2", "area_code_x2", "pho_num_x2", "pho_type_x2", "pho_status_x2", "pho_pref_x2", "rnk_x2")
test6 <- left_join(test5, tmp_addr_fon_nd, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x3" = "ID_NUMBER"))
colnames(test6)[57:69] <- c("entity_x3", "addr_type_x3", "addr_status_x3", "addr_pref_x3", "xsequence_x3", "addr_line1_x3", "addr_line2_x3", "area_code_x3", "pho_num_x3", "pho_type_x3", "pho_status_x3", "pho_pref_x3", "rnk_x3")
test7 <- left_join(test6, tmp_addr_fon_nd, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x4" = "ID_NUMBER"))
colnames(test7)[70:82] <- c("entity_x4", "addr_type_x4", "addr_status_x4", "addr_pref_x4", "xsequence_x4", "addr_line1_x4", "addr_line2_x4", "area_code_x4", "pho_num_x4", "pho_type_x4", "pho_status_x4", "pho_pref_x4", "rnk_x4")

## Now go back and glue stop dates, status code from email pull to each ID/email combo
## Help ID lowest hanging fruit most easily

tmp_stub <- subset(tmp_dupe_wk1, select = c('EMAIL_ADDRESS', 'ID_NUMBER', 'EMAIL_STATUS_CODE', 'PREFERRED_IND', 'DATE_ADDED', 'STOP_DT', 'NBR_BOUNCEBACKS', 'XCOMMENT_EMAIL'))
test8 <- left_join(test7, tmp_stub, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x1" = "ID_NUMBER"))
colnames(test8)[83:88] <- c("em_status_x1", "pref_x1", "add_dt_x1", "stop_dt_x1", "bounce_x1", "comment_x1")
test9 <- left_join(test8, tmp_stub, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x2" = "ID_NUMBER"))
colnames(test9)[89:94] <- c("em_status_x2", "pref_x2", "add_dt_x2", "stop_dt_x2", "bounce_x2", "comment_x2")
test10 <- left_join(test9, tmp_stub, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x3" = "ID_NUMBER"))
colnames(test10)[95:100] <- c("em_status_x3", "pref_x3", "add_dt_x3", "stop_dt_x3", "bounce_x3", "comment_x3")
test11 <- left_join(test10, tmp_stub, c("EMAIL_ADDRESS_x" = "EMAIL_ADDRESS", "ID_NUMBER_x4" = "ID_NUMBER"))
colnames(test11)[101:106] <- c("em_status_x4", "pref_x4", "add_dt_x4", "stop_dt_x4", "bounce_x4", "comment_x4")

##############################
#########  NEXT  #############
##### start making flags  ####
##############################

## List of relationship codes that probably consititute familial relationship
fam_code_list <- c("AU", "CH", "CU", "DS", "FM", "GC", "GD", "GP", "GR", "IN", "NN", "PA", "PP", "RE", "SB", "SD", "SG", "SM", "SS", "WW", "ZZ")

## YN IDK dummy for relationship types
## Is there a relationship code listed YN
## If code is listed n is fam code, 1 ; if code listed not fam code, 0 ; if no code listed, is NA

## ID 2 compared to ID 1  
test11$rel2to1_yn <- ifelse(is.na(test11$R_TYPE1to2),0,1)
test11$rel2to1_fam <- ifelse((test11$rel2to1_yn == 1) & (test11$R_TYPE1to2 %in% fam_code_list), 1, ifelse((test11$rel2to1_yn == 1) & (!test11$R_TYPE1to2 %in% fam_code_list), 0, NA))
## ID 3 compared to ID 2
test11$rel3to2_yn <- ifelse(is.na(test11$R_TYPE2to3),0,1)
test11$rel3to2_fam <- ifelse((test11$rel3to2_yn == 1) & (test11$R_TYPE2to3 %in% fam_code_list), 1, ifelse((test11$rel3to2_yn == 1) & (!test11$R_TYPE2to3 %in% fam_code_list), 0, NA))
## ID 4 compared to ID 3
test11$rel4to3_yn <- ifelse(is.na(test11$R_TYPE3to4),0,1)
test11$rel4to3_fam <- ifelse((test11$rel4to3_yn == 1) & (test11$R_TYPE3to4 %in% fam_code_list), 1, ifelse((test11$rel4to3_yn == 1) & (!test11$R_TYPE3to4 %in% fam_code_list), 0, NA))

## YN dummys for address matches
## For each pair do addresses match, if Y=1, N=2, either are blank = NA

test11$addr_2to1_match <- ifelse((test11$addr_line1_x1 == test11$addr_line1_x2) & (test11$addr_line2_x1 == test11$addr_line2_x2), 1,0)
test11$addr_2to1_match[is.na(test11$addr_line1_x1) | is.na(test11$addr_line1_x2) | is.na(test11$addr_line2_x1) | is.na(test11$addr_line2_x1)] <- NA
test11$addr_3to2_match <- ifelse((test11$addr_line1_x2 == test11$addr_line1_x3) & (test11$addr_line2_x2 == test11$addr_line2_x3), 1,0)
test11$addr_3to2_match[is.na(test11$addr_line1_x2) | is.na(test11$addr_line1_x3) | is.na(test11$addr_line2_x2) | is.na(test11$addr_line2_x3)] <- NA
test11$addr_4to3_match <- ifelse((test11$addr_line1_x3 == test11$addr_line1_x4) & (test11$addr_line2_x3 == test11$addr_line2_x4), 1,0)
test11$addr_3to4_match[is.na(test11$addr_line1_x3) | is.na(test11$addr_line1_x4) | is.na(test11$addr_line2_x3) | is.na(test11$addr_line2_x4)] <- NA

## Make smaller subset of test11 for use below, col 1-9, 83-116
test12 <- subset(test11, select=c(1:9, 83:116))

## write.csv(test11x, file="email_dupe_batman_rough6.csv", na="", row.names=TRUE)
## write.csv(FY14_pledge_stub, file="N:/AIS/Yerik/Data Audits/damen_society/fy14_pledge_stub.csv", na="", row.names=TRUE)


#################################
#### K NOW WE GO NOW
#################################
## File from write csv above was analyzed by JCole
## Go through n add xcomment from OG tmp dupes file to check for Blackbaud

jc_yk_email <- read.csv('N:/AIS/Yerik/Email Cleanup/email_dupe_batman_rough3_JC_YK.csv')
jc_yk_email_small <- subset(jc_yk_email, select=c(1:3, 42:50))

##tmp_comment_merge <- subset(tmp_dupe_list, select = c("ID_NUMBER", "EMAIL_ADDRESS", "XCOMMENT_EMAIL"))

jc_yk_email_tmp1 <- left_join(jc_yk_email_small, tmp_dupe_list, c("Email.Address" = "EMAIL_ADDRESS", "ID_ENT1" = "ID_NUMBER"))
colnames(jc_yk_email_tmp1)[13] <- c("ent1_comment")
jc_yk_email_tmp2 <- left_join(jc_yk_email_tmp1, tmp_dupe_list, c("Email.Address" = "EMAIL_ADDRESS", "ID_ENT2" = "ID_NUMBER"))
colnames(jc_yk_email_tmp2)[14] <- c("ent2_comment")

jc_yk_email_test1 <- left_join(jc_yk_email_small, test12, c("Email.Address" = "EMAIL_ADDRESS_x", "ID_ENT1" = "ID_NUMBER_x1"))
colnames(jc_yk_email_tmp1)[13] <- c("ent1_comment")
jc_yk_email_test2 <- left_join(jc_yk_email_test1, test12, c("Email.Address" = "EMAIL_ADDRESS", "ID_ENT2" = "ID_NUMBER"))
colnames(jc_yk_email_tmp2)[14] <- c("ent2_comment")









write.csv(jc_yk_email_tmp2, file="N:/AIS/Yerik/Email Cleanup/email_dupe_batman_rough3_JC_YK_comments.csv", na="", row.names=TRUE)

write.csv(tmp_addr_fon, file="test.csv", na="", row.names = TRUE)
