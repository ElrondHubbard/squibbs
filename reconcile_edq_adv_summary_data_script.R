# This can only be run after the advance_ncoa_blackbaud_reconcile 
# script has been run up to the GROUP 2 section, ~~ line 230

# Below creates a copy file and runs a bunch of summary 
# counts etc so the CSBs get all they need for detail

# Check data is merged4 but only those 39K w EDQ flag
check <- subset(merged4, merged4$edq_flag == 1 )


check %>%
  group_by(edq_flag) %>%
  summarise(number=n())

# Not updating following

# Dead in BB per their dead flag - 
check %>%
  group_by(flag_dead) %>%
  summarise(number=n())

# Lost in adv but has addr in EDQ 
check %>%
  group_by(lost_addr) %>%
  summarise(number=n())

# Dead in ADV 
check %>%
  group_by(big_dead_flag == 1 & flag_dead ==0) %>%
  summarise(number=n())

# Make var for people w NO adv address period for any reason
check$no_adv_addr[check$has_adv_addr==0] <- "NO PREF ADV ADDR"


##########################################
##########################################
##########################################
# LEFT OFF HERE SEE WORD DOC DID BY HAND #
##########################################
##########################################
##########################################


# Clean up match addr type field - if no match



# Create one master column for data in advance - if any TF addr matches 
# BB return, use it, otherwise use whatever their pref addr is from Adv
check$adv_edq_match_addr <- 
  ifelse(check$adv_home_edq != '',check$adv_home_edq, 
    ifelse(check$adv_biz_edq != '', check$adv_biz_edq,
      ifelse(check$adv_home2_edq != '', check$adv_home2_edq, 
        ifelse(check$adv_biz2_edq != '', check$adv_biz2_edq,
          ifelse(check$adv_ssnl_edq != '', check$adv_ssnl_edq, 
            ifelse(check$any_adv_edq_match == 0,check$pref_addr_default,
              "NOPE"))))))

# Addr type for MA column - so it matches, is match pref, biz2, seasonal?
check$adv_edq_match_type <- 
  ifelse(check$adv_home_edq != '',check$adv_home_edq, 
    ifelse(check$adv_biz_edq != '', check$adv_biz_edq,
      ifelse(check$adv_home2_edq != '', check$adv_home2_edq, 
        ifelse(check$adv_biz2_edq != '', check$adv_biz2_edq,
          ifelse(check$adv_ssnl_edq != '', check$adv_ssnl_edq, 
            ifelse(check$any_adv_edq_match == 0,check$pref_addr_default,
              "NOPE"))))))


# Copy BB addr return column so can visually compare easy if needed
check$edq_addr_return <- check$Addrlines_A

# Calculate Lev Distance b/t MA adv column and BB return column
check$lev_distance <- mapply(adist, check$adv_edq_match_addr, check$edq_addr_return)

# Categorical var for distance - full match, low distance, probable match, not probable
# Distance for each 1 = 0, 2 = less than 5, 3 = 5 to 10, 4 = < 10
# 1 are 100% match
# 2 are mostly addr corrections very minor adds - capitalization or street direction
# 3 are mostly apt appends - adding " Apt 101" or " Unit 2394" - latter is 10
# 4 are mostly different addresses compeltely - check dates on these if ours post dates BB, keep, else maybe use
check$lev_dist_grp <- 
  ifelse(check$adv_edq_match_addr == check$edq_addr_return, "full match" ,
    ifelse(check$lev_distance <= 4, "< 5" , 
      ifelse(check$lev_distance <= 10, "5 to 10" , 
        "> 10")))

# Theory - for lev dist grp 2 & 3, probably same address w something added
# within string matching - charmatch function compares two addresses n asks
# Is address 1 (adv_edq_match_addr) within address 2?
# AKA - is address 2 just address 1 w s.thing added to end (apt #)
# Also copied BB field for description so if BB just appended apt # will state such
check$adv_addr_partial_edq <- mapply(charmatch, check$adv_edq_match_addr, check$edq_addr_return)
check$adv_addr_partial_edq[is.na(check$adv_addr_partial_edq)] <- 0
check$addr_edit_type <- check$cattrdesc_A
sum(check$adv_addr_partial_edq)






check <- subset(merged_data7, RECORD_TYPE_CODE == 'AL')

# The "EDQ ADDRESS IS OK TO USE FOR UPDATE" flag
# Starts as a bunch of individual flags == 1
# Then all are combined into a flag that == 1 if OK, == 0 if not OK
# Record is dead per Advance
# ADV and EDQ addresses are perfect match
# Weird N=12 group - BB says Apt Append but regular address is Inactive
# Any addr w Adv add date after 03/01/2018 - is most recent date for all BB data aka anything in Adv later is newer than their newest
#check$edq_addr_OK <- 1


check$no_addr_update_dead [(check$adv_dead_flag == 1)] <- 1 
check$no_addr_update_match [(check$lev_dist_grp == "full match")] <- 1 
check$no_update_weird12 [((check$lev_dist_grp == "< 5" | check$lev_dist_grp == "5 to 10") & check$ADV_ADDR_STATUS == "I" & check$adv_addr_partial_edq == 1)] <- 1
check$no_update_adv_date_gt_1march [(check$adv_add_date2 > 20180301)] <- 1



edq_file_exp %>%
  group_by(adv_curr_addr_type) %>%
  summarise(number=n())

edq_file_exp3 <- edq_file_exp
edq_file_exp3 <- left_join(edq_file_exp3, edq_file_gr23_results, c("ID_NUMBER" = "ID_NUMBER"))
edq_file_exp3 <- left_join(edq_file_exp3, big_address_list, c("ID_NUMBER" = "ID_NUMBER", "adv_current_addr" = "ADV_STREET"))
edq_file_exp2$dupes <- duplicated(edq_file_exp2$ID_NUMBER)
edq_file_exp2 <- subset(edq_file_exp2, dupes == FALSE)

