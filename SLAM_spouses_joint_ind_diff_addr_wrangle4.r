

#########################
# Entire script below is part of build_ncoa_mover_comp_file_2 
# As of 16 October 2018 #
#########################



# MH wants to just slam in data that is known to be bad b/c metrics duhhhh
# Use same datasets as wrangle.x script, with add'l date data
# Try find a single actual method of looking at dates b/t spouses to see if wtf get it right.
# IE say "these data are trash, but I know that we can use date_varX to say which is 'more current' to us

diff_exp <- read.csv("N:/AIS/Yerik/Data Audits/spouse_joint_ind_diff_addr/spouses_with_same_home_check_mismatched.csv")
diff_exp$spouse_link <- 1:nrow(diff_exp)
colnames(diff_exp)[1:12] <- c(
      "id1", "id1_street", "id1_addr_type" ,"id1_operator" ,"id1_comment" ,"id1_mod_date" ,
      "id2", "id2_addr_type", "id2_street" ,"id2_operator" ,"id2_comment" ,"id2_mod_date" )
diff_exp$id1_street <- as.character(diff_exp$id1_street)
diff_exp$id2_street <- as.character(diff_exp$id2_street)
diff_exp$id1_street <- gsub('.','',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Avenue','Ave',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Court','Ct',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Drive','Dr',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub(',',' ',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('#','',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub(' Road','Rd',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('st','St',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- gsub('Street','St',diff_exp$id1_street, fixed = TRUE)
diff_exp$id1_street <- tolower(diff_exp$id1_street)
diff_exp$id2_street <- gsub('.','',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Avenue','Ave',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Court','Ct',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Drive','Dr',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub(',',' ',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('#','',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub(' Road','Rd',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('st','St',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- gsub('Street','St',diff_exp$id2_street, fixed = TRUE)
diff_exp$id2_street <- tolower(diff_exp$id2_street)
diff_exp <- subset(diff_exp, select=c(13,1:12))

# Fix awful add/mod dates
diff_exp$id1_mod_date2 <- as.character(substring(diff_exp$id1_mod_date,1,9))
diff_exp$id1_mod_date2 <- as.Date(diff_exp$id1_mod_date, format = "%d-%B-%y")
diff_exp$id2_mod_date2 <- as.character(substring(diff_exp$id2_mod_date,1,9))
diff_exp$id2_mod_date2 <- as.Date(diff_exp$id2_mod_date, format = "%d-%B-%y")

months <- data.frame(month=c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), num=sprintf('%0.2d',1:12))
diff_exp$id1_mod_date <- paste(substring(diff_exp$id1_mod_date,1,4), substring(diff_exp$id1_mod_date,6,7), substring(diff_exp$id1_mod_date,9,10),sep='')
diff_exp$id2_mod_date <- paste(substring(diff_exp$id2_mod_date,1,4), substring(diff_exp$id2_mod_date,6,7), substring(diff_exp$id2_mod_date,9,10),sep='')
diff_exp$id1_mod_date3 <- paste(substring(diff_exp$id1_mod_date2,1,4), substring(diff_exp$id1_mod_date2,6,7), substring(diff_exp$id1_mod_date2,9,10),sep='')
diff_exp$id2_mod_date3 <- paste(substring(diff_exp$id2_mod_date2,1,4), substring(diff_exp$id2_mod_date2,6,7), substring(diff_exp$id2_mod_date2,9,10),sep='')
diff_exp$id1_mod_date3 <- as.integer(diff_exp$id1_mod_date3)
diff_exp$id2_mod_date3 <- as.integer(diff_exp$id2_mod_date3)
diff_exp$id1_mod_date <- diff_exp$id1_mod_date3
diff_exp$id2_mod_date <- diff_exp$id2_mod_date3
diff_exp <- subset(diff_exp, select =(1:13))

