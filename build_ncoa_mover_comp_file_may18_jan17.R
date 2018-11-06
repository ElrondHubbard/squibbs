# Script below takes last 5 NCOA updates
# May 2018, Jan 2018, Sept 2017, May 2017, Jan 2017
# Finds all those who moved, pulls dates of move, type code, etc
# So can compare against funny dates for new movers etc

# Look through ALL NCOA data and find all move date codes.
# Pull them out n get them in a list.

# May 2018
ncoa_may18 <- read.csv("N:/AIS/NCOA/Loyola_Addr_Cleaning_2018_05_11/Loyola Addr Cleaning 2018-05-11_Import.csv")
ncoa_may18$ncoa_dt <- "may18"
ncoa_may18$ncoa_addr_na <- ifelse(ncoa_may18$NCO_ADDR=="",1,0)
ncoa_may18_movers <- subset(ncoa_may18, ncoa_may18$NCO_MOVEDT > 0, select=c(1,158,170:172,175,132,68,67,69,183:184))
colnames(ncoa_may18_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")
 
# January 2018
ncoa_jan18 <- read.csv("N:/AIS/NCOA/Loyola Addr Cleaning 2018-01-09_Appended.csv")
ncoa_jan18$ncoa_dt <- "jan18"
ncoa_jan18$ncoa_addr_na <- ifelse(ncoa_jan18$NCO_ADDR=="",1,0)
ncoa_jan18_movers <- subset(ncoa_jan18, ncoa_jan18$NCO_MOVEDT > 0, select=c(1,158,170:172,175,132,68,67,69,183:184))
colnames(ncoa_jan18_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# Sept 2017
ncoa_sept17 <- read.csv("N:/AIS/NCOA/Loyola_Addr_cleaning_2017_9_22_Appended/Loyola_Addr_Cleaning_2017_09_22_Appended.csv")
ncoa_sept17$ncoa_dt <- "sept17"
ncoa_sept17$ncoa_addr_na <- ifelse(ncoa_sept17$Address_NCOA=="",1,0)
ncoa_sept17_movers <- subset(ncoa_sept17, ncoa_sept17$MoveDate_NCOA > 0, select=c(1,24:34))
colnames(ncoa_sept17_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# May 2017
ncoa_may17 <- read.csv("N:/AIS/NCOA/Loyola_Addr_cleaning_2017_05_31_Appended/Loyola_Addr_Cleaning_2017_05_31_Appended.csv")
ncoa_may17$ncoa_dt <- "may17"
ncoa_may17$ncoa_addr_na <- ifelse(ncoa_may17$Address_NCOA=="",1,0)
ncoa_may17_movers <- subset(ncoa_may17, ncoa_may17$MoveDate_NCOA > 0, select=c(1,24:34))
colnames(ncoa_may17_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# Jan 2017
ncoa_jan17 <- read.csv("N:/AIS/NCOA/Loyola_Addr_cleaning_2017_01_23_Appended/Loyola_Addr_Cleaning_2017_01_23_Appended.csv")
ncoa_jan17$ncoa_dt <- "jan17"
ncoa_jan17$ncoa_addr_na <- ifelse(ncoa_jan17$Address_NCOA=="",1,0)
ncoa_jan17_movers <- subset(ncoa_jan17, ncoa_jan17$MoveDate_NCOA > 0, select=c(1,24:34))
colnames(ncoa_jan17_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")

# Feb 2015 - I think, the "one AIS did in 2015"
ncoa_feb15 <- read.csv("N:/AIS/Address_Cleaning/Addr_list_returned_from_NCOA_2015_2_23.csv")
ncoa_feb15$ncoa_dt <- "feb15"
ncoa_feb15$ncoa_addr_na <- ifelse(ncoa_feb15$MoveDate_NCOA=="",1,0)
ncoa_feb15$ID <- substring(ncoa_feb15$ID,1,10)
ncoa_feb15$ID <- as.integer(ncoa_feb15$ID)
ncoa_feb15_movers <- subset(ncoa_feb15, ncoa_feb15$MoveDate_NCOA > 0, select=c(2,25:35))
colnames(ncoa_feb15_movers)[2:10] <- c("ADDR", "CITY", "STATE" ,"ZIP" ,"CARRY_RT" ,"DPV_CODE" , "MOVE_TYPE" , "MOVE_DT" , "NIXIE")
ncoa_may18_movers$ZIP <- as.character(ncoa_may18_movers$ZIP) 
ncoa_jan18_movers$ZIP <- as.character(ncoa_jan18_movers$ZIP) 
ncoa_sept17_movers$ZIP <- as.character(ncoa_sept17_movers$ZIP) 
ncoa_may17_movers$ZIP <- as.character(ncoa_may17_movers$ZIP) 
ncoa_jan17_movers$ZIP <- as.character(ncoa_jan17_movers$ZIP) 
ncoa_feb15_movers$ZIP <- as.character(ncoa_feb15_movers$ZIP) 

# Merge all separate NCOA mover data together
ncoa_data_jan17_may18 <-  bind_rows(ncoa_may18_movers, ncoa_jan18_movers, ncoa_sept17_movers, ncoa_may17_movers, ncoa_jan17_movers, ncoa_feb15)
# Sort so order is ID asc , addr desc, nixie desc - addr doesn't matter but desc nixie means all A code will be on top - aka if address is 100% by NCOA, it will be on top 
ncoa_data_jan17_may18 <- ncoa_data_jan17_may18[order(-(ncoa_data_jan17_may18$ID), ncoa_data_jan17_may18$ADDR, ncoa_data_jan17_may18$NIXIE, decreasing=TRUE),]

# Check for n remove dupes such that any with A code are kept
# Subset keep only NIXIE w match A, drop secondary, or only missing zip + 4
# Resort so that most recent date for move types above is kept
# Merge dupe dates back in so have as recent as possible date for all address change
ncoa_data_jan17_may18$dupes <- duplicated(ncoa_data_jan17_may18[,1:2])
nixie_list <- c("91", "92", "A", "19")
ncoa_data_j17_m18_dupes <- subset(ncoa_data_jan17_may18, (ncoa_data_jan17_may18$dupes==TRUE & ncoa_data_jan17_may18$NIXIE %in% nixie_list), select=c(1,9))
ncoa_data_j17_m18_dupes <- ncoa_data_j17_m18_dupes[order(-(ncoa_data_j17_m18_dupes$ID), ncoa_data_j17_m18_dupes$MOVE_DT, decreasing=TRUE),]
ncoa_data_j17_m18_dupes$dupes <- duplicated(ncoa_data_j17_m18_dupes$ID)
ncoa_data_j17_m18_dupes <- subset(ncoa_data_j17_m18_dupes, ncoa_data_j17_m18_dupes$dupes==FALSE, select=c(1:2))
ncoa_data_jan17_may18 <-subset(ncoa_data_jan17_may18, ncoa_data_jan17_may18$dupes ==FALSE, select=c(1:12))
ncoa_data_jan17_may18 <- left_join(ncoa_data_jan17_may18, ncoa_data_j17_m18_dupes, c("ID"="ID"))
colnames(ncoa_data_jan17_may18)[9:13] <- c("MOVE_DT1", "NIXIE1", "ncoa_addr_na1" ,"ncoa_group" ,"MOVE_DT2")
ncoa_data_jan17_may18$MOVE_DT2 <- ifelse(is.na(ncoa_data_jan17_may18$MOVE_DT2),0,ncoa_data_jan17_may18$MOVE_DT1)
ncoa_data_jan17_may18$most_recent_dt <- ifelse(ncoa_data_jan17_may18$MOVE_DT1 > ncoa_data_jan17_may18$MOVE_DT2, ncoa_data_jan17_may18$MOVE_DT1,ncoa_data_jan17_may18$MOVE_DT2)

#rm(ncoa_data_j17_m18_dupes, ncoa_jan17, ncoa_jan17_movers, ncoa_jan18, ncoa_jan18_movers, ncoa_may17, ncoa_may17_movers, ncoa_may18, ncoa_may18_movers, ncoa_sept17, ncoa_sept17_movers)

# Check dupes just by ID , dupes == 5660 / 48010 or ~ 10%
ncoa_data_jan17_may18$dupes <- duplicated(ncoa_data_jan17_may18$ID)
ncoa_data_jan17_may18 <- within(ncoa_data_jan17_may18, count <- ave(rep(1,nrow(ncoa_data_jan17_may18)),ncoa_data_jan17_may18$ID,FUN=cumsum))
# One instance of 5 dupes, 22 of 4, 416 of 3, 5221 of 2, 42350 of only one
# reshape
ncoa_data_jan17_may18_HRZ <- subset(ncoa_data_jan17_may18, select=c(1,2,10,12,14,16))
ncoa_data_jan17_may18_HRZ$ADDR <- gsub('.','',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub('Avenue','Ave',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub('Court','Ct',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub('Drive','Dr',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub(',',' ',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub('#','',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub(' Road','Rd',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub('st','St',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- gsub('Street','St',ncoa_data_jan17_may18_HRZ$ADDR, fixed = TRUE)
ncoa_data_jan17_may18_HRZ$ADDR <- tolower(ncoa_data_jan17_may18_HRZ$ADDR)
ncoa_data_jan17_may18_HRZ <- reshape(ncoa_data_jan17_may18_HRZ, direction = "wide", idvar = "ID", timevar="count")

# Match past NCOA against spouse data, diff_exp
spouse_diff_ncoa <- left_join(diff_exp, ncoa_data_jan17_may18_HRZ, c("id1" = "ID"))



spouse_diff_ncoa %>%
  group_by(NIXIE1.1) %>%
  summarise(number=n())

