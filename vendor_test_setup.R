
# Open DF with ID, names, addresses of test cases
# Cases are 400 total, subset of EDQ file, 200 various, 200 lost
vendor_test <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/vendor_test_files/vendor_test_file_addr_update_180928.csv")
vendor_test <- subset(vendor_test, select=c(1:5,10))

address_181003 <- sqlQuery(squibbs, "( SELECT DISTINCT 
  a.ID_NUMBER ,a.XSEQUENCE ,a.ADDR_TYPE_CODE ,a.ADDR_STATUS_CODE ,a.ADDR_PREF_IND
  ,a.STREET1 ,a.STREET2 ,a.STREET3 ,a.FOREIGN_CITYZIP ,a.CITY ,a.STATE_CODE ,a.ZIPCODE
  ,a.START_DT as ADDR_START_DT ,a.STOP_DT AS ADDR_STOP_DT ,a.XCOMMENT AS XCOMMENT_ADDR 
  ,a.DATE_ADDED AS ADDR_ADD_DT ,a.DATE_MODIFIED AS ADDR_MOD_DT ,a.OPERATOR_NAME AS ADDR_OPERATOR
  ,a.LINE_1 ,a.LINE_2 ,a.LINE_3 ,a.LINE_4 ,a.LINE_5 ,a.LINE_6 ,a.LINE_7 ,a.LINE_8 
  ,RANK() OVER (PARTITION BY a.id_number ORDER BY a.date_added DESC, rownum) AS rnk 
  FROM advance.address a  
  ORDER BY a.ID_NUMBER, a.date_added DESC)")

phone_data <- sqlQuery(squibbs, "(
SELECT *
  FROM advance.telephone)")

address_data_sub <- subset(address_181003, select=c(1,3:4,6:8,10:12,27))
vendor_test2 <- left_join(vendor_test, address_data_sub, c("ID_NUMBER" = "ID_NUMBER"))
vendor_test2$STREET1 <- as.character(vendor_test2$STREET1)
vendor_test2$addr_type_num <- ifelse((vendor_test2$ADDR_TYPE_CODE=='H' & vendor_test2$ADDR_STATUS_CODE=='A'),0,
  ifelse((vendor_test2$ADDR_TYPE_CODE=='B' & vendor_test2$ADDR_STATUS_CODE=='A'),1,2))
vendor_test2 <- vendor_test2[order(vendor_test2$ID_NUMBER, vendor_test2$ADDR_STATUS_CODE, vendor_test2$addr_type_num),]
vendor_test2 <- vendor_test2[,c(2,9,1,3:8,10:16)]
vendor_test2$dupes <- duplicated(vendor_test2[,(1:2)])
vendor_test2 <- subset(vendor_test2, vendor_test2$dupes==FALSE)
vendor_test3 <- vendor_test2[,c(1,3:9,2,10:16)]
vendor_test3 <- within(vendor_test3, count <- ave(rep(1,nrow(vendor_test3)),vendor_test3$ID_NUMBER,FUN=cumsum))
vendor_test3 <- subset(vendor_test3, vendor_test3$count < 5)
vendor_test3$addr_status <- ifelse((vendor_test3$addr_type_num==0 | vendor_test3$addr_type_num==1),'active','past')
num_list <- c("1","2","3","4","5","6","7","8","9","0")
vendor_test3$STREET1 <- gsub('.','',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub(' Avenue',' Ave',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub(' Court',' Ct',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub(' Drive',' Dr',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub(',',' ',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub(' Road',' Rd',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub(' st',' St',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub(' Street',' St',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET1 <- gsub('#',' ',vendor_test3$STREET1, fixed = TRUE)
vendor_test3$STREET2 <- gsub('.','',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub(' Avenue',' Ave',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub(' Court',' Ct',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub(' Drive',' Dr',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub(',',' ',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub(' Road',' Rd',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub(' st',' St',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub(' Street',' St',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET2 <- gsub('#',' ',vendor_test3$STREET2, fixed = TRUE)
vendor_test3$STREET3 <- gsub('.','',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub(' Avenue',' Ave',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub(' Court',' Ct',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub(' Drive',' Dr',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub(',',' ',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub(' Road',' Rd',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub(' st',' St',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub(' Street',' St',vendor_test3$STREET3, fixed = TRUE)
vendor_test3$STREET3 <- gsub('#',' ',vendor_test3$STREET3, fixed = TRUE)

#vendor_test3$STREET1 <- tolower(vendor_test3$STREET1)
# Flag to ID any STREET1 value that is not numeric or PO Box ie when Apt X is first
vendor_test3$order_flag <- ifelse((substring(vendor_test3$STREET1,1,1) %in% num_list) | (substring(vendor_test3$STREET1,1,2) == 'PO'),0,1)
vendor_test3$order_flag2 <- ifelse((substring(vendor_test3$STREET1,1,2) != 'PO'),0,1)
vendor_test3$STREET_1 <- as.character(NA)
vendor_test3$STREET_2 <- as.character(NA)
vendor_test3$STREET_1 <- ifelse(vendor_test3$order_flag==0,vendor_test3$STREET1,vendor_test3$STREET2)
vendor_test3$STREET_2 <- ifelse(vendor_test3$order_flag==0,vendor_test3$STREET2,vendor_test3$STREET1)
vendor_test3$STREET1 <- vendor_test3$STREET_1
vendor_test3$STREET2 <- vendor_test3$STREET_2
vendor_test3_hrz_ready <- subset(vendor_test3, select=c(2,9:14,17:18))
vendor_test_hrz <- reshape(vendor_test3_hrz_ready, direction = "wide", idvar = "ID_NUMBER2", timevar="count")
rm(vendor_test3_hrz_ready)

other_var <- subset(vendor_test2, select=c(1,3:7))
other_var$dupes <- duplicated(other_var$ID_NUMBER2)
other_var <- subset(other_var, other_var$dupes==FALSE)
vendor_test_hrz2 <- left_join(vendor_test_hrz, other_var, c("ID_NUMBER2"="ID_NUMBER2"))
vendor_test_final <- vendor_test_hrz2[,c(1,30,34,31:33,2:29)]

# Add phone data
# Active cell and land phones, order by add date
phone_data_cell <-subset(phone_data, phone_data$TELEPHONE_STATUS_CODE=='A' & phone_data$TELEPHONE_TYPE_CODE=='C')
phone_data_land <-subset(phone_data, phone_data$TELEPHONE_STATUS_CODE=='A' & phone_data$TELEPHONE_TYPE_CODE=='H')
phone_data_cell <- phone_data_cell[order(phone_data_cell$ID_NUMBER, phone_data_cell$DATE_ADDED),]
phone_data_land <- phone_data_land[order(phone_data_land$ID_NUMBER, phone_data_land$DATE_ADDED),]
# ID and remove dupes in each
phone_data_cell$dupes <- duplicated(phone_data_cell$ID_NUMBER)
phone_data_land$dupes <- duplicated(phone_data_land$ID_NUMBER)
phone_data_land <- subset(phone_data_land, phone_data_land$dupes==FALSE, select=c(1,6:7))
colnames(phone_data_land)[2:3] <- c("land_area_code", "land_phone")
phone_data_cell <- subset(phone_data_cell, phone_data_cell$dupes==FALSE, select=c(1,6:7))
colnames(phone_data_cell)[2:3] <- c("cell_area_code", "cell_phone")
vendor_test_final <-left_join(vendor_test_final, phone_data_land, c("ID_NUMBER"="ID_NUMBER"))
vendor_test_final <-left_join(vendor_test_final, phone_data_cell, c("ID_NUMBER"="ID_NUMBER"))


rm(num_list, vendor_test, vendor_test2, vendor_test3, vendor_test_hrz, vendor_test_hrz2, other_var, address_data_sub, phone_data2, phone_data_land, phone_data_cell)
write.csv(vendor_test_final, file="N:/AIS/Yerik/Data Audits/Address_et_al/vendor_test_files/vendor_test_file_final_181003.csv", na="", row.names=TRUE)




