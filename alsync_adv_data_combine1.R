# Check results from AL Sync against what we have in DB and got back from Blackbaud

alsync <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/vendor_test_files/FinalResults_AlumniSync_TEST_loyola/FINAL_loyola_locator.csv")
adv_data <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/vendor_test_files/vendor_test_file_all_obs_all_var.csv")
adv_data2 <- subset(adv_data, adv_data$Group==6, select = c(1:11))
address_181005_stub <- subset(address_181005, select=c(1,6,13))
adv_data2 <- left_join(adv_data2, address_181005_stub, c("ID_NUMBER" = "ID_NUMBER", "ADV_STREET" = "STREET1"))
adv_data2$ADV_DATE <- adv_data2$ADDR_START_DT
adv_data2$dupes <- duplicated(adv_data2$ID_NUMBER2)
adv_data2 <- subset(adv_data2, adv_data2$dupes == FALSE, select=c(1:11))
adv_data <- subset(adv_data, adv_data$ID_NUMBER2<201)
adv_data <-  bind_rows(adv_data, adv_data2)
rm(adv_data2)

alsync2 <- subset(alsync, select=c(2,10:34))

adv_al_comp <- left_join(adv_data, alsync2, c("ID_NUMBER2"="ID_NUMBER"))


#write.csv(adv_al_comp, file="N:/AIS/Yerik/Data Audits/Address_et_al/adv_alsync_comp.csv", na="", row.names=TRUE)
