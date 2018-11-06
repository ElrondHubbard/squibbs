# Need to bring in add'l addr info and compare, realized that some addr were incorrect b/c 
# one version to one vendor other to next so addr might be updated w BB results, this extra
# bit will guarantee that addr are correc for comp.
test_addr2 <- read.csv("N:/AIS/Yerik/Data Audits/Address_et_al/vendor_test_files/vendor_test_file_final_180928.csv", header=TRUE, na.strings="")
test_addr2 <- subset(test_addr2, select = c(3,8:13))
test_addr <- left_join(test_addr, test_addr2, c("ID_NUMBER"="ID_NUMBER"))
test_addr[] <- lapply(test_addr, as.character)

test_addr$str_ch <- ifelse(test_addr$ADV_STREET == test_addr$STREET1.1,1,0)
test_addr$city_ch <- ifelse(test_addr$ADV_CITY == test_addr$CITY.1, 1,0)
test_addr$stat_ch <- ifelse(test_addr$ADV_STATE == test_addr$STATE_CODE.1, 1,0)
test_addr$stat_ch <- ifelse(test_addr$ADV_STATE == test_addr$STATE_CODE.1, 1,0)
test_addr3 <- subset(test_addr, select=c(3:5,7,12,18,24:26))