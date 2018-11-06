# Need to code in NA for all missing data in vendor addr 
# Make smaller DF with only street stuff.
comp2 <- subset(comp, select=c(1:5,7,11:12,16:18,24,32:33))
comp2$addr_af[lengths(comp2$addr_af)==0] <- NA_character_

comp2$af_len <- nchar(comp2$addr_af, type="chars", allowNA=FALSE, keepNA=FALSE)
comp2$as_len <- nchar(comp2$start_dt_as, type="chars", allowNA=FALSE, keepNA=FALSE)
comp2$addr_af <- ifelse(comp2$af_len==0, is.na(comp2$addr_af), comp2$addr_af)
comp2$addr_af[comp2$af_len==0] <- NA

# ADV to AF
comp2$addr_af <- ifelse((comp2$addr_af == ''), NA, comp2$addr_af)
comp2$adv_af_same <- mapply(vintersect, strsplit(comp2$ADV_STREET, split=" "), strsplit(comp2$addr_af, split=" "), SIMPLIFY = TRUE)
comp2$af_adv_same <- mapply(vintersect, strsplit(comp2$addr_af, split=" "), strsplit(comp2$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp2$adv_af_same <- lapply(comp2$adv_af_same, paste, collapse = ' ')
comp2$af_adv_same <- lapply(comp2$af_adv_same, paste, collapse = ' ')
comp2$adv_af_same <- as.character(comp2$adv_af_same)
comp2$af_adv_same <- as.character(comp2$af_adv_same)

comp2$adv_af_diff <- mapply(vsetdiff, strsplit(comp2$ADV_STREET, split=" "), strsplit(comp2$addr_af, split=" "), SIMPLIFY = TRUE)
comp2$af_adv_diff <- mapply(vsetdiff, strsplit(comp2$addr_af, split=" "), strsplit(comp2$ADV_STREET, split=" "), SIMPLIFY = TRUE)
comp2$adv_af_diff <- lapply(comp2$adv_af_diff, paste, collapse = ' ')
comp2$af_adv_diff <- lapply(comp2$af_adv_diff, paste, collapse = ' ')
comp2$adv_af_diff <- as.character(comp2$adv_af_diff)
comp2$af_adv_diff <- as.character(comp2$af_adv_diff)