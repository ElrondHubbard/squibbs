
# EG string dataframe with names

eg_data <- data.frame(name1 = c('Jimmy Conway', 'Jimmy'), name2 = c('Conway','Jimmy Conway'))
eg_data$share_name1 <- mapply(charmatch, eg_data$name1, eg_data$name2)
eg_data$share_name2 <- mapply(charmatch, eg_data$name2, eg_data$name1)
eg_data$share_name <- 0
eg_data$share_name [(eg_data$share_name1==1 | eg_data$share_name2==1)] <- 1
